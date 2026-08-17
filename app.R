library(tidyverse)
library(shiny)
library(rsconnect)
library(shinyjs)
library(googlesheets4)

#rsconnect::deployApp("/Users/redfi/Documents/Personal R/Picks ATS")

#Give Google authentication access to R
#gs4_auth(scopes = "https://www.googleapis.com/auth/spreadsheets")

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1HNe0QmeT8SnXNh0zrh2Z5QoSYG_-VBs56PU_BEkev00/edit?gid=1781931991#gid=1781931991"


ui <- fluidPage(
  useShinyjs(),
  titlePanel("ATS Picks App"),
  
  sidebarLayout(
    sidebarPanel(
      # --- Login panel (shown first) ---
      div(
        id = "login_panel",
        h4("Enter your name to access your picks"),
        textInput("login_name", "Your name"),
        actionButton("login", "Log In"),
        br(), br(),
        textOutput("login_error")
      ),
      
      # --- Picks panel (hidden until authenticated) ---
      shinyjs::hidden(
        div(
          id = "picks_panel",
          uiOutput("who_ui"),
          uiOutput("week_ui"),
          actionButton("submit", "Submit Picks"),
          br(), br(),
          actionButton("logout", "Log Out")
        )
      )
    ),
    
    mainPanel(
      uiOutput("game_inputs"),
      br(),
      textOutput("lock_status"),
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {
  
  # Tracks who is currently authenticated (NULL = not logged in)
  auth_user <- reactiveVal(NULL)
  
  # Load list of valid users from sheet
  users_data <- reactive({
    read_sheet(SHEET_URL, sheet = "Users") %>%
      mutate(user = as.character(user))
  })
  
  # Load games from sheet
  games_data <- reactive({
    read_sheet(SHEET_URL, sheet = "Games")
  })
  
  # Load weeks/locked status
  weeks_data <- reactive({
    read_sheet(SHEET_URL, sheet = "Weeks") %>%
      mutate(week   = as.character(week),
             locked = as.logical(locked))
  })
  
  # --- Login handling ---
  observeEvent(input$login, {
    req(input$login_name)
    
    entered_name <- trimws(input$login_name)
    
    users <- users_data()
    
    match_row <- users %>%
      filter(tolower(trimws(user)) == tolower(entered_name))
    
    if (nrow(match_row) == 1) {
      auth_user(match_row$user[1])   # use canonical name/casing from sheet
      output$login_error <- renderText("")
      shinyjs::hide("login_panel")
      shinyjs::show("picks_panel")
    } else {
      output$login_error <- renderText("Name not recognized. Check spelling and try again.")
      updateTextInput(session, "login_name", value = "")
    }
  })
  
  # --- Logout handling ---
  observeEvent(input$logout, {
    auth_user(NULL)
    updateTextInput(session, "login_name", value = "")
    shinyjs::show("login_panel")
    shinyjs::hide("picks_panel")
    output$status <- renderText("")
  })
  
  # Display who's logged in
  output$who_ui <- renderUI({
    req(auth_user())
    h5(paste("Logged in as:", auth_user()))
  })
  
  # Dynamic week dropdown (only meaningful once authenticated)
  output$week_ui <- renderUI({
    req(auth_user())
    weeks <- sort(unique(games_data()$week))
    selectInput("week", "Select Week", choices = weeks)
  })
  
  # Filter games for selected week
  current_games <- reactive({
    req(auth_user(), input$week)
    games_data() %>% filter(week == input$week)
  })
  
  # Check if selected week is locked
  week_is_locked <- reactive({
    req(input$week)
    row <- weeks_data() %>% filter(week == as.character(input$week))
    if (nrow(row) == 0) return(FALSE)
    isTRUE(row$locked[1])
  })
  
  # Disable/enable submit button based on lock status
  observe({
    req(auth_user())
    if (week_is_locked()) {
      shinyjs::disable("submit")
    } else {
      shinyjs::enable("submit")
    }
  })
  
  # Show lock message
  output$lock_status <- renderText({
    req(auth_user(), input$week)
    if (week_is_locked()) "This week is locked. Picks can no longer be changed." else ""
  })
  
  # Reactive trigger to force picks reload after submit
  picks_trigger <- reactiveVal(0)
  
  # Load existing picks for current user/week
  existing_picks <- reactive({
    picks_trigger()
    req(auth_user(), input$week)
    read_sheet(SHEET_URL, sheet = "Picks") %>%
      mutate(week      = as.character(week),
             game_id   = as.character(game_id),
             timestamp = as.character(timestamp)) %>%
      filter(user == auth_user(), week == as.character(input$week))
  })
  
  # Dynamic UI for games — pre-populate saved picks
  output$game_inputs <- renderUI({
    req(auth_user(), input$week)
    games <- current_games()
    if (nrow(games) == 0) return(h4("No games found for this week"))
    
    saved <- existing_picks()
    locked <- week_is_locked()
    
    lapply(1:nrow(games), function(i) {
      game <- games[i, ]
      
      saved_pick <- saved %>% filter(game_id == as.character(game$game_id)) %>% pull(pick)
      selected_val <- if (length(saved_pick) > 0 && !is.na(saved_pick[1])) saved_pick[1] else character(0)
      
      # Disable radio buttons if week is locked
      rb <- radioButtons(
        inputId  = paste0("game_", game$game_id),
        label    = paste(game$team_home, "vs", game$team_away, "| Spread:", game$spread),
        choices  = c(game$team_home, game$team_away),
        selected = selected_val
      )
      
      if (locked) {
        tagAppendAttributes(rb, style = "pointer-events: none; opacity: 0.6;")
      } else {
        rb
      }
    })
  })
  
  # Submit picks
  observeEvent(input$submit, {
    
    req(auth_user())
    
    # Server-side lock check — prevents submit even if button is bypassed
    if (week_is_locked()) {
      output$status <- renderText("🔒 This week is locked. Picks cannot be changed.")
      return()
    }
    
    games <- current_games()
    if (nrow(games) == 0) return()
    
    picks_vec <- sapply(games$game_id, function(id) {
      val <- input[[paste0("game_", id)]]
      if (is.null(val) || val == "") NA_character_ else val
    })
    
    new_picks <- data.frame(
      user      = auth_user(),
      week      = as.character(input$week),
      game_id   = as.character(games$game_id),
      pick      = picks_vec,
      timestamp = as.character(Sys.time())
    )
    
    existing <- read_sheet(SHEET_URL, sheet = "Picks") %>%
      mutate(week      = as.character(week),
             game_id   = as.character(game_id),
             timestamp = as.character(timestamp))
    
    updated_picks <- existing %>%
      filter(!(user == auth_user() & week == as.character(input$week) & game_id %in% as.character(games$game_id))) %>%
      bind_rows(new_picks %>% filter(!is.na(pick)))
    
    sheet_write(updated_picks, SHEET_URL, sheet = "Picks")
    
    picks_trigger(picks_trigger() + 1)
    
    output$status <- renderText("Picks saved! Previously saved picks were preserved.")
  })
}

shinyApp(ui, server)
