# _data.R — sourced by results.qmd and standings.qmd

#setwd("/Users/redfi/Documents/Personal R/Picks ATS")

library(googlesheets4)
library(dplyr)

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1HNe0QmeT8SnXNh0zrh2Z5QoSYG_-VBs56PU_BEkev00/edit?gid=1781931991#gid=1781931991"


gs4_deauth()  # public read access, no auth needed for reading

games <- read_sheet(SHEET_URL, sheet = "Games") %>%
  mutate(week    = as.character(week),
         game_id = as.character(game_id))

picks <- read_sheet(SHEET_URL, sheet = "Picks") %>%
  mutate(week    = as.character(week),
         game_id = as.character(game_id))

# Join picks to games and evaluate correctness
results <- picks %>%
  left_join(games %>% select(game_id, team_home, team_away, spread, correct_pick), by = "game_id") %>%
  mutate(correct = case_when(
    is.na(correct_pick) ~ NA,           # game not yet played
    correct_pick == "PUSH"     ~ TRUE,       # push = win for everyone
    pick == correct_pick ~ TRUE,
    TRUE ~ FALSE
  ))

# Weekly results table — one row per user/week
weekly_summary <- results %>%
  filter(!is.na(correct_pick)) %>%
  group_by(user, week) %>%
  summarise(
    correct = sum(correct, na.rm = TRUE),
    total   = n(),
    .groups = "drop"
  ) %>%
  arrange(as.numeric(week), user)

# Overall standings
standings <- results %>%
  filter(!is.na(correct_pick)) %>%
  group_by(user) %>%
  summarise(
    correct = sum(correct, na.rm = TRUE),
    total   = n(),
    pct     = round(correct / total * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(correct))