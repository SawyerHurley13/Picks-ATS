# _data.R — sourced by results.qmd and standings.qmd

#setwd("/Users/redfi/Documents/Personal R/Picks ATS")

library(googlesheets4)
library(dplyr)
library(tidyr)

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1HNe0QmeT8SnXNh0zrh2Z5QoSYG_-VBs56PU_BEkev00/edit?gid=1781931991#gid=1781931991"


gs4_deauth()  # public read access, no auth needed for reading

# --- Raw data --------------------------------------------------------------

games <- read_sheet(SHEET_URL, sheet = "Games") %>%
  mutate(week    = as.character(week),
         game_id = as.character(game_id))

picks <- read_sheet(SHEET_URL, sheet = "Picks") %>%
  mutate(user    = as.character(user),
         week    = as.character(week),
         game_id = as.character(game_id),
         pick    = as.character(pick),
         bonus   = if ("bonus" %in% names(.)) as.logical(bonus) else FALSE)

all_users <- read_sheet(SHEET_URL, sheet = "Users") %>%
  mutate(user = as.character(user)) %>%
  pull(user)

# --- Scoring rules -----------------------------------------------------
# - Correct pick (or push, which counts as correct for everyone): +$100
# - Incorrect pick: -$100
# - Bonus flag (max 5/week) stacks an EXTRA +$200 if correct, -$200 if
#   wrong, on top of the normal $100 above
# - Failing to make a pick on a completed game: -$25 penalty

BASE_WIN    <- 100
BONUS_WIN   <- 200
MISSED_DOCK <- 25

# --- Join picks to games and evaluate correctness ---------------------------

results <- picks %>%
  left_join(games %>% select(game_id, week, team_home, team_away, spread, correct_pick),
            by = c("game_id", "week")) %>%
  mutate(
    correct = case_when(
      is.na(correct_pick)    ~ NA,       # game not yet played
      correct_pick == "PUSH" ~ TRUE,     # push = win for everyone
      pick == correct_pick   ~ TRUE,
      TRUE                    ~ FALSE
    ),
    base_dollars = case_when(
      is.na(correct) ~ NA_real_,
      correct         ~ BASE_WIN,
      TRUE            ~ -BASE_WIN
    ),
    bonus_dollars = case_when(
      is.na(correct)  ~ NA_real_,
      !bonus           ~ 0,
      bonus & correct  ~ BONUS_WIN,
      TRUE             ~ -BONUS_WIN
    )
  )

# --- Missed picks: completed games a user never picked ----------------------

missed_by_week <- games %>%
  filter(!is.na(correct_pick)) %>%
  select(week, game_id) %>%
  crossing(user = all_users) %>%
  anti_join(picks %>% select(user, week, game_id), by = c("user", "week", "game_id")) %>%
  group_by(user, week) %>%
  summarise(missed_dollars = -MISSED_DOCK * n(), .groups = "drop")

# --- Weekly results table — one row per user/week ---------------------------

weekly_summary <- results %>%
  filter(!is.na(correct_pick)) %>%
  group_by(user, week) %>%
  summarise(
    correct       = sum(correct, na.rm = TRUE),
    total         = n(),
    base_dollars  = sum(base_dollars, na.rm = TRUE),
    bonus_dollars = sum(bonus_dollars, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  full_join(missed_by_week, by = c("user", "week")) %>%
  mutate(
    correct        = coalesce(correct, 0),
    total          = coalesce(total, 0),
    base_dollars   = coalesce(base_dollars, 0),
    bonus_dollars  = coalesce(bonus_dollars, 0),
    missed_dollars = coalesce(missed_dollars, 0),
    week_dollars   = base_dollars + bonus_dollars + missed_dollars
  ) %>%
  arrange(suppressWarnings(as.numeric(week)), user)

# --- Overall standings -------------------------------------------------------

standings <- weekly_summary %>%
  group_by(user) %>%
  summarise(
    correct        = sum(correct),
    total          = sum(total),
    pct            = round(100 * correct / total, 1),
    base_dollars   = sum(base_dollars),
    bonus_dollars  = sum(bonus_dollars),
    missed_dollars = sum(missed_dollars),
    total_dollars  = sum(week_dollars),
    .groups = "drop"
  ) %>%
  arrange(desc(total_dollars))

