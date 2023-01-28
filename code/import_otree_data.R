# ------------------------------------------------------------------------------
# This reads the raw data that have been exported from oTree.
# oTree data are not included in this repo to keep private keys
# confidential but data imported by this script are
# ------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(lubridate)

EXPYEAR <- 2023
FNAME_RESP <- "private_data/ctrl_lb11_exp_2023-01-24.csv"
FNAME_PTIMES <- "private_data/PageTimes-2023-01-24.csv"

resp_raw <- read_csv(FNAME_RESP, col_types = cols(), guess_max = 1500) %>%
  filter(
    participant.code != "n6498xek", # Joachim's test run
    session.is_demo == 0, 
    participant._current_app_name == "ctrl_lb11_exp",
    !is.na(player.treatment)
  ) %>%
  mutate(time_started = as_datetime(participant.time_started_utc)) 

subjects <- resp_raw %>%
  mutate(id = str_sub(player.exp_id, 1,4)) %>%
  rename(treatment = player.treatment) %>%
  select(id, treatment, time_started) %>%
  distinct() %>%
  left_join(
    resp_raw %>%
      filter(subsession.round_number == 10) %>%
      mutate(id = str_sub(player.exp_id, 1,4)) %>%
      mutate(
        compensation = case_when(
          player.comp_q == 0 ~ "only wealth",
          player.comp_q == 1 ~ "only profit",
          player.comp_q == 2 ~ "both",
        ),
        cost_known = case_when(
          player.pcost_q == 0 ~ "only to me",
          player.pcost_q == 1 ~ "also to firm"
        ),
        budget_denial_never = player.prob1 == 1,
        budget_denial_high = player.prob2 == 1,
        budget_denial_increasing = player.prob3 == 1,
        budget_denial_decreasing = player.prob4 == 1,
      ) %>%
      rename(
        wealth_recall = player.wealth_q,
        profit_recall = player.profit_q,
        feedback = player.feedback
      ) %>%
      select(
        id, compensation, cost_known, budget_denial_never, 
        budget_denial_high, budget_denial_increasing, budget_denial_decreasing,
        wealth_recall, profit_recall, feedback
      ),
    by = "id"
  )

rounds <- resp_raw %>%
  filter(player.b_budget > 0) %>%
  mutate(
    id = str_sub(player.exp_id, 1,4),
    offer = ifelse(player.treatment == "Top-down", player.offer, NA),
    accepted = player.is_accepted == 0
  ) %>%
  rename(
    round = subsession.round_number,
    cost = player.cost,
    budget = player.b_budget,
    wealth = player.wealth,
    profit = player.profit,
    asked_invalid_budget = player.asked_invalid_budget
  ) %>%
  select(
    id, round, cost, offer, accepted, asked_invalid_budget, budget, 
    wealth, profit
  ) %>%
  arrange(id, round)

exp_ids <- resp_raw %>%
  transmute(
    public_id = substr(player.exp_id, 1, 4),
    private_id = substr(player.exp_id, 6, 21)
  ) %>%
  distinct()

times <- read_csv(
  FNAME_PTIMES, col_types = cols()
) %>%
  rename(
    otree_id = participant_code,
    round = round_number,
    epoch_time = epoch_time_completed
  ) %>%
  select(
    otree_id, round, page_name, epoch_time
  ) %>%
  left_join(
    resp_raw %>% 
      rename(otree_id = participant.code) %>% 
      mutate(id = str_sub(player.exp_id, 1, 4)) %>%
      select(otree_id, id) %>% 
      distinct(),
    by = "otree_id"
  ) %>%
  filter(!is.na(id)) %>%
  select(-otree_id) %>%
  bind_rows(
    subjects %>% 
      mutate(
        page_name = "S0", 
        round = 1,
        epoch_time = as.integer(time_started)
      ) %>%
      select(id, round, page_name, epoch_time)
  ) %>%
  arrange(id, round, page_name) %>%
  group_by(id) %>%
  mutate(
    time_entered = as_datetime(lag(epoch_time)),
    time_done = as_datetime(epoch_time),
    time_spent = epoch_time - lag(epoch_time)
  ) %>%
  ungroup() %>%
  filter(!is.na(time_spent)) %>%
  select(id, round, page_name, time_spent, time_entered, time_done)

write_csv(subjects, sprintf("data/budget_exp_subjects_%d.csv", EXPYEAR))
write_csv(rounds,  sprintf("data/budget_exp_rounds_%d.csv", EXPYEAR))
write_csv(times, sprintf("data/budget_exp_times_%d.csv", EXPYEAR))
write_csv(exp_ids, sprintf("private_data/exp_ids_%d.csv", EXPYEAR))
