library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

# I ran this code prior to excluding the data with experimental id "vp2l7wsl"
# So, to reproduce it, you would need to comment out line 17 from 
# 'import_otree_data.R'

subjects <- read_csv("data/budget_exp_subjects.csv", col_types = cols())
rounds <- read_csv("data/budget_exp_rounds.csv", col_types = cols())

wealth <- subjects %>% 
  filter(!is.na(compensation)) %>%
  left_join(rounds, by = "id")  %>%
  mutate(
    claimed_slack = budget - cost*1000
  ) %>% 
  group_by(id) %>%
  summarise(wealth = 10000 + sum(claimed_slack), .groups = "drop")

tickets <- do.call(c, mapply(rep, wealth$id, wealth$wealth/100, USE.NAMES = F))

set.seed(42)
winners <- NULL
remaining_tickets <- tickets
for (draw in 1:10) {
  winner <- sample(remaining_tickets, 1)
  remaining_tickets <- remaining_tickets[remaining_tickets != winner]
  winners <- c(winners, winner)
}

df <- wealth %>%
  left_join(tibble(id = winners, winner = TRUE), by = "id") %>%
  mutate(winner = ifelse(is.na(winner), FALSE, winner))

t.test(df$wealth[df$winner], df$wealth[!df$winner])

private_keys <- read_csv(
  "otree_raw_data/ctrl_lb11_exp_2021-02-04.csv", col_types = cols(), 
  guess_max = 1500 
) %>%
  transmute(
    id = substr(player.exp_id, 1, 4),
    key = substr(player.exp_id, 6, 21)
  ) %>%
  distinct()

winners <- tibble(id = winners) %>%
  left_join(private_keys, by = "id") %>%
  bind_cols(read_csv("otree_raw_data/amazon_keys.csv", col_type = cols()))

create_winner_plot <- function(winner) {
  df <- rounds %>%
    filter(id == winner) %>%
    select(round, wealth, profit) %>%
    pivot_longer(
      all_of(c("wealth", "profit")), names_to = "outcome", values_to = "value"
    ) 
  
  df$outcome <- ifelse(df$outcome == "wealth", "Your wealth", "Firm profits")
  
  ggplot(df, aes(
      as.factor(round), value, 
      label = outcome, group = outcome, color = outcome
    )) +
    geom_line() +
    geom_point() +
    geom_text_repel(data = df %>% filter(round == 10), nudge_x = -1) +
    theme_classic() +
    labs(
      x = "Round",
      title = "Vielen Dank für die Teilnahme und herzlichen Glückwunsch!",
      y = "",
      caption = paste(
        "Was Sie da sehen, sind Ihre Daten. Und nun zur Entlohnung:\n",
        "Ihr Amazon Gutschein Einlösungscode ist:", 
        paste0("'", winners$gutschein_code[winners$id == winner], "'.\n"),
        "Sie können ihn hier einlösen: https://www.amazon.de/gc/redeem."
      )
    ) + 
    theme(legend.position = "none", plot.title.position = "plot")
}

create_voucher_files <- function(ids) {
  my_wd<-getwd()
  tmp_dir <- tempdir()
  setwd(tmp_dir)
  for (id in ids) {
    pdf_file <- file.path(sprintf("gutschein_%s.pdf", id))
    zip_file <- file.path(my_wd, sprintf("otree_raw_data/gutschein_%s.zip", id))
    ggsave(pdf_file, create_winner_plot(id), width = 6, height = 6)
    zip(
      zip_file, files = pdf_file, 
      flags = paste("--password", winners$key[winners$id == id])
    )
  }
  setwd(my_wd)
}

create_voucher_files(winners$id)
