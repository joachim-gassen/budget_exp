library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

EXPYEAR <- 2024

subjects <- read_csv(
  sprintf("data/budget_exp_subjects_%d.csv", EXPYEAR), col_types = cols()
)
rounds <- read_csv(
  sprintf("data/budget_exp_rounds_%d.csv", EXPYEAR), col_types = cols()
)
exp_ids <- read_csv(
  sprintf("private_data/exp_ids_%d.csv", EXPYEAR), col_types = cols()
)

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

winners <- tibble(public_id = winners) %>%
  left_join(exp_ids, by = "public_id") %>%
  bind_cols(read_csv("private_data/amazon_keys.csv", col_type = cols()))

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
      title = "Thank you participating and congratulations!",
      y = "",
      caption = paste(
        "Thiese are your experimental data. And now for your compensation:\n",
        "Your Amazon voucher url is:", 
        winners$voucher_url[winners$public_id == winner], ".\n",
        "Simply click or copy & paste into your browser to redeem."
      )
    ) + 
    theme(legend.position = "none", plot.title.position = "plot")
}

create_voucher_files <- function(ids) {
  my_wd <- getwd()
  tmp_dir <- tempdir()
  setwd(tmp_dir)
  for (id in ids) {
    pdf_file <- file.path(sprintf("voucher_%s.pdf", id))
    zip_file <- file.path(my_wd, sprintf("private_data/voucher_%s.zip", id))
    ggsave(pdf_file, create_winner_plot(id), width = 6, height = 6)
    zip(
      zip_file, files = pdf_file, 
      flags = paste("--password", winners$private_id[winners$public_id == id])
    )
  }
  setwd(my_wd)
}

create_voucher_files(winners$public_id)

email_text <- "
Classroom experiment: the winners have been selected

Dear all:

Those of you who participated in last week's classroom experiment: We have 
drawn the lucky winners. If your public 4 digit experimental ID is included 
in one of the URLs below, you can claim your 10 € Amazon voucher.

The winning IDs/URLs are: 

%s
To claim your woucher click on the respective link. It will lead you to a 
password zipped PDF containing the information on how to redeem your 
voucher. The password is your case-sensitive private experimental ID.
This is the 16 digit string following your public experimental ID. You did
write this one down, right? Please claim your voucher soon as I might be
reusing unclaimed vouchers in the next term.

In any case, winner or not, you can still take a look at the experimental
findings at: https://jgassen.shinyapps.io/budget_exp/

Thanks for participating and enjoy your weekends everybody!

Joachim Gassen
"

cat(sprintf(
  email_text, 
  paste(
    sprintf(
      "https://trr266.wiwi.hu-berlin.de/ctrl/budget_exp/voucher_%s.zip\n",
      sort(winners$public_id)
    ),
    collapse = ""
  )
))
