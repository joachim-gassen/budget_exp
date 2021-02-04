# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2021, gassen@wiwi.hu-berlin.de
# License: MIT. See LICENSE file for details.
#
# Shiny app communicating the results of the budgeting experiment
# ------------------------------------------------------------------------------

library(shiny, quietly = TRUE)
library(tidyverse)
library(kableExtra)
library(zip)
library(ggbeeswarm)
library(ggridges)

transpose_table <- function(tbl) {
  # tbl Needs to have a first column containing variable names
  # for the transposed table (normally group levels)
  ttbl <- as_tibble(t(tbl[, 2:ncol(tbl)])) %>%
    mutate(` ` = names(tbl)[2:ncol(tbl)]) %>%
    select(c(nrow(tbl) + 1, 1:nrow(tbl)))
  
  names(ttbl)[2:ncol(ttbl)] <- pull(tbl[, 1])
  ttbl
}

subjects_static <- read_csv("data/budget_exp_subjects.csv", col_types = cols())
rounds_static <- read_csv("data/budget_exp_rounds.csv", col_types = cols())
times_static <- read_csv("data/budget_exp_times.csv", col_types = cols())

participation <- subjects_static %>%
  left_join(rounds_static, by = "id") %>%
  filter(!is.na(round)) %>%
  rename(Treatment = treatment) %>%
  group_by(Treatment) %>%
  summarise(
    `Unique players` = length(unique(id)),
    `Played rounds` = n(),
    `Completed games` = sum(!is.na(compensation))/10,
    .groups = "drop"
  ) %>%
  transpose_table()

times_by_id <- times_static %>%
  filter(page_name == "S1") %>%
  mutate(round = 0) %>%
  select(id, round, time_spent) %>%
  bind_rows(
    times_static %>%
      filter(page_name %in% c("S2", "S3")) %>%
      select(id, round, time_spent) %>%
      group_by(id, round) %>%
      summarise(time_spent = sum(time_spent), .groups = "drop")
  ) %>%
  bind_rows(
    times_static %>%
      group_by(id) %>%
      filter(any(page_name == "S4")) %>%
      summarise(time_spent = sum(time_spent), .groups = "drop") %>%
      mutate(round = 11) %>%
      select(id, round, time_spent)
  ) %>%
  arrange(id, round)

times_table <- times_by_id %>%
  group_by(round) %>%
  summarise(
    N = n(),
    Mean = mean(time_spent),
    `Standard deviation` = sd(time_spent),
    Median = median(time_spent),
    Min = min(time_spent),
    Max = max(time_spent),
    .groups = "drop"
  ) %>%
  rename(Round = round) %>%
  arrange(Round) %>%
  mutate_at(c("Mean", "Standard deviation", "Median"), format, digits = 1) %>%
  mutate(
    Round = case_when(
      Round == 0 ~ "Intro", 
      Round == 11 ~ "Total",
      TRUE ~ paste("Round", Round)
    )
  )

subjects_mc <- subjects_static %>%
  mutate(
    completed_exp = !is.na(cost_known),
    passed_mc = (
      compensation == "only wealth" & cost_known == "only to me" &
        budget_denial_never == TRUE & budget_denial_high == FALSE &
        budget_denial_increasing == FALSE & budget_denial_decreasing == FALSE
    )
  ) %>% 
  left_join(
    times_by_id %>%
      filter(round == 11 | round == 0) %>%
      mutate(name = ifelse(round == 0, "time_intro", "time_total")) %>%
      select(id, name, time_spent) %>%
      pivot_wider(id_cols = id, names_from = name, values_from = time_spent),
    by = "id"
  )

mc_table <- subjects_mc %>%
  filter(completed_exp) %>%
  group_by(treatment) %>%
  rename(Treatment = treatment) %>%
  summarise(
    `Completed experiment` = n(),
    `Passed manipulaton checks` = sprintf(
      "%d (%.1f %%)", sum(passed_mc), 100*(sum(passed_mc)/n())
    ),
    .groups = "drop"
  ) %>%
  transpose_table()

ui <- fluidPage(
  titlePanel("A Budgeting Experiment: Findings"),
  br(),
  br(),
  br(),
  p(HTML("These are the findings of our classroom experiment.", 
    "The design of the experiment is heavily inspired by",
    "the baseline case of",
    "<a href=https://doi.org/10.2308/accr.2001.76.4.537>",
    "Evans, Hannan, Krishnan and Moser (TAR, 2001).</a>",
    "To spice it up a little we added a treatment where firm",
    "headquarters offer a budget to the manager (the experimental",
    "subject). They can then either agree with it or request a different budget."
    )),
  br(),
  h3("Participation"),
  p("The Table below shows the number of participants by treatment",
    "and the number of completed rounds."),
  tableOutput("participants"),
  hr(),
  h3("Time spent by round"),
  p("How long did it take you to process the intro screen and the",
    "respective rounds?"),
  tableOutput("times"),
  hr(),
  h3("Manipulation Check"),
  p("How many of you answered all manipulation checks correctly",
    "(Compensation depends only on personal wealth, unit costs were",
    "only known to you and firm will always accept your budget"),
  tableOutput("manipulation_check"),
  p("Does this depend on the time that you spend on the experiment?"),
  plotOutput("mc_times"),
  hr(),
  p("OK. Now we turn to the experimental results. By default, they are",
    "presented for the full sample. Alternatively you can use the left side",
    "bar to limit the sample to subjects that passed the manipulation checks", 
    "and/or that took a reasonable amount of time to process the experimental",
    "material."),
  p("In addition, you can download all experimental data to prepare your own",
    "anaylsis"
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      radioButtons("smp_sel", "Do you want to limit the sample?",
                   c("All observations" = "none",
                     "Only observations that pass manipulation checks" = "mc")),
      br(),
      sliderInput("exclude_below_time",
                  "Only observations that spend more than ... seconds on the intro page",
                  value = 0,
                  min = 0,
                  max = 120),
      downloadButton("download", "Download data")
    ),
    mainPanel(
      h3("Honesty versus Wealth"),
      p("What was the share of the available budget slack that you",
        "claimed for yourself? Compare this to",
        "Evans et al. (2001, Figure 1, p. 549)"),
      plotOutput("ave_cbs"),
      p("A similar plot containing all data"),
      plotOutput("all_cbs"),
      p("The next plot displays the average claimed budget slack over time",
        "and by treatment. You did not converge to an egoistic stratgey."),
      plotOutput("cbs_time"),
      br(),
      tableOutput("cbs_table"),
      p("Were you willing to sacrify significant money?"),
      plotOutput("wealth_hist"),
      p("Yes you were."),
      tableOutput("wealth_tests"),
      hr(),
      h3("The Treatment Effect"),
      p("While the 'Bottom-up' group simply had to file their requested",
        "budget, the 'Top-down' group first received a random budget offer",
        "from firm headquarters. Has this random number influenced your",
        "decision making?"),
      plotOutput("tment_match"),
      p("Hard to tell. You notice, however, that when you exclude the",
        "players that played egoistically that the reminders were anchored",
        "by the offer and also accepted it much more often that one would",
        "expect."),
      tableOutput("tment_tests"),
      p("This is it. Feel free to download your experimental data and",
        "see for yourself!")
    )
  ),
  hr(),
  fluidRow(
    column(
      12, align="center",
      HTML(
        "Created with <a href=https://shiny.rstudio.com>R/shiny</a> by",
        "Joachim Gassen,",
        "<a href=https://www.wiwi.hu-berlin.de/de/professuren/bwl/rwuwp/staff/gassen>",
        "Humboldt-Universität zu Berlin</a>",
        "and <a href=https://www.accounting-for-transparency.de>",
        "TRR 266 'Accounting for Transparency'</a>, 2021.<br>",
        "See <a href='https://github.com/joachim-gassen/budget_exp'>",
        "GitHub repository</a> for license, code and details."
      )
    )
  )
)    

server <- function(input, output, session) {
  rounds <- reactive({
    if (input$smp_sel  == "mc") df <- rounds_static %>%
      inner_join(subjects_mc %>% filter(passed_mc)  %>% select(id), by = "id")
    else df <- rounds_static
    if (input$exclude_below_time > 0)
      df <- df %>%
        anti_join(
          subjects_mc %>% 
            filter(time_intro < input$exclude_below_time)  %>% 
            select(id),
          by = "id"
        )
    
    df %>%
      mutate(
        pot_slack = 6000 - cost*1000,
        claimed_slack = budget - cost*1000,
        pct_slack_claimed = claimed_slack/pot_slack
      ) %>%
      left_join(subjects_static %>% select(id, treatment), by = "id")
  })
  
  outcomes <- reactive({
    rounds() %>%
      group_by(id) %>%
      summarise(
        rounds = n(),
        pot_slack = sum(pot_slack),
        pct_slack_claimed = sum(claimed_slack)/sum(pot_slack),
        partialy_honest = pct_slack_claimed != 1 & 
          pct_slack_claimed != 0,
        always_honest = pct_slack_claimed == 0,
        .groups = "drop"
      ) %>%
      mutate(wealth = 10000 + pct_slack_claimed * pot_slack) %>% 
    left_join(subjects_mc %>% select(id, treatment, passed_mc, time_total), by = "id")
  })  
  

  output$participants <- function() {
    kable(participation, format = "html") %>%
       kable_styling(full_width = FALSE)
  }
  
  output$times <- function() {
    kable(times_table, align = "lrrrrrr") %>%
      kable_styling(full_width = FALSE) %>% 
      row_spec(11, extra_css = "border-bottom: solid; border-bottom-width: 0.08em;")
  }
  
  output$manipulation_check <- function() {
    kable(mc_table, align = "lrr") %>%
      kable_styling(full_width = FALSE)
  }
  
  output$mc_times <- renderPlot({
    ggplot(
      data = subjects_mc %>%
        select(id, passed_mc, time_intro, time_total) %>%
        filter(time_total <= 3600) %>%
        rename(
          Intro = time_intro,
          Total = time_total
        ) %>%
        pivot_longer(
          all_of(c("Intro", "Total")), names_to = "time", values_to = "sec"
        ), 
      aes(x = time, y = sec, group = passed_mc, color = passed_mc)
    ) +
      geom_quasirandom(dodge.width = 0.8, width = 0.1, size = 3) +
      theme_classic(base_size = 16) + 
      labs(
        title = sprintf(
          "Time spent in seconds (%d obs with total > 1h excluded)",
          nrow(subjects_mc %>% filter(time_total > 3600))
        ),
        x = "",
        y = "",
        color = "Passed manipulation check"
      ) + theme(legend.position = "bottom", plot.title.position = "plot") 
  })

  output$ave_cbs <- renderPlot({
    df <- rounds() %>%
      group_by(cost) %>%
      summarise(ave_cbs_unit = mean(claimed_slack) / 1000, .groups = "drop")
    
    ggplot(df, aes(x = cost, y = ave_cbs_unit)) +
      geom_point(color = "#377EB8", size = 3) +
      labs(
        title = "Average claimed budget slack per unit [Taler]",
        x = "Cost per unit [Taler]",
        y = "",
        color = "Treatment"
      ) +
      theme_classic(base_size = 16) +
      geom_segment(x = 4, y = 2, xend = 6, yend = 0, color = "#E41A1C", lty = 2) + 
      xlim(4.0, 6.0) + ylim(0.0, 2.0) +
      theme(plot.title.position =  "plot")
  })
  
  
  output$all_cbs <- renderPlot({
    df <- rounds() %>%
      mutate(cbs_unit = claimed_slack/1000)
    
    ggplot(df, aes(x = cost, y = cbs_unit, color = treatment)) +
      geom_jitter(size = 3) +
      labs(
        title = "Average claimed budget slack per unit [Taler]",
        x = "Cost per unit [Taler]",
        y = "",
        color = "Treatment"
      ) +
      theme_classic(base_size = 16) +
      geom_segment(x = 4, y = 2, xend = 6, yend = 0, color = "#E41A1C", lty = 2) + 
      xlim(4.0, 6.0) + ylim(0.0, 2.0) +
      theme(plot.title.position =  "plot", legend.position = "bottom")
  })
  
  output$cbs_table <- function() {
    tab <- outcomes() %>%
      group_by(treatment) %>%
      summarise(
        Particpants = sprintf("%d", n()),
        Rounds = sprintf("%d", sum(rounds)),
        `Mean claimed budget slack (%)` = scales::percent(
          mean(pct_slack_claimed)
        ),
        `Std. dev. claimed budget slack (%)` = scales::percent(
          sd(pct_slack_claimed)
        ),
        `Partially honest (%)` = scales::percent(sum(partialy_honest)/n()), 
        `Always honest (%)` = scales::percent(sum(always_honest)/n()),
        .groups = "drop"
      ) %>%
      transpose_table()
    
    kable(tab, align = "lrr") %>%
      kable_styling(full_width = FALSE)
  }
  
  output$cbs_time <- renderPlot({
    df <- rounds() %>%
      filter(cost != 6) %>%
      group_by(round, treatment) %>%
      mutate(round = as.factor(round)) %>%
      summarise(mean_pct_slack_claimed = mean(pct_slack_claimed), .groups = "drop") 
    
    ggplot(df, aes(
        x = round, y = mean_pct_slack_claimed, 
        group = treatment, color = treatment
      )) +
      geom_point(size = 3) +
      geom_line() +
      scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
      labs(
        title = "Mean share of claimed budget slack by treatment and round",
        x = "Round",
        y = ""
      ) +
      theme_classic(base_size = 16) +
      theme(
        legend.position = "bottom",
        plot.title.position = "plot"
      )
  })

    
  output$wealth_hist <- renderPlot({
    ggplot(outcomes() %>% filter(rounds == 10), aes(
      x = wealth, y = treatment,
      fill = treatment, height = stat(density)
    )) +
      geom_density_ridges(
        stat = "binline", binwidth = 2000, center = 11000, scale = 0.95
      ) +
      scale_x_continuous(
        labels =  scales::comma_format(accuracy = 1), 
        limits = c(10000, 30000)
      ) +
      labs(
        title = "Accumulated private wealth by treatment",
        x = "Private wealth [Taler]",
        y = ""
      ) +
      geom_vline(xintercept = 20000, color = "#E41A1C", lty = 2) +
      theme_ridges() +
      theme(
        legend.position = "bottome",
        plot.title.position = "plot"
      )    
  })
  
  output$wealth_tests <- function() {
    tab <- outcomes() %>% 
      filter(rounds == 10) %>%
      group_by(treatment) %>%
      summarise(
        `t-stat (mu = 20,000)` = sprintf(
          "%.3f", t.test(wealth, mu= 20000)$statistic
        ),
        `Prob |t| > 0` = sprintf("%.3f", t.test(wealth, mu = 20000)$p.value),
        .groups = "drop"
      ) %>%
      bind_rows(
        outcomes() %>%
          summarise(
            treatment = "Total",
            `t-stat (mu = 20,000)` = sprintf(
              "%.3f", t.test(wealth, mu = 20000)$statistic
            ),
            `Prob |t| > 0` = sprintf("%.3f", t.test(wealth, mu = 20000)$p.value),
            .groups = "drop"
          )
      ) %>%
      transpose_table()
    
    kable(tab, align = "lrrr") %>%
      kable_styling(full_width = FALSE)
  }
  
  output$tment_match <- renderPlot({
    rounds() %>%
      filter(treatment == "Top-down") %>%
      mutate(offer_feasible = offer >= cost*1000) %>%
      ggplot(aes (x = offer, y = budget, color = offer_feasible)) +
      geom_jitter(size = 3) +
      labs(
        x = "Offered budget [Taler]",
        y = "Final budget [Taler]",
        color = "Offer is feasible"
      ) +
      theme_classic(base_size = 16) +
      xlim(4000, 6000) + ylim(4000, 6000) + 
      theme(legend.position = "bottom")     
  })
  

  output$tment_tests <- function() {
    df <- rounds() %>%
      filter(treatment == "Top-down") 
    
    fs_cor <- cor.test(df$offer, df$budget, method = "spearman", exact = FALSE)
    n6_cor <- cor.test(
      df$offer[df$budget != 6000], df$budget[df$budget != 6000], 
      method = "spearman", exact = FALSE
    )

    tab <- tibble(
      ` ` = c(
        "% accepted",
        "Spearman corr offered/final (full sample)",
        "Spearman corr offered/final (max budgets excluded)"
      ),
      Statistic = c(
        scales::percent(sum(df$accepted, na.rm = TRUE)/nrow(df)),
        sprintf("%.3f (|Prob| > 0: %.3f)", fs_cor$estimate, fs_cor$p.value),
        sprintf("%.3f (|Prob| > 0: %.3f)", n6_cor$estimate, n6_cor$p.value)
      )
    )
    
    kable(tab) %>%
      kable_styling(full_width = FALSE)
  }
  

  output$download <- downloadHandler(
    filename <- function() {
      paste0('budget_exp_data-', Sys.Date(), '.zip')
    },
    content <- function(con) {
      path <- tempdir()
      write_csv(rounds_static, file = file.path(path, "rounds.csv"))
      write_csv(subjects_static, file = file.path(path, "subjects.csv"))
      write_csv(times_static, file = file.path(path, "times.csv"))
      zipr(
        con, 
        files = list.files(path = path, pattern = ".csv$", full.names = TRUE)
      )
    }
  )
}


# Run the application 
shinyApp(ui = ui, server = server)

