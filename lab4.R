
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(np)

movies_initial <- read_csv("TMDB_movie_dataset_v11.csv")

budget_to_na <- c(1381066, 1235037, 1057999, 1022208, 
                  1201764, 1399448, 1426913, 1449031, 1320160, 
                  1453767, 1453985, 1414861, 1398923, 1365277,
                  1417006, 1441191, 1450893, 1450893, 1301115, 1228885,
                  1272552, 1229118, 1294480, 622311, 1369796, 1108211)

runtime_to_na <- c(206026, 368247, 717019, 392372, 454409, 500980,
                   544686, 732330, 66871, 633832, 671214, 531640, 535892,
                   523167, 631038, 698754, 685310, 651033, 125120, 298752)

movies_cleaned <- movies_initial %>%
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count   = if_else(vote_count < 0, NA_integer_, vote_count),
    runtime      = if_else(runtime <= 0, NA_real_, runtime),
    budget       = if_else(budget < 10, NA_real_, budget),
    release_date = as.Date(gsub("\\s+|\"", "", release_date)),
    release_year = year(release_date),
    release_month = month(release_date, label = TRUE, abbr = TRUE),
    season = case_when(
      release_month %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_
    ),
    budget = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime),
    season = factor(season),
    release_decade = if_else(release_year <= 2010, 0, 1)
  ) %>%
  filter(vote_count > 0) %>%
  drop_na(vote_average, runtime, season, budget, vote_count, release_decade)

nrow(movies_cleaned)

movies_autumn <- movies_cleaned %>% filter(season == "Осінь")

set.seed(123)
train_index <- sample(1:nrow(movies_autumn), size = 0.7 * nrow(movies_autumn))

train_data <- movies_autumn[train_index, ]
test_data  <- movies_autumn[-train_index, ]

lm_model <- lm(vote_average ~ budget + runtime + vote_count + release_decade, data = train_data)

summary(lm_model)

budget_grid <- seq(min(train_data$budget), max(train_data$budget), length.out = 500)


fixed_runtime <- 90
fixed_vote_count <- median(train_data$vote_count, na.rm = TRUE)
fixed_release_decade <- 1


new_data <- data.frame(
  budget = budget_grid,
  runtime = fixed_runtime,
  vote_count = fixed_vote_count,
  release_decade = fixed_release_decade
)


pred_lm <- predict(lm_model, newdata = new_data, se.fit = TRUE)


df_plot <- data.frame(
  budget = budget_grid,
  vote_average = pred_lm$fit,
  se = pred_lm$se.fit
)


ggplot(df_plot, aes(x = budget, y = vote_average)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = vote_average - 1.96 * se, ymax = vote_average + 1.96 * se),
              fill = "lightblue", alpha = 0.3) +
  labs(title = "ЛІНІЙНА модель: vote_average ~ budget + runtime + vote_count + release_decade",
       x = "Бюджет (budget)",
       y = "Середня оцінка (vote_average)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))









bw <- npplregbw(vote_average ~ budget | runtime + vote_count + release_decade, data = train_data)
model_nppl <- npplreg(bws = bw)



budget_grid_df <- data.frame(
  budget = budget_grid,
  runtime = fixed_runtime,
  vote_count = fixed_vote_count,
  release_decade = fixed_release_decade
)

pred_nppl <- predict(model_nppl, newdata = budget_grid_df)

df_plot_nppl <- data.frame(
  budget = budget_grid,
  vote_average = pred_nppl
)

ggplot(df_plot_nppl, aes(x = budget, y = vote_average)) +
  geom_line(color = "brown", size = 1.2) +
  labs(title = "ЧАСТКОВО-ЛІНІЙНА: vote_average ~ budget | runtime, vote_count, release_decade",
       x = "Бюджет (budget)",
       y = "Середня оцінка (vote_average)") +
  theme_minimal()

