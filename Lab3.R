library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stargazer)
library(lmtest)
library(car)
library(GGally)


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
    release_date = gsub('"', '', release_date),
    release_date = gsub("\\s+", "", release_date),
    release_date = as.Date(release_date),
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
    budget = log(budget),
    release_decade = if_else(release_year <= 2010, 0, 1)
  ) %>%
  filter(vote_count > 0) %>% 
  drop_na(vote_average, runtime, season, budget, vote_count, release_decade)


nrow(movies_cleaned)





model_base <- lm(vote_average ~ runtime + season, data = movies_cleaned)
model_base_hc1 <- coeftest(model_base, vcov. = hccm(model_base, type = "hc1"))
stargazer(model_base_hc1, type = "text", style = "default", digits = 3)





model_control <- lm(vote_average ~ runtime + season + budget + vote_count + release_decade, data = movies_cleaned)
model_control_hc1 <- coeftest(model_control, vcov. = hccm(model_control, type = "hc1"))




model1 <- lm(vote_average ~ runtime + season, data = movies_cleaned)


model2 <- lm(vote_average ~ runtime + I(runtime^2) + season, data = movies_cleaned)


model3 <- lm(vote_average ~ log(runtime) + season, data = movies_cleaned)


model4 <- lm(vote_average ~ log(runtime) + I(log(runtime)^2) + season, data = movies_cleaned)


model5 <- lm(vote_average ~ log(runtime) + I(log(runtime)^2) + I(log(runtime)^3) + season, data = movies_cleaned)

model1_hc1 <- coeftest(model1, vcov. = hccm(model1, type = "hc1"))
model2_hc1 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))
model3_hc1 <- coeftest(model3, vcov. = hccm(model3, type = "hc1"))
model4_hc1 <- coeftest(model4, vcov. = hccm(model4, type = "hc1"))
model5_hc1 <- coeftest(model5, vcov. = hccm(model5, type = "hc1"))


stargazer(model1, model2, model3, model4, model5,
          se = list(
            sqrt(diag(hccm(model1, type = "hc1"))),
            sqrt(diag(hccm(model2, type = "hc1"))),
            sqrt(diag(hccm(model3, type = "hc1"))),
            sqrt(diag(hccm(model4, type = "hc1"))),
            sqrt(diag(hccm(model5, type = "hc1")))
          ),
          type = "text",
          title = "Dependent variable: vote_average",
          column.labels = c("Linear", "Square", "Linear-Log", "Linear-Log-Quad", "Linear-Log-Cubic"),
          digits = 3,
          omit.stat = c("f", "ser"),
          align = TRUE)



library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)


movies_cleaned <- movies_cleaned %>%
  mutate(decade = paste0(floor(release_year / 10) * 10, "s"),
         decade = factor(decade))


model_decade <- lm(vote_average ~ decade, data = movies_cleaned)


se_decade <- sqrt(diag(vcovHC(model_decade, type = "HC1")))


stargazer(model_decade,
          se = list(se_decade),
          type = "text",
          title = "Залежність середнього рейтингу від десятиліть",
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space = TRUE)

model_decade <- lm(vote_average ~ decade, data =movies_cleaned)
summary(model_decade)



library(stargazer)


model <- lm(vote_average ~ runtime + season, data = movies_cleaned)

stargazer(model,
          type = "text",
          title = "Модель з runtime та season",
          dep.var.labels = "vote_average",
          digits = 3,
          no.space = TRUE)

model_interaction <- lm(vote_average ~ runtime * season + budget + vote_count + decade, data = movies_cleaned)

library(lmtest)
library(sandwich)
se_interaction <- sqrt(diag(vcovHC(model_interaction, type = "HC1")))


stargazer(model_interaction,
          se = list(se_interaction),
          type = "text",
          title = "Модель з взаємодією runtime і season та контрольними змінними",
          digits = 3,
          omit.stat = c("f", "ser"),
          no.space = TRUE)


summary(model_interaction)




