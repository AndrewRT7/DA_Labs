library(tidyverse)
library(dplyr)
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
  # Очистка числових колонок
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count   = if_else(vote_count <= 0, NA_integer_, vote_count),
    runtime      = if_else(runtime <= 0, NA_real_, runtime),
    budget       = if_else(budget < 10, NA_real_, budget)
  ) %>%
  # Обробка дати
  mutate(
    release_date = gsub('"', '', release_date),
    release_date = gsub("\\s+", "", release_date),
    release_date = as.Date(release_date),
    release_year = year(release_date),
    release_month = month(release_date, label = TRUE, abbr = TRUE)
  ) %>%
  # Створення сезонної змінної
  mutate(
    season = case_when(
      release_month %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_
    )
  ) %>%
  # Виправлення вручну
  mutate(
    budget  = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime)
  ) %>%
  mutate(
    season = factor(season),
    budget = log(budget)
  ) %>%
  # Видалення усіх NA з основних змінних
  drop_na(vote_average, runtime, season, budget, vote_count)


# тільки потрібні колонки
movies_cleaned <- movies_cleaned[, c("vote_average", "runtime", "season", "vote_count",
                                     "release_year", "budget")]


movies_cleaned <- movies_cleaned %>%
  filter(!is.na(vote_average), !is.na(runtime), !is.na(season), 
         !is.na(vote_count), !is.na(budget), !is.na(release_year)) %>%
  mutate(
    vote_count_log = log(vote_count),
    budget_log = log(budget),
    decade = factor(floor(release_year / 10) * 10),
    release_decade_bin = ifelse(movies_cleaned$release_year <= 2010, 0, 1)
  )



# ============= МОДЕЛІ ==============
# базова
model_base <- lm(vote_average ~ runtime + season, data = movies_cleaned)
model_base_hc1 <- coeftest(model_base, vcov. = hccm(model_base, type = "hc1"))

stargazer(model_base, 
          type = "text",
          se = model_base_hc1[, 2],
          omit.stat = c("rsq", "f", "ser"),
          style = "default", 
          digits = 3)




# контрольні змінні
model_сrl <- lm(vote_average ~ runtime + season + vote_count + release_year+log(budget), data = movies_cleaned)
model_сrl_hc1 <- coeftest(model_сrl, vcov. = hccm(model_сrl, type = "hc1"))
# бінарні десятиліття
model_сrl_bin <- lm(vote_average ~ runtime + season + vote_count + release_decade_bin+log(budget), data = movies_cleaned)
model_сrl_bin_hc1 <- coeftest(model_сrl_bin, vcov. = hccm(model_сrl_bin, type = "hc1"))

stargazer(model, model_сrl, model_сrl_bin, 
          type = "text",
          column.labels = c("Базова", "Контрольні змінні", "Десятиліття"),
          se = list(model_hc1[, 2], model_сrl_hc1[, 2], model_сrl_bin_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          digits = 3)




# runtime^2
model2 <- lm(vote_average ~ runtime + I(runtime^2) + season, data = movies_cleaned)
model2_hc1 <- coeftest(model2, vcov. = hccm(model2, type = "hc1"))

# log(runtime)
model_log <- lm(vote_average ~ I(log(runtime)) + season, data = movies_cleaned)
model_log_hc1 <- coeftest(model_log, vcov. = hccm(model_log, type = "hc1"))

# log(runtime)^2
model_log2 <- lm(vote_average ~ I(log(runtime)) + I(log(runtime)^2) + season, data = movies_cleaned)
model_log2_hc1 <- coeftest(model_log2, vcov. = hccm(model_log2, type = "hc1"))

# log(runtime)^3
model_log3 <- lm(vote_average ~ I(log(runtime)) + I(log(runtime)^2) + I(log(runtime)^3) + season, data = movies_cleaned)
model_log3_hc1 <- coeftest(model_log3, vcov. = hccm(model_log3, type = "hc1"))

stargazer(model, model2, model_log, model_log2, model_log3,
          type = "text",
          column.labels = c("Linear", "Square", "Linear-Log", "Linear-Log-Quad", "Linear-Log-Cubic"),
          se = list(model_hc1[, 2], model2_hc1[, 2], model_log_hc1[, 2], model_log2_hc1[, 2], model_log3_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          digits = 3) 




# факторна взаємодія
model_inter <- lm(vote_average ~ runtime + season + runtime:season, data = movies_cleaned)
model_inter_hc1 <- coeftest(model_inter, vcov. = hccm(model_inter, type = "hc1"))

stargazer(model, model_inter, 
          type = "text",
          column.labels = c("Базова", "Факторна взаємодія"),
          se = list(model_hc1[, 2], model_inter_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          digits = 3)




# ============= OЦІНКИ ==============
# факторна взаємодія
linearHypothesis(model_inter, c("seasonЗима = 0", "seasonЛіто = 0", "seasonОсінь = 0","runtime:seasonЗима = 0", "runtime:seasonЛіто = 0", "runtime:seasonОсінь = 0"),
                 vcov = hccm(model_inter, type = "hc1"))

linearHypothesis(model_inter, c("seasonЗима = 0", "runtime:seasonЗима = 0"),
                 vcov = hccm(model_inter, type = "hc1"))

linearHypothesis(model_inter, c("seasonЛіто = 0", "runtime:seasonЛіто = 0"),
                 vcov = hccm(model_inter, type = "hc1"))

linearHypothesis(model_inter, c("seasonОсінь = 0", "runtime:seasonОсінь = 0"),
                 vcov = hccm(model_inter, type = "hc1"))



# базова
linearHypothesis(model_base, c("seasonЗима = 0", "seasonЛіто = 0", "seasonОсінь = 0"),
                 vcov = hccm(model_base, type = "hc1"))


# степені
linearHypothesis(model2, c("runtime = 0", "I(runtime^2) = 0"),
                 vcov = hccm(model2, type = "hc1"))

# log
linearHypothesis(model_log, c("I(log(runtime)) = 0"),
                 vcov = hccm(model_log, type = "hc1"))

# log^2
linearHypothesis(model_log2, c("I(log(runtime)) = 0", "I(log(runtime)^2) = 0"),
                 vcov = hccm(model_log2, type = "hc1"))

# log^3
linearHypothesis(model_log3, c("I(log(runtime)) = 0", "I(log(runtime)^2) = 0", "I(log(runtime)^3) = 0"),
                 vcov = hccm(model_log3, type = "hc1"))




#========МУЛЬТИКОЛІНЕАРНІСТЬ=======
movies_cor <- movies_cleaned%>%select(-budget, -vote_count_log)

# пірсона
ggcorr(movies_cor, label = TRUE)

# спірмана
ggcorr(movies_cor, method = c("pairwise", "spe"), label = TRUE)
