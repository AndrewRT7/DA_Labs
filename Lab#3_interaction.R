library(tidyverse)

movies_initial <- read_csv("TMDB_movie_dataset_v11.2.csv")

month_translation <- c(
  Jan = "Січ", Feb = "Лют", Mar = "Бер", Apr = "Кві", May = "Тра", Jun = "Чер",
  Jul = "Лип", Aug = "Сер", Sep = "Вер", Oct = "Жов", Nov = "Лис", Dec = "Гру")

untitled_translations <- c("untitled", "без назви", "بدون عنوان", "Без названия", 
                           "Senza Titolo", "Bez názvu", "Ohne Titel", "無題",
                           "Sin título", "Isimsiz", "Uten tittel")

revenue_to_na <- c(1407985, 1270893, 1224207, 1326885, 1294302, 1236552)
budget_to_na <- c(1381066, 1235037, 1224207, 1057999, 1022208, 
                  1201764, 1399448, 1426913, 1449031, 1320160, 
                  1453767, 1453985, 1414861, 1398923, 1365277,
                  1417006, 1441191, 1450893, 1450893, 1301115, 1228885,
                  1272552, 1229118, 1294480, 622311, 1369796, 1108211)
runtime_to_na <- c(206026, 368247, 717019, 206026, 392372, 454409, 500980,
                   544686, 732330, 66871, 633832, 671214, 531640, 535892,
                   523167, 631038, 698754, 685310, 651033, 125120, 298752)

movies_clean <- movies_initial %>%
  
  #Numeric columns
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count = if_else(vote_count <= 0, NA_integer_, vote_count),
    revenue = if_else(revenue < 0, NA_real_, revenue),
    runtime = if_else(runtime <= 0, NA_real_, runtime),
    budget = if_else(budget < 10, NA_real_, budget)) %>%
  
  #Categorical variables
  mutate(  
    status = factor(status),
    original_language = factor(original_language)) %>%
  
  #Text columns
  mutate(
    production_companies = na_if(str_trim(production_companies), ""),
    keywords = na_if(str_trim(keywords), ""),
    genres = na_if(str_trim(genres), "")) %>%
  
  #Untitled rows
  mutate(
    title = if_else(str_to_lower(title) %in% untitled_translations, NA_character_, title),
    original_title = if_else(str_to_lower(original_title) %in% untitled_translations, NA_character_, original_title)) %>%
  
  #Date  
  mutate(  
    release_date = gsub('"', '', release_date),
    release_date = gsub("\\s+", "", release_date),
    release_date = as.Date(release_date),
    release_year = year(release_date),
    release_month = month(release_date),
    release_day = day(release_date),
    release_month_ua = month_translation[as.character(month(release_date, 
                                                            label = TRUE, abbr = TRUE))]) %>%
  
  #Date labels
  mutate(
    season = case_when(
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_)) %>%
  
  #Manual error correction
  mutate(
    revenue = if_else(release_date > Sys.Date(), NA_real_, revenue)) %>%
  
  mutate(
    revenue = if_else(id %in% revenue_to_na, NA_real_, revenue),
    budget  = if_else(id %in% budget_to_na, NA_real_, budget),
    runtime = if_else(id %in% runtime_to_na, NA_real_, runtime)) %>%
  
  #Unnecessary columns removal
  select(-backdrop_path, -homepage, -popularity, -poster_path, -imdb_id)



#STEP 2
library(broom)

vars_used <- c("budget", "vote_count", "vote_average", "runtime", "season")

movies_clean$decade <- floor(movies_clean$release_year / 10) * 10
df_clean <- movies_clean[complete.cases(movies_clean[, vars_used]), ]
df_clean$season <- as.factor(df_clean$season)
df_clean$decade <- as.factor(df_clean$decade)

model1 <- lm(vote_average ~ runtime + season, data = df_clean)
tidy(model1, conf.int = TRUE)
summary(model1)

model2 <- lm(vote_average ~ runtime + season + vote_count, data = df_clean)
tidy(model2, conf.int = TRUE)
summary(model2)

model3 <- lm(vote_average ~ runtime + season + vote_count + I(log(budget)), data = df_clean)
tidy(model3, conf.int = TRUE)
summary(model3)

model4 <- lm(vote_average ~ runtime + season + vote_count + I(log(budget)) + decade, data = df_clean)
tidy(model4, conf.int = TRUE)
summary(model4)



#STEP 3
ggplot(df_clean, aes(x = vote_count, y = vote_average)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE, color = "red") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14))

df_clean$vote_count2 <- df_clean$vote_count^2

model_poly <- lm(vote_average ~ runtime + season + vote_count + vote_count2, data = df_clean)
summary(model_poly)

model_interact_season <- lm(vote_average ~ runtime + season + vote_count:season, data = df_clean)
summary(model_interact_season)

df_clean$log_vote_count <- log1p(df_clean$vote_count)
model_log <- lm(vote_average ~ runtime + season + log_vote_count, data = df_clean)
summary(model_log)



#STEP 4
library(corrplot)
library(lmtest)
library(sandwich)
library(car)
library(stargazer)

stargazer(model1, type = "text",
          digits = 3, 
          out = "model1_summary.txt")
stargazer(model2, type = "text",
          digits = 3, 
          out = "model2_summary.txt")
stargazer(model3, type = "text",
          digits = 3, 
          out = "model3_summary.txt")
stargazer(model4, type = "text",
          digits = 3, 
          out = "model4_summary.txt")

stargazer(model_poly, type = "text",
          digits = 3, 
          out = "model_poly_summary.txt")
stargazer(model_interact_season, type = "text",
          digits = 3, 
          out = "model_interact_season_summary.txt")
stargazer(model_log, type = "text",
          digits = 3, 
          out = "model_log_summary.txt")



#STEP 5
sum(df_clean$budget < 500000, na.rm = TRUE)
mean(df_clean$budget < 500000, na.rm = TRUE) * 100

df_interaction <- df_clean[df_clean$budget > 0, ]
df_interaction$log_budget <- log(df_interaction$budget)
df_interaction$low_budget <- ifelse(df_interaction$budget < 500000, 1, 0)

model_interaction <- lm(vote_average ~ runtime * low_budget, data = df_interaction)

summary(model_interaction)
tidy(model_interaction, conf.int = TRUE)

library(car)
library(lmtest)

model_interaction_hc1 <- coeftest(model_interaction, vcov. = hccm(model_interaction, type = "hc1"))

library(stargazer)
stargazer(model_interaction,
          type = "text",
          se = list(model_interaction_hc1[, 2]),
          omit.stat = c("rsq", "f", "ser"),
          digits = 3)

linearHypothesis(model_interaction, "runtime:low_budget = 0", 
                 vcov = vcovHC(model_interaction, type = "HC1"))

linearHypothesis(model_interaction, 
                 c("low_budget = 0", "runtime:low_budget = 0"), 
                 vcov = vcovHC(model_interaction, type = "HC1"))
