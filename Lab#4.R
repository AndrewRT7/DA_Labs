setwd("D://UNI//da//lab 4")
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(dplyr)
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
    release_after_2010 = ifelse(movies_cleaned$release_year <= 2010, 0, 1)
  )

movies <- movies_cleaned





set.seed(123) 

n <- nrow(movies)
train_sample_index <- sample(1:n, size = 0.7 * n)

train_sample_data <- movies[train_sample_index, ]
test_sample_data <- movies[-train_sample_index, ]


x_sample_grid <- seq(min(train_sample_data$runtime), max(train_sample_data$runtime), length.out = 200)




bw_sample_lc <- npregbw(vote_average ~ runtime + factor(season) ,
                        data = train_sample_data, regtype = "lc",
                        bwtype = "adaptive", 
                        bwmethod = "cv.ls")
model_sample_np_lc <- npreg(bw_sample_lc)

bw_sample_ll <- npregbw(vote_average ~ runtime + factor(season),
                        data = train_sample_data, regtype = "ll",
                        bwtype = "adaptive", 
                        bwmethod = "cv.ls")
model_sample_np_lc <- npreg(bw_sample_ll)

mode_sample_season <- names(sort(table(test_sample_data$season), decreasing = TRUE))[1]

movies_sample_runtime <- data.frame(
  runtime = x_sample_grid,
  season = factor(mode_sample_season, levels = levels(train_sample_data$season))
)



fitted_sample_runtime_lc <- predict(model_sample_np_lc, newdata = movies_sample_runtime, se.fit = TRUE)
fitted_sample_runtime_ll <- predict(model_sample_np_ll, newdata = movies_sample_runtime, se.fit = TRUE)


movies_sample_runtime$fit_lc <- fitted_sample_runtime_lc$fit
movies_sample_runtime$se_lc <- fitted_sample_runtime_lc$se.fit

conf.level <- 0.95
z <- qnorm(1 - (1 - conf.level)/2)

movies_sample_runtime$upper_lc <- movies_sample_runtime$fit_lc + z * movies_sample_runtime$se_lc
movies_sample_runtime$lower_lc <- movies_sample_runtime$fit_lc - z * movies_sample_runtime$se_lc


movies_sample_runtime$fit_ll <- fitted_sample_runtime_ll$fit
movies_sample_runtime$se_ll <- fitted_sample_runtime_ll$se.fit

movies_sample_runtime$upper_ll <- movies_sample_runtime$fit_ll + z * movies_sample_runtime$se_ll
movies_sample_runtime$lower_ll <- movies_sample_runtime$fit_ll - z * movies_sample_runtime$se_ll



ggplot(movies_sample_runtime, aes(x = runtime)) +
  
  # Локальна константна регресія (lc)
  geom_line(aes(y = fit_lc, color = "Надараї-Вотсона"), size = 1.2) +
  geom_ribbon(aes(ymin = lower_lc, ymax = upper_lc), fill = "deepskyblue3", alpha = 0.3) +
  
  # Локальна лінійна регресія (ll)
  geom_line(aes(y = fit_ll, color = "Локальна лінійна регресія"), size = 1.2) +
  geom_ribbon(aes(ymin = lower_ll, ymax = upper_ll), fill = "darkorange", alpha = 0.3) +
  
  scale_color_manual(name = "Метод", values = c(
    "Надараї-Вотсона" = "deepskyblue3",
    "Локальна лінійна регресія" = "darkorange"
  )) +
  
  labs(
    x = "Runtime",
    y = "Прогноз vote_average"
  ) +
  coord_cartesian(ylim = c(0, 10)) +
  theme_minimal()







#=============PCA============

movies_tibble <- as_tibble(movies_cleaned)
movies_tibble <- movies_tibble %>%
  mutate(
    log_runtime = log(movies$runtime),
    log_budget = log(movies$budget)
  ) 

movies_numeric <- movies_tibble %>%
  dplyr::select(where(is.numeric))%>%
  dplyr::select(-id, -popularity) %>%
  drop_na()

movies_tibble_pca <- PCA(movies_numeric, graph = FALSE)

fviz_screeplot(movies_tibble_pca, addlabels = TRUE)

fviz_pca_var(movies_tibble_pca,repel = TRUE)   
fviz_pca_var(movies_tibble_pca, axes = c(1, 3), repel = TRUE)

fviz_cos2(movies_tibble_pca, choice = "var", axes = 1)
fviz_cos2(movies_tibble_pca, choice = "var", axes = 2)
