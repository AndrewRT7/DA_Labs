library(tidyverse)
library(boot)


df <- read_csv("TMDB_movie_dataset_v11.csv")

df_clean <- df %>%
  mutate(
    vote_average = if_else(vote_average <= 0, NA_real_, vote_average),
    vote_count = if_else(vote_count < 0, NA_integer_, vote_count),
    revenue = if_else(revenue < 0, NA_real_, revenue),
    runtime = if_else(runtime <= 0, NA_real_, runtime),
    genres = na_if(genres, ""),
    budget = if_else(budget <= 0, NA_real_, budget),
    status = factor(status),
    original_language = factor(original_language),
    genres_split = str_split(genres, ", "),
    release_date = as.Date(gsub("\\s+", "", gsub('"', '', release_date))),
    release_month = month(release_date, label = TRUE, abbr = TRUE),
    release_year = year(release_date),
    release_month_ua = recode(release_month,
                              Jan = "Січ", Feb = "Лют", Mar = "Бер",
                              Apr = "Кві", May = "Тра", Jun = "Чер",
                              Jul = "Лип", Aug = "Сер", Sep = "Вер",
                              Oct = "Жов", Nov = "Лис", Dec = "Гру"),
    season = case_when(
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Зима",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Весна",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Літо",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Осінь",
      TRUE ~ NA_character_
    )
  )


df_clean <- df_clean %>%
  filter(revenue > 0, season != "Unknown", !id %in% c(1270893, 1224207)) 



bootstrap_sample <- function(data, n_boot = 1000) {
  boot_means <- replicate(n_boot, mean(sample(data, replace = TRUE)))
  return(boot_means)
}


boot_revenue <- bootstrap_sample(df_clean$revenue)


mean_revenue <- mean(df_clean$revenue, na.rm = TRUE)

ci_lower <- quantile(boot_revenue, 0.025)  
ci_upper <- quantile(boot_revenue, 0.975)  

print(paste("Середнє значення доходу:", mean_revenue))
print(paste("Довірчий інтервал для доходу: [", ci_lower, ", ", ci_upper, "]"))

ci_data <- tibble(
  mean_revenue = mean_revenue,
  ci_lower = ci_lower,
  ci_upper = ci_upper
)


ggplot(ci_data, aes(x = 1, y = mean_revenue)) +
  geom_point(color = "blue", size = 3) +  
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1) +  # Лінії довірчого інтервалу
  labs(
    x = "Середній дохід",
    y = "Доходи",
    title = "Довірчий інтервал для середнього доходу"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())






df_clean <- df_clean %>%
  filter(revenue > 0, season != "Unknown", !id %in% c(1270893, 1224207))  


bootstrap_sample <- function(data, n_boot = 1000) {
  boot_means <- replicate(n_boot, mean(sample(data, replace = TRUE)))
  return(boot_means)
}


ci_revenue_season_bootstrap <- df_clean %>%
  group_by(season) %>%
  summarize(
    mean_revenue = mean(revenue, na.rm = TRUE),
    n = n(),
    boot_revenue = list(bootstrap_sample(revenue, n_boot = 1000)),
    .groups = "drop"
  )


ci_revenue_season_bootstrap <- ci_revenue_season_bootstrap %>%
  mutate(
    ci_lower = sapply(boot_revenue, function(x) quantile(x, 0.025)),  
    ci_upper = sapply(boot_revenue, function(x) quantile(x, 0.975))   
  )


print(ci_revenue_season_bootstrap)

ggplot(ci_revenue_season_bootstrap, aes(x = season, y = mean_revenue)) +
  geom_point(color = "black") +  # Точки середнього
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Лінії довірчого інтервалу
  theme_minimal() +
  labs(x = "Сезон", y = "Середнє значення доходу") +
  ggtitle("Довірчі інтервали для середнього доходу по сезонах (бутстреп)") 



















mean_spring_revenue <- df_clean %>%
  filter(season == "Весна") %>%
  summarize(mean_revenue = mean(revenue, na.rm = TRUE)) %>%
  pull(mean_revenue)

mean_all_revenue <- df_clean %>%
  summarize(mean_revenue = mean(revenue, na.rm = TRUE)) %>%
  pull(mean_revenue)

t_test_result <- t.test(
  df_clean$revenue[df_clean$season == "Весна"],  
  mu = mean_all_revenue  
)

result_table <- tibble(
  Mean = mean_spring_revenue,  
  SD = sd(df_clean$revenue[df_clean$season == "Весна"], na.rm = TRUE),  
  N = sum(df_clean$season == "Весна", na.rm = TRUE),  
  SE = t_test_result$stderr, 
  Wald_statistic = t_test_result$statistic,  
  P_value = t_test_result$p.value,  
  Conclusion = if_else(t_test_result$p.value < 0.05, "Reject H0", "Do not reject H0")  
)

print(result_table)






ci_genre_count_season <- df_clean %>%
  unnest(genres_split) %>%  
  group_by(season, genres_split) %>%  
  summarize(
    genre_count = n(),  
    .groups = "drop"
  )


ci_genre_count_season_summary <- ci_genre_count_season %>%
  group_by(season) %>%
  summarize(
    mean_genre_count = mean(genre_count),
    sd_genre_count = sd(genre_count),
    n = n(),
    ci_lower = mean_genre_count - qt(0.975, df = n - 1) * sd_genre_count / sqrt(n),
    ci_upper = mean_genre_count + qt(0.975, df = n - 1) * sd_genre_count / sqrt(n)
  )


print(ci_genre_count_season_summary)


ggplot(ci_genre_count_season_summary, aes(x = season, y = mean_genre_count)) +
  geom_point(color = "black") +  
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Лінії довірчого інтервалу
  theme_minimal() +
  labs(x = "Сезон", y = "Середнє значення кількості фільмів") +
  ggtitle("Довірчі інтервали для кількості фільмів по сезонах")







library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


df_clean <- df_clean %>%
  mutate(genres_split = str_split(genres, ", ")) %>%
  filter(!is.na(revenue))


genres_to_test <- c("Action", "Comedy", "Drama", "Family", "Horror", "Romance")


genre_revenue_tbl <- map_dfr(genres_to_test, function(genre) {
  genre_movies <- df_clean %>% filter(map_lgl(genres_split, ~ genre %in% .x))
  
  tibble(
    genre = genre,
    revenue_winter = list(genre_movies %>% filter(season == "Зима") %>% pull(revenue)),
    revenue_other = list(genre_movies %>% filter(season != "Зима") %>% pull(revenue))
  )
})


genre_revenue_tbl <- genre_revenue_tbl %>%
  filter(lengths(revenue_winter) > 1 & lengths(revenue_other) > 1)


ttests <- genre_revenue_tbl %>%
  rowwise() %>%
  mutate(p_value = t.test(revenue_winter, revenue_other)$p.value) %>%
  ungroup() %>%
  mutate(
    p_value_bonf = p.adjust(p_value, method = "bonferroni"),
    p_value_BH = p.adjust(p_value, method = "BH"),
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(-revenue_winter, -revenue_other)

print(ttests)






df_clean <- df_clean %>%
  filter(revenue > 0, season != "Unknown", !id %in% c(1270893, 1224207))  # Фільтруємо дані


bootstrap_sample_variance <- function(data, n_boot = 1000) {
  boot_vars <- replicate(n_boot, var(sample(data, replace = TRUE)))
  return(boot_vars)
}


ci_variance_season_bootstrap <- df_clean %>%
  group_by(season) %>%
  summarize(
    variance_revenue = var(revenue, na.rm = TRUE),
    n = n(),
    boot_variance = list(bootstrap_sample_variance(revenue, n_boot = 1000)),
    .groups = "drop"
  )


ci_variance_season_bootstrap <- ci_variance_season_bootstrap %>%
  mutate(
    ci_lower = sapply(boot_variance, function(x) quantile(x, 0.025)),  
    ci_upper = sapply(boot_variance, function(x) quantile(x, 0.975))   
  )


print(ci_variance_season_bootstrap)


ggplot(ci_variance_season_bootstrap, aes(x = season, y = variance_revenue)) +
  geom_point(color = "black") +  # Точки середнього
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  
  theme_minimal() +
  labs(x = "Сезон", y = "Дисперсія доходу") +
  ggtitle("Бутстреп-довірчі інтервали для дисперсії доходу по сезонах") 





bootstrap_sample_median <- function(data, n_boot = 1000) {
  boot_medians <- replicate(n_boot, median(sample(data, replace = TRUE)))
  return(boot_medians)
}


ci_median_season_bootstrap <- df_clean %>%
  group_by(season) %>%
  summarize(
    median_revenue = median(revenue, na.rm = TRUE),
    n = n(),
    boot_median = list(bootstrap_sample_median(revenue, n_boot = 1000)),
    .groups = "drop"
  )


ci_median_season_bootstrap <- ci_median_season_bootstrap %>%
  mutate(
    ci_lower = sapply(boot_median, function(x) quantile(x, 0.025)),  
    ci_upper = sapply(boot_median, function(x) quantile(x, 0.975))   
  )


print(ci_median_season_bootstrap)


ggplot(ci_median_season_bootstrap, aes(x = season, y = median_revenue)) +
  geom_point(color = "black") +  
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  
  theme_minimal() +
  labs(x = "Сезон", y = "Медіана доходу") +
  ggtitle("Бутстреп-довірчі інтервали для медіан доходу по сезонах")

