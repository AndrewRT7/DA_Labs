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
      release_month_ua %in% c("Січ", "Лют", "Бер") ~ "Winter",
      release_month_ua %in% c("Кві", "Тра", "Чер") ~ "Spring",
      release_month_ua %in% c("Лип", "Сер", "Вер") ~ "Summer",
      release_month_ua %in% c("Жов", "Лис", "Гру") ~ "Autumn",
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

df_clean <- df_clean[, c("vote_average", "runtime", "season", "vote_count",
                                     "release_year", "budget", "revenue")]

df_clean <- df_clean %>%
  filter(!is.na(vote_average), !is.na(runtime), !is.na(season), 
         !is.na(vote_count), !is.na(budget), !is.na(release_year), !is.na(revenue))

df_clean$season <- factor(df_clean$season,
                          levels = c("Winter", "Spring", "Summer", "Autumn"))

# STEP 2

set.seed(123)
train_indices <- sample(1:nrow(df_clean), size = 0.1 * nrow(df_clean))
train_df <- df_clean[train_indices, ]
test_df  <- df_clean[-train_indices, ]

bw_nw_bivar     <- npregbw(formula = vote_average ~ runtime + season, data = train_df, regtype = "lc")
bw_locall_bivar <- npregbw(formula = vote_average ~ runtime + season, data = train_df, regtype = "ll")

model_nw_bivar     <- npreg(bws = bw_nw_bivar)
model_locall_bivar <- npreg(bws = bw_locall_bivar)

budget_grid <- seq(
  from = quantile(df_clean$budget, 0.01, na.rm = TRUE),
  to   = quantile(df_clean$budget, 0.99, na.rm = TRUE),
  length.out = 200
)

season_fixed <- "Autumn"

grid_df <- data.frame(
  runtime = rep(runtime_fixed, length(budget_grid)),
  budget  = budget_grid
)

pred_nw_bivar     <- predict(model_nw_bivar,     newdata = grid_df, se = TRUE)
pred_locall_bivar <- predict(model_locall_bivar, newdata = grid_df, se = TRUE)

df_plot_nw_bivar <- data.frame(
  budget = budget_grid,
  fit    = pred_nw_bivar$fit,
  se     = pred_nw_bivar$se
)
df_plot_nw_bivar$lower <- df_plot_nw_bivar$fit - 1.96 * df_plot_nw_bivar$se
df_plot_nw_bivar$upper <- df_plot_nw_bivar$fit + 1.96 * df_plot_nw_bivar$se

df_plot_ll_bivar <- data.frame(
  budget = budget_grid,
  fit    = pred_locall_bivar$fit,
  se     = pred_locall_bivar$se
)
df_plot_ll_bivar$lower <- df_plot_ll_bivar$fit - 1.96 * df_plot_ll_bivar$se
df_plot_ll_bivar$upper <- df_plot_ll_bivar$fit + 1.96 * df_plot_ll_bivar$se

ggplot(df_plot_nw_bivar, aes(x = budget, y = fit)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "lightgreen") +
  labs(
    title = paste0("Надар’я–Вотсон: vote_average ~ budget | runtime = ", runtime_fixed, " хв"),
    x     = "Бюджет (budget)",
    y     = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
  )

ggplot(df_plot_ll_bivar, aes(x = budget, y = fit)) +
  geom_line(color = "darkorange", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "moccasin") +
  labs(
    title = paste0("Локальна лінійна: vote_average ~ budget | runtime = ", runtime_fixed, " хв"),
    x     = "Бюджет (budget)",
    y     = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# STEP 3

bw_nw_triple     <- npregbw(formula = vote_average ~ runtime + budget + season,
                            data = train_df,
                            regtype = "lc")
bw_locall_triple <- npregbw(formula = vote_average ~ runtime + budget + season,
                            data = train_df,
                            regtype = "ll")

model_nw_triple     <- npreg(bws = bw_nw_triple)
model_locall_triple <- npreg(bws = bw_locall_triple)

budget_grid <- seq(
  from = quantile(df_clean$budget, 0.01, na.rm = TRUE),
  to   = quantile(df_clean$budget, 0.99, na.rm = TRUE),
  length.out = 200
)

runtime_fixed <- 90
season_fixed  <- "Autumn"

grid_df <- data.frame(
  runtime = rep(runtime_fixed, length(budget_grid)),
  budget  = budget_grid,
  season  = factor(rep(season_fixed, length(budget_grid)),
                   levels = levels(df_clean$season))
)

pred_nw_triple     <- predict(model_nw_triple,     newdata = grid_df, se = TRUE)
pred_locall_triple <- predict(model_locall_triple, newdata = grid_df, se = TRUE)

df_plot_nw_triple <- data.frame(
  budget = budget_grid,
  fit    = pred_nw_triple$fit,
  se     = pred_nw_triple$se
)
df_plot_nw_triple$lower <- df_plot_nw_triple$fit - 1.96 * df_plot_nw_triple$se
df_plot_nw_triple$upper <- df_plot_nw_triple$fit + 1.96 * df_plot_nw_triple$se

df_plot_ll_triple <- data.frame(
  budget = budget_grid,
  fit    = pred_locall_triple$fit,
  se     = pred_locall_triple$se
)
df_plot_ll_triple$lower <- df_plot_ll_triple$fit - 1.96 * df_plot_ll_triple$se
df_plot_ll_triple$upper <- df_plot_ll_triple$fit + 1.96 * df_plot_ll_triple$se

ggplot(df_plot_nw_triple, aes(x = budget, y = fit)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.2) +
  labs(
    title = paste0(
      "Надар’я–Вотсон: vote_average ~ runtime = ",
      " season = ", season_fixed
    ),
    x = "Runtime",
    y = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
  )

ggplot(df_plot_ll_triple, aes(x = budget, y = fit)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "pink", alpha = 0.2) +
  labs(
    title = paste0(
      "Локальна лінійна: vote_average ~ runtime",
      " season = ", season_fixed
    ),
    x = "Runtime",
    y = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5)
  )

# STEP 4

lm_linear <- lm(vote_average ~ season + release_year, data = train_df)

train_df$lin_pred  <- predict(lm_linear, newdata = train_df)
train_df$residuals <- train_df$vote_average - train_df$lin_pred

bw_nw_pl <- npregbw(
  formula = residuals ~ runtime + budget,
  data    = train_df,
  regtype = "lc"
)
bw_ll_pl <- npregbw(
  formula = residuals ~ runtime + budget,
  data    = train_df,
  regtype = "ll"
)

model_nw_pl <- npreg(bws = bw_nw_pl)
model_ll_pl <- npreg(bws = bw_ll_pl)

budget_grid <- seq(
  from       = quantile(df_clean$budget, 0.01, na.rm = TRUE),
  to         = quantile(df_clean$budget, 0.99, na.rm = TRUE),
  length.out = 200
)

runtime_fixed <- 90

baseline_season    <- "Autumn"
baseline_year      <- median(train_df$release_year, na.rm = TRUE)

linear_offset <- predict(
  lm_linear,
  newdata = data.frame(
    season       = factor(baseline_season, levels = levels(train_df$season)),
    release_year = baseline_year
  )
)

grid_df <- data.frame(
  runtime = rep(runtime_fixed, length(budget_grid)),
  budget  = budget_grid
)

pred_resid_nw <- predict(model_nw_pl, newdata = grid_df, se = TRUE)
pred_resid_ll <- predict(model_ll_pl, newdata = grid_df, se = TRUE)

df_plot_nw_pl <- data.frame(
  budget = budget_grid,
  fit    = linear_offset + pred_resid_nw$fit,
  se     = pred_resid_nw$se
)
df_plot_nw_pl$lower <- df_plot_nw_pl$fit - 1.96 * df_plot_nw_pl$se
df_plot_nw_pl$upper <- df_plot_nw_pl$fit + 1.96 * df_plot_nw_pl$se

df_plot_ll_pl <- data.frame(
  budget = budget_grid,
  fit    = linear_offset + pred_resid_ll$fit,
  se     = pred_resid_ll$se
)
df_plot_ll_pl$lower <- df_plot_ll_pl$fit - 1.96 * df_plot_ll_pl$se
df_plot_ll_pl$upper <- df_plot_ll_pl$fit + 1.96 * df_plot_ll_pl$se

ggplot(df_plot_nw_pl, aes(x = budget, y = fit)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2, fill = "lightgreen") +
  labs(
    title = paste0("Partial Linear (NW):  vote_average ~ [runtime,budget]_np  + [season,year]_lin\n",
                   "runtime = ", runtime_fixed, "  |  season = ", baseline_season,
                   "  |  release_year = ", baseline_year),
    x = "Budget",
    y = "Predicted vote_average"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 11),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5)
  )


ggplot(df_plot_ll_pl, aes(x = budget, y = fit)) +
  geom_line(color = "darkorange", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.2, fill = "moccasin") +
  labs(
    title = paste0("Partial Linear (LL):  vote_average ~ [runtime,budget]_np  + [season,year]_lin\n",
                   "runtime = ", runtime_fixed, "  |  season = ", baseline_season,
                   "  |  release_year = ", baseline_year),
    x = "Budget",
    y = "Predicted vote_average"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 11),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# STEP 5

set.seed(123)
train_indices <- sample(1:nrow(df_clean), size = 0.05 * nrow(df_clean))
train_df <- df_clean[train_indices, ]
test_df  <- df_clean[-train_indices, ]

bw_nw_full <- npregbw(
  formula = vote_average ~ runtime + season + vote_count + revenue + budget,
  data = train_df,
  regtype = "lc",
  bwtype = "adaptive",
  bwmethod = "cv.ls"
)

bw_ll_full <- npregbw(
  formula = vote_average ~ runtime + season + vote_count + revenue + budget,
  data = train_df,
  regtype = "ll",
  bwtype = "adaptive",
  bwmethod = "cv.ls"
)

model_nw_full <- npreg(bws = bw_nw_full)
model_ll_full <- npreg(bws = bw_ll_full)

runtime_grid <- seq(
  from = quantile(train_df$runtime, 0.01, na.rm = TRUE),
  to   = quantile(train_df$runtime, 0.99, na.rm = TRUE),
  length.out = 200
)

train_df$budget <- log1p(train_df$budget)
train_df$revenue <- log1p(train_df$revenue)
train_df$vote_count <- log1p(train_df$vote_count)

budget_fixed     <- median(train_df$budget, na.rm = TRUE)
season_fixed     <- "Autumn"
vote_count_fixed <- median(train_df$vote_count, na.rm = TRUE)
revenue_fixed    <- median(train_df$revenue, na.rm = TRUE)

grid_df <- data.frame(
  runtime    = runtime_grid,
  season     = factor(rep(season_fixed, length(runtime_grid)), levels = levels(train_df$season)),
  vote_count = rep(vote_count_fixed, length(runtime_grid)),
  revenue    = rep(revenue_fixed, length(runtime_grid)),
  budget     = rep(budget_fixed, length(runtime_grid))
)

pred_nw_full <- predict(model_nw_full, newdata = grid_df, se = TRUE)
pred_ll_full <- predict(model_ll_full, newdata = grid_df, se = TRUE)

df_plot_nw_full <- data.frame(
  runtime = runtime_grid,
  fit     = pred_nw_full$fit,
  se      = pred_nw_full$se
)
df_plot_nw_full$lower <- df_plot_nw_full$fit - 1.96 * df_plot_nw_full$se
df_plot_nw_full$upper <- df_plot_nw_full$fit + 1.96 * df_plot_nw_full$se

ggplot(df_plot_nw_full, aes(x = runtime, y = fit)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightgreen", alpha = 0.3) +
  labs(
    title = paste0("Надар’я–Вотсон: vote_average ~ runtime + season + vote_count + revenue + budget\n",
                   "season = ", season_fixed, 
                   ", vote_count = ", round(vote_count_fixed), ", revenue = ", round(revenue_fixed), 
                   ", budget = ", round(budget_fixed)),
    x = "Тривалість (runtime)",
    y = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5)
  )

df_plot_ll_full <- data.frame(
  runtime = runtime_grid,
  fit     = pred_ll_full$fit,
  se      = pred_ll_full$se
)
df_plot_ll_full$lower <- df_plot_ll_full$fit - 1.96 * df_plot_ll_full$se
df_plot_ll_full$upper <- df_plot_ll_full$fit + 1.96 * df_plot_ll_full$se

ggplot(df_plot_ll_full, aes(x = runtime, y = fit)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "pink", alpha = 0.3) +
  labs(
    title = paste0("Локальна лінійна: vote_average ~ runtime + season + vote_count + revenue + budget\n",
                   "season = ", season_fixed, 
                   ", vote_count = ", round(vote_count_fixed), ", revenue = ", round(revenue_fixed),
                   ", budget = ", round(budget_fixed)),
    x = "Тривалість (runtime)",
    y = "Середня оцінка (vote_average)"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5)
  )

df_compare <- rbind(
  data.frame(runtime = runtime_grid, fit = pred_nw_full$fit, se = pred_nw_full$se, method = "NW"),
  data.frame(runtime = runtime_grid, fit = pred_ll_full$fit, se = pred_ll_full$se, method = "LL")
)

df_compare$lower <- df_compare$fit - 1.96 * df_compare$se
df_compare$upper <- df_compare$fit + 1.96 * df_compare$se

ggplot(df_compare, aes(x = runtime, y = fit, color = method, fill = method)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("NW" = "darkgreen", "LL" = "darkred")) +
  scale_fill_manual(values = c("NW" = "lightgreen", "LL" = "pink")) +
  labs(
    title = paste0("Надараї-Вотсона vs Локальна лінійна\n",
                   "season + vote_count + revenue + budget_fixed"),
    x = "Тривалість (runtime)",
    y = "Середня оцінка (vote_average)",
    color = "Метод",
    fill = "Метод"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 12),
    plot.title   = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 12)
  )