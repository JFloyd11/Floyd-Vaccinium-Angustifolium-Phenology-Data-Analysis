# Multicollinearity test of weather data

library(dplyr)
library(tibble)

# Load data ---------------------------------------------------------------

vaccinium <- read.csv("./5 - Running The Models/vaccinium_with_zones_weather_clean.csv") %>%
  dplyr::select(-X)

# Split by phenophase -----------------------------------------------------

bud_data <- vaccinium %>% filter(bud == "Y")
flower_data <- vaccinium %>% filter(flower == "Y")
fruit_data <- vaccinium %>% filter(fruit == "Y")
seed_disperse_data <- vaccinium %>% filter(seed_disperse == "Y")

# Select only the candidate monthly predictors used in each phenophase ----
# (months in observation window + one month prior)

bud_vars <- c("march_temperature", "april_temperature", "may_temperature",
              "june_temperature", "july_temperature")

flower_vars <- c("march_temperature", "april_temperature", "may_temperature",
                 "june_temperature", "july_temperature")

fruit_vars <- c("april_temperature", "may_temperature", "june_temperature",
                "july_temperature", "august_temperature")

seed_vars <- c("may_temperature", "june_temperature", "july_temperature",
               "august_temperature", "sept_temperature")

# Correlation matrices ----------------------------------------------------

cor_mat_bud <- cor(bud_data[, bud_vars], use = "complete.obs", method = "pearson")
cor_mat_flower <- cor(flower_data[, flower_vars], use = "complete.obs", method = "pearson")
cor_mat_fruit <- cor(fruit_data[, fruit_vars], use = "complete.obs", method = "pearson")
cor_mat_seed <- cor(seed_disperse_data[, seed_vars], use = "complete.obs", method = "pearson")

# Convert correlation matrix to tidy table --------------------------------

cor_to_df <- function(cor_mat, phenophase, threshold = 0.7) {
  as.data.frame(as.table(cor_mat)) %>%
    rename(r = Freq) %>%
    mutate(Phenophase = phenophase) %>%
    filter(Var1 != Var2) %>%
    rowwise() %>%
    mutate(pair = paste(sort(c(Var1, Var2)), collapse = "_")) %>%
    ungroup() %>%
    distinct(Phenophase, pair, .keep_all = TRUE) %>%
    dplyr::select(Phenophase, Var1, Var2, r) %>%
    mutate(`|r| > 0.7` = ifelse(abs(r) > threshold, "Y", ""))
}

# Combine tidy tables -----------------------------------------------------

cor_df_all <- bind_rows(
  cor_to_df(cor_mat_bud, "Bud"),
  cor_to_df(cor_mat_flower, "Flower"),
  cor_to_df(cor_mat_fruit, "Fruit pre-veraison"),
  cor_to_df(cor_mat_seed, "Fruit post-veraison")
)

# View results
print(cor_df_all)

# Save tidy correlation table
write.csv(
  cor_df_all,
  "./5 - Running The Models/temperature_correlation_table_by_phenophase.csv",
  row.names = FALSE
)

# Optional: format full matrices for supplement/output --------------------

format_cor_matrix <- function(cor_mat, phenophase, threshold = 0.7, digits = 2) {
  
  mat <- round(cor_mat, digits)
  
  mat_chr <- matrix(
    as.character(mat),
    nrow = nrow(mat),
    dimnames = dimnames(mat)
  )
  
  # blank upper triangle
  mat_chr[upper.tri(mat_chr)] <- ""
  
  # add * for |r| > threshold in lower triangle
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (i > j && abs(mat[i, j]) > threshold) {
        mat_chr[i, j] <- paste0(mat_chr[i, j], "*")
      }
    }
  }
  
  data.frame(
    Phenophase = phenophase,
    Variable = rownames(mat_chr),
    mat_chr,
    row.names = NULL,
    check.names = FALSE
  )
}

cor_tables <- list(
  format_cor_matrix(cor_mat_bud, "Bud"),
  tibble(),
  format_cor_matrix(cor_mat_flower, "Flower"),
  tibble(),
  format_cor_matrix(cor_mat_fruit, "Fruit pre-veraison"),
  tibble(),
  format_cor_matrix(cor_mat_seed, "Fruit post-veraison")
)

cor_combined <- bind_rows(cor_tables)

write.csv(
  cor_combined,
  "./5 - Running The Models/temperature_correlation_matrices_by_phenophase.csv",
  row.names = FALSE,
  na = ""
)

# Optional quick summary --------------------------------------------------

cor_summary <- cor_df_all %>%
  group_by(Phenophase) %>%
  summarise(
    n_pairs = n(),
    max_abs_r = max(abs(r), na.rm = TRUE),
    n_above_0.7 = sum(abs(r) > 0.7, na.rm = TRUE)
  )

print(cor_summary)

write.csv(
  cor_summary,
  "./5 - Running The Models/temperature_correlation_summary_by_phenophase.csv",
  row.names = FALSE
)