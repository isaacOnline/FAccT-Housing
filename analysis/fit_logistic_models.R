library(dplyr)
library(rstanarm)
options(mc.cores = 20)


source("analysis/data_loading.R")

# Create formulas for models
assessment_fmla <- "sold_to_institutional ~ prev_assessed_value * days_since_prev_assessment + race_desc_seller * ib_intermediary + (1|npa_id)"
nbhd_demo_house_fmla <- paste0(
  "sold_to_institutional ~ as.factor(sale_year) + race_desc_seller* ib_intermediary",
  "+ `", paste(neighborhood, collapse = "`+`"), "`",
  "+ `", paste(house_features, collapse = "`+`"), "`",
  "+ `", paste(neighborhood_demographics, collapse = "`+`"), "`",
  "+ (1|npa_id)"
)

# Fit housing/neighborhood model
fitted_model = stan_glmer(nbhd_demo_house_fmla, data=resales,
          family=binomial,
        seed = 31656, iter=5000, chains=20,init = 0
)

# Create list of model artifacts
fitted_model = list(
  'model' = fitted_model,
  'fmla' = assessment_fmla,
  'r2' = bayes_R2(fitted_model),
  'waic' = waic(fitted_model)
)

# Save model
save_path = paste0('analysis/models/', 'logistic_housing', '.RDS')
saveRDS(file = save_path,
        object = fitted_model)


# Fit assessment model
fitted_model <- stan_glmer(assessment_fmla,
  data = resales,
  family = binomial,
  seed = 316563, iter = 5000, chains = 20
)

# Create list of model artifacts
fitted_model <- list(
  "model" = fitted_model,
  "fmla" = assessment_fmla,
  "r2" = bayes_R2(fitted_model),
  "waic" = waic(fitted_model)
)

# Save model
save_path <- paste0("analysis/models/", "logistic_assessment", ".RDS")
saveRDS(
  file = save_path,
  object = fitted_model
)
