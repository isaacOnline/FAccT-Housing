### Load Libraries
# Packages
library(lme4)
library(rstanarm)
options(mc.cores = 10)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(MuMIn)
library(loo)


source("analysis/data_loading.R")

assessment_fmla <- "~ prev_assessed_value * days_since_prev_assessment + race_desc_seller* buyer_full_entitity_type"

house_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(house_features, collapse = "`+`"), "`"
)

nbhd_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood, collapse = "`+`"), "`"
)

demo_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood_demographics, collapse = "`+`"), "`"
)

house_nbhd_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood, collapse = "`+`"), "`",
  "+ `", paste(house_features, collapse = "`+`"), "`"
)

house_demo_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood_demographics, collapse = "`+`"), "`",
  "+ `", paste(house_features, collapse = "`+`"), "`"
)

nbhd_demo_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood, collapse = "`+`"), "`",
  "+ `", paste(neighborhood_demographics, collapse = "`+`"), "`"
)

nbhd_demo_house_fmla <- paste0(
  "~ as.factor(sale_year) + race_desc_seller* buyer_full_entitity_type",
  "+ `", paste(neighborhood, collapse = "`+`"), "`",
  "+ `", paste(house_features, collapse = "`+`"), "`",
  "+ `", paste(neighborhood_demographics, collapse = "`+`"), "`"
)

fit_model <- function(data, fmla, model_function, save_path) {
  # Delete file if script killed
  on.exit(file.remove(save_path))

  fitted_model <- model_function(
    as.formula(fmla),
    family = gaussian(link = "identity"),
    seed = 316563, iter = 10000, chains = 10, data = data
  )

  return(
    list(
      "model" = fitted_model,
      "model_r2" = bayes_R2(fitted_model),
      "model_waic" = waic(fitted_model)
      # 'model_loo' = loo(fitted_model)
    )
  )
}

models_to_fit <- list(
  # c(nbhd_demo_fmla, 'neighborhood_with_demographics'),
  # c(house_demo_fmla, 'house_with_demographics'),
  c(nbhd_demo_house_fmla, "house_and_neighborhood_with_demographics")
  # c(demo_fmla, 'demographics_only'),
  # c(assessment_fmla, 'assessment')
  # c(house_nbhd_fmla, 'house_and_neighborhood'),
  # c(nbhd_fmla, 'neighborhood_only'),
  # c(house_fmla, 'house_only')
)

model_types <- list(
  c(stan_glm, "", "fe"),
  c(stan_glmer, "+ (1 | npa_id)", "re")
)

targets <- list(
  # c('sale_price','sp'),
  # c('profit','profit'),
  # c('log(sale_price)','log_sp'),
  c("sqrt(sale_price)", "sqrt_sp"),
  c("nth_rt(sale_price, 3)", "cbrt_sp"),
  c("nth_rt(sale_price, 4)", "fourthrt_sp"),
  c("nth_rt(sale_price, 5)", "fifthrt_sp")
  # c('log(profit)','log_profit')
)



data_subsets <- list(
  list("sold_by_human", ds1)
  # list('sold_to_human', ds2)
)

Sys.sleep(runif(1, 0, 10))


for (mt in model_types) {
  model_type <- mt[[1]]
  additional_term <- mt[[2]]
  name_of_model_type <- mt[[3]]

  for (tg in targets) {
    target <- tg[[1]]
    target_name <- tg[[2]]

    for (md in models_to_fit) {
      fmla <- md[[1]]
      model_name <- md[[2]]

      for (ds in data_subsets) {
        data <- ds[[2]]
        data_subset_name <- ds[[1]]

        save_name <- paste(data_subset_name, model_name, name_of_model_type, target_name, sep = "_")
        save_path <- paste0("analysis/models/", save_name, ".RDS")

        # Check if model exists
        if (file.exists(save_path)) {
          next()
        }

        print(save_path)

        # Create empty file in case other instances of this script are running
        file.create(save_path)


        if (target == "profit") {
          fmla_to_use <- paste0(fmla, " + sale_price * days_until_next_sale")
        } else {
          fmla_to_use <- fmla
        }

        if (data_subset_name == "sold_to_human") {
          fmla_to_use <- gsub("buyer_full_entitity_type", "seller_full_entitity_type", fmla_to_use)
          fmla_to_use <- gsub("race_desc_seller", "race_desc_buyer", fmla_to_use)
        }


        fitted_model <- fit_model(
          data = data,
          fmla = paste0(target, fmla_to_use, additional_term),
          model_function = model_type,
          save_path = save_path
        )

        saveRDS(
          file = save_path,
          object = fitted_model
        )
      }
    }
  }
}
