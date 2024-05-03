library(rstanarm)
library(bayesplot)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)

source("analysis/data_loading.R")

get_central <- function(aggregator, model_type) {
  # Get the "Typical" home in dataset, with identities of buyer/seller changed

  if (model_type == "resales") {
    dataset <- resales
  } else if (model_type == "sold_by_human") {
    dataset <- ds1
  } else {
    dataset <- ds2
  }

  # Convert sale year to factor, as we did in model fitting
  dataset$sale_year <- as.factor(dataset$sale_year)

  aggregated <- dataset %>%
    summarize_if(is.numeric, aggregator, na.rm = TRUE) %>%
    as.data.frame()

  # Function for calculating modal value for categorical columns
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  # Get most common value for all categorical columns
  most_common <- dataset %>%
    summarize_if(function(x) {
      is.character(x) | is.factor(x)
    }, Mode) %>%
    as.data.frame()

  # Join to aggregated
  aggregated <- aggregated %>%
    bind_cols(most_common)

  # Repeat for different combinations of inputs
  if (model_type %in% c("sold_to_human", "sold_by_human")) {
    aggregated <- rbind(aggregated, aggregated, aggregated, aggregated, aggregated, aggregated)
    aggregated$race_desc_seller <- c(
      "White", "Black/African American",
      "White", "Black/African American",
      "White", "Black/African American"
    )
    aggregated$buyer_full_entitity_type <- c(
      "Non-iBuyer (Personal)", "Non-iBuyer (Personal)",
      "Non-iBuyer (Institutional)", "Non-iBuyer (Institutional)",
      "iBuyer", "iBuyer"
    )

    return(aggregated)
  } else if (model_type == "resales") {
    aggregated <- rbind(
      aggregated, aggregated, aggregated, aggregated, aggregated,
      aggregated, aggregated, aggregated, aggregated, aggregated
    )
    aggregated$race_desc_seller <- c(
      "White", "White",
      "Black/African American", "Black/African American",
      "Multiple", "Multiple",
      "Unknown", "Unknown",
      "Other", "Other"
    )
    aggregated$ib_intermediary <- c(
      TRUE, FALSE,
      TRUE, FALSE,
      TRUE, FALSE,
      TRUE, FALSE,
      TRUE, FALSE
    )

    return(aggregated)
  }
}


coefs <- c(
  "race_desc_sellerWhite",
  "buyer_full_entitity_typeNon-iBuyer (Institutional)",
  "buyer_full_entitity_typeNon-iBuyer (Personal)",
  "race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Institutional)",
  "race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Personal)",
  "race_desc_buyerWhite",
  "seller_full_entitity_typeNon-iBuyer (Institutional)",
  "seller_full_entitity_typeNon-iBuyer (Personal)",
  "race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Institutional)",
  "race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Personal)",
  "race_gap_ibuyer",
  "race_gap_personal",
  "race_gap_institutional_non_ibuyer",
  "ibuyer_discount_white",
  "ibuyer_discount_black",
  "institutional_discount_white",
  "instiutional_discount_black",
  "dif_between_ibuyer_and_institutional_race_gaps",
  "dif_between_ibuyer_and_personal_race_gaps",
  "median_value_white_and_personal",
  "median_value_black_and_personal",
  "median_value_white_and_institutional",
  "median_value_black_and_institutional",
  "median_value_white_and_ibuyer",
  "median_value_black_and_ibuyer",
  "mean_value_white_and_personal",
  "mean_value_black_and_personal",
  "mean_value_white_and_institutional",
  "mean_value_black_and_institutional",
  "mean_value_white_and_ibuyer",
  "mean_value_black_and_ibuyer",
  "black_to_ibuyer",
  "black_to_personal",
  "black_to_institutional",
  "white_to_ibuyer",
  "white_to_personal",
  "white_to_institutional",
  "median_value_white_ib_intermediated",
  "median_value_white_non_ib_intermediated",
  "median_value_black_ib_intermediated",
  "median_value_black_non_ib_intermediated",
  "mean_value_white_ib_intermediated",
  "mean_value_white_non_ib_intermediated",
  "mean_value_black_ib_intermediated",
  "mean_value_black_non_ib_intermediated",
  "race_desc_sellerWhite:ib_intermediaryTRUE",
  "race_desc_sellerUnknown:ib_intermediaryTRUE",
  "race_desc_sellerOther:ib_intermediaryTRUE",
  "race_desc_sellerMultiple:ib_intermediaryTRUE",
  "ib_intermediaryTRUE",
  "race_desc_sellerUnknown",
  "race_desc_sellerOther",
  "race_desc_sellerMultiple",
  "ibuyer_gap_white",
  "ibuyer_gap_black",
  "ibuyer_gap_other",
  "ibuyer_gap_unknown",
  "ibuyer_gap_multiple"
)

for (i in 1:476) {
  coefs <- c(coefs, paste0("b[(Intercept) npa_id:", i, "]"))
}

get_estimate <- function(coef, ci_prob, samples) {
  mn <- samples %>%
    .[[coef]] %>%
    mean()
  lb <- samples %>%
    .[[coef]] %>%
    quantile((1 - ci_prob) / 2)
  ub <- samples %>%
    .[[coef]] %>%
    quantile(ci_prob + (1 - ci_prob) / 2)
  sd <- samples %>%
    .[[coef]] %>%
    sd()
  return(
    list(
      "mean" = mn,
      "lb" = lb,
      "ub" = ub,
      "sd" = sd
    )
  )
}

get_estimates <- function(coefs, model_info) {
  samples <- as.data.frame(model$model) %>% tibble()

  medians <- get_central(median, model_info$model_type)
  median_posterior_predictions <- posterior_predict(model$model, newdata = medians, re.form = NA, seed = 624164)

  means <- get_central(mean, model_info$model_type)
  mean_posterior_predictions <- posterior_predict(model$model, newdata = means, re.form = NA, seed = 630268)

  if (model_info$model_type == "resales") {
    samples$median_value_white_ib_intermediated <- median_posterior_predictions %>% .[, 1]
    samples$median_value_white_non_ib_intermediated <- median_posterior_predictions %>% .[, 2]
    samples$median_value_black_ib_intermediated <- median_posterior_predictions %>% .[, 3]
    samples$median_value_black_non_ib_intermediated <- median_posterior_predictions %>% .[, 4]

    samples$mean_value_white_ib_intermediated <- mean_posterior_predictions %>% .[, 1]
    samples$mean_value_white_non_ib_intermediated <- mean_posterior_predictions %>% .[, 2]
    samples$mean_value_black_ib_intermediated <- mean_posterior_predictions %>% .[, 3]
    samples$mean_value_black_non_ib_intermediated <- mean_posterior_predictions %>% .[, 4]

    samples$ibuyer_gap_white <- samples$`race_desc_sellerWhite:ib_intermediaryTRUE` + samples$`ib_intermediaryTRUE`
    samples$ibuyer_gap_black <- samples$`ib_intermediaryTRUE`
    samples$ibuyer_gap_other <- samples$`race_desc_sellerOther:ib_intermediaryTRUE` + samples$`ib_intermediaryTRUE`
    samples$ibuyer_gap_unknown <- samples$`race_desc_sellerUnknown:ib_intermediaryTRUE` + samples$`ib_intermediaryTRUE`
    samples$ibuyer_gap_multiple <- samples$`race_desc_sellerMultiple:ib_intermediaryTRUE` + samples$`ib_intermediaryTRUE`
  } else {
    samples$median_value_white_and_personal <- median_posterior_predictions %>% .[, 1]
    samples$median_value_black_and_personal <- median_posterior_predictions %>% .[, 2]
    samples$median_value_white_and_institutional <- median_posterior_predictions %>% .[, 3]
    samples$median_value_black_and_institutional <- median_posterior_predictions %>% .[, 4]
    samples$median_value_white_and_ibuyer <- median_posterior_predictions %>% .[, 5]
    samples$median_value_black_and_ibuyer <- median_posterior_predictions %>% .[, 6]

    samples$mean_value_white_and_personal <- mean_posterior_predictions %>% .[, 1]
    samples$mean_value_black_and_personal <- mean_posterior_predictions %>% .[, 2]
    samples$mean_value_white_and_institutional <- mean_posterior_predictions %>% .[, 3]
    samples$mean_value_black_and_institutional <- mean_posterior_predictions %>% .[, 4]
    samples$mean_value_white_and_ibuyer <- mean_posterior_predictions %>% .[, 5]
    samples$mean_value_black_and_ibuyer <- mean_posterior_predictions %>% .[, 6]
  }
  if (model_info$model_type == "sold_by_human") {
    samples$race_gap_ibuyer <- samples$race_desc_sellerWhite

    samples$race_gap_personal <- samples$race_desc_sellerWhite + samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Personal)`
    samples$race_gap_institutional_non_ibuyer <- samples$race_desc_sellerWhite + samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Institutional)`

    samples$ibuyer_discount_black <- samples$`buyer_full_entitity_typeNon-iBuyer (Personal)`
    samples$ibuyer_discount_white <- samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Personal)` + samples$`buyer_full_entitity_typeNon-iBuyer (Personal)`
    samples$instiutional_discount_black <- samples$`buyer_full_entitity_typeNon-iBuyer (Personal)` - samples$`buyer_full_entitity_typeNon-iBuyer (Institutional)`
    samples$institutional_discount_white <- samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Personal)` + samples$`buyer_full_entitity_typeNon-iBuyer (Personal)` - (samples$"race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Institutional)" + samples$`buyer_full_entitity_typeNon-iBuyer (Institutional)`)

    samples$dif_between_ibuyer_and_institutional_race_gaps <- samples$race_gap_institutional_non_ibuyer - samples$race_gap_ibuyer
    samples$dif_between_ibuyer_and_personal_race_gaps <- samples$race_gap_personal - samples$race_gap_ibuyer

    samples$black_to_ibuyer <- 0
    samples$black_to_personal <- samples$ibuyer_discount_black
    samples$black_to_institutional <- samples$instiutional_discount_black

    samples$white_to_ibuyer <- samples$race_gap_ibuyer
    samples$white_to_personal <- samples$`buyer_full_entitity_typeNon-iBuyer (Personal)` + samples$race_gap_ibuyer + samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Personal)`
    samples$white_to_institutional <- samples$`buyer_full_entitity_typeNon-iBuyer (Institutional)` + samples$race_gap_ibuyer + samples$`race_desc_sellerWhite:buyer_full_entitity_typeNon-iBuyer (Institutional)`
  }

  if (model_info$model_type == "sold_to_human") {
    samples$race_gap_ibuyer <- samples$race_desc_buyerWhite

    samples$race_gap_personal <- samples$race_desc_buyerWhite + samples$`race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Personal)`
    samples$race_gap_institutional_non_ibuyer <- samples$race_desc_buyerWhite + samples$`race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Institutional)`

    samples$ibuyer_discount_black <- samples$`seller_full_entitity_typeNon-iBuyer (Personal)`
    samples$ibuyer_discount_white <- samples$`race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Personal)` + samples$`seller_full_entitity_typeNon-iBuyer (Personal)`
    samples$instiutional_discount_black <- samples$`seller_full_entitity_typeNon-iBuyer (Personal)` - samples$`seller_full_entitity_typeNon-iBuyer (Institutional)`
    samples$institutional_discount_white <- samples$`race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Personal)` + samples$`seller_full_entitity_typeNon-iBuyer (Personal)` - (samples$"race_desc_buyerWhite:seller_full_entitity_typeNon-iBuyer (Institutional)" + samples$`seller_full_entitity_typeNon-iBuyer (Institutional)`)

    samples$dif_between_ibuyer_and_institutional_race_gaps <- samples$race_gap_institutional_non_ibuyer - samples$race_gap_ibuyer
    samples$dif_between_ibuyer_and_personal_race_gaps <- samples$race_gap_personal - samples$race_gap_ibuyer
  }

  for (coef in coefs) {
    estimate <- get_estimate(coef, 0.95, samples)
    model_info[[paste0(coef, "_lower")]] <- estimate$lb
    model_info[[paste0(coef, "_point")]] <- estimate$mean
    model_info[[paste0(coef, "_upper")]] <- estimate$ub
    model_info[[paste0(coef, "_sd")]] <- estimate$sd
  }
  return(model_info)
}

model_dir <- file.path("analysis", "models")

files <- list.files(model_dir)
files <- files[grepl("RDS", files, fixed = TRUE)]

all_models <- c()
for (model_name in files) {
  # Make sure file isn't empty
  if (file.size(file.path(model_dir, model_name)) == 0) {
    next()
  }
  print(model_name)
  model_info <- c()
  model <- read_model(model_name)
  if (grepl("logistic", model_name, fixed = TRUE)) {
    model_info$r2 <- mean(model$r2)
    model_info$waic <- model$waic$estimates["waic", 1]
  } else {
    model_info$r2 <- mean(model$model_r2)
    model_info$waic <- model$model_waic$estimates["waic", 1]
  }

  if (grepl("sold_to_human", model_name, fixed = TRUE)) {
    model_info$model_type <- "sold_to_human"
  } else if (grepl("sold_by_human", model_name, fixed = TRUE)) {
    model_info$model_type <- "sold_by_human"
  } else {
    model_info$model_type <- "resales"
  }


  model_info <- tibble::as_tibble_row(model_info)
  model_info$model_name <- model_name

  if (grepl("log_sp", model_name, fixed = TRUE)) {
    model_info$target <- "log_sp"
  } else if (grepl("profit", model_name, fixed = TRUE)) {
    model_info$target <- "profit"
  } else if (grepl("sp", model_name, fixed = TRUE)) {
    model_info$target <- "sp"
  } else {
    model_info$target <- "sold_to_instiutional"
  }

  model_info <- get_estimates(coefs, model_info)

  remaining_string <- gsub(paste0("_", model_info$target, ".RDS"), "", model_name)
  fixed_or_random <- strsplit(remaining_string, "_") %>%
    .[[1]] %>%
    tail(1)
  model_info$fixed_or_random <- fixed_or_random

  remaining_string <- gsub(paste0(model_info$model_type, "_"), "", remaining_string)

  remaining_string <- gsub(paste0("_", fixed_or_random), "", remaining_string)
  model_info$variables <- remaining_string


  all_models <- rbind(all_models, model_info)

  write_csv(all_models, file.path("../data", "analysis", "model_summaries.csv"))
}


