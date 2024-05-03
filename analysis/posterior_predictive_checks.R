library(rstanarm)
library(bayesplot)
library(dplyr)
library(tibble)
library(readr)
library(tidyr)
options(mc.cores = parallel::detectCores())

source("analysis/data_loading.R")


########################################################################################################################
# Data loading

targets <- c(
  "sp",
  "log_sp",
  "sqrt_sp",
  "cbrt_sp",
  "fourthrt_sp",
  "fifthrt_sp",
  "logistic_housing"
)

# Iterate through models
ypred <- list()
models <- list()

# Calculate posterior predictions for each model
for (target in targets) {
  # Get file name for model
  if (grep("sp", target)) {
    model_name <- paste0("sold_by_human_house_and_neighborhood_with_demographics_re_", target, ".RDS")
  } else {
    model_name <- paste0(target, ".RDS")
  }

  # Read model
  model <- read_model(model_name)

  # Save model to array
  models[[target]] <- model

  # Get path for posterior draws
  posterior_draws_path <- paste0("analysis/posterior_draws/", target, "_posterior_draws.RDS")
  ndraws <- 5000

  if (file.exists(posterior_draws_path)) {
    # Read from disk, if posterior draws already calculated
    ypred[[target]] <- readRDS(posterior_draws_path)
  } else {
    # Calculate/save posterior draws otherwise
    ypred[[target]] <- posterior_predict(model$model, draws = ndraws)
    saveRDS(ypred[[target]], file = posterior_draws_path)
  }
}

# un-transform targets
ypred$log_sp <- exp(ypred$log_sp)
models$log_sp$model$y <- exp(models$log_sp$model$y)
models$log_sp$model$fitted.values <- exp(models$log_sp$model$fitted.values)
ypred$sqrt_sp <- (ypred$sqrt_sp)^2
models$sqrt_sp$model$y <- (models$sqrt_sp$model$y)^2
models$sqrt_sp$model$fitted.values <- (models$sqrt_sp$model$fitted.values)^2
ypred$cbrt_sp <- (ypred$cbrt_sp)^3
models$cbrt_sp$model$y <- (models$cbrt_sp$model$y)^3
models$cbrt_sp$model$fitted.values <- (models$cbrt_sp$model$fitted.values)^3
ypred$fourthrt_sp <- (ypred$fourthrt_sp)^4
models$fourthrt_sp$model$y <- (models$fourthrt_sp$model$y)^4
models$fourthrt_sp$model$fitted.values <- (models$fourthrt_sp$model$fitted.values)^4
ypred$fifthrt_sp <- (ypred$fifthrt_sp)^5
models$fifthrt_sp$model$y <- (models$fifthrt_sp$model$y)^5
models$fifthrt_sp$model$fitted.values <- (models$fifthrt_sp$model$fitted.values)^5
models$logistic_housing$model$y <- (models$logistic_housing$model$y) %>% as.numeric()

get_variable <- function(variable_name, model) {
  # Find the value for a specific character variable
  # in the data that was used for training a model

  # Get filtered data that was used for fitting the model
  # (Extract the data from the model object)
  df <- as.matrix(model$model$x) %>%
    as_tibble(names(model$model$coefficients))

  # The variable may have been turned into a dummy
  # variable behind the scenes, in which case non of the
  # new column will be a direct match for it. Here,
  # we search for direct matches, to rule out dummies
  direct_matches <- names(df)[
    names(df) == variable_name
  ]

  # Now search for columns that start with the variable name,
  # which would be the dummy variables
  indirect_matches <- names(df)[
    names(df) %>% stringr::str_detect(pattern = paste0("^(as\\.factor\\()?", variable_name, "\\)?[^:]*$"))
  ]

  # If there were not any matches or indirect matches, this column
  # was not actually used for fitting the data
  if ((length(direct_matches) == 0) & (length(indirect_matches) == 0)) {
    return()

    # If there's a direct match (indicating no dummy) return
    # the column
  } else if (length(direct_matches) == 1) {
    return(
      df %>%
        dplyr::select(!!variable_name) %>%
        dplyr::pull()
    )

    # Otherwise, we need to consdense the dummy variable
    # back into a single column
  } else {
    # Get unique values of the variable
    # (These will be used for filling in the base class)
    unique_vals <- model$model$data %>%
      dplyr::select(!!variable_name) %>%
      unique() %>%
      dplyr::pull()

    # Create a vector of NAs
    undummied <- rep(NA, nrow(df))

    # Iterate through dummy columns, adding their info
    # to the vector of NAs as we go
    for (colname in indirect_matches) {
      # Get the value of the variable that this
      # dummy corresponds to
      colvalue <- stringr::str_replace(colname, paste0("^(as.factor\\()?", variable_name, "\\)?([^\\:]+)"), "\\2")

      # Find the observations of this dummy
      # and put them into int vector
      colobservations <- df %>%
        dplyr::pull(!!colname)

      # Add info to dummy
      undummied <-
        ifelse(
          test = colobservations == 1,
          yes = colvalue,
          no = undummied
        )

      # Remove this dummy var from the name of unique values of the variable
      # as this dummy var is not the base class
      unique_vals <- unique_vals[!(unique_vals %in% colvalue)]
    }

    # Fill NAs with the last remaining unique
    # value of the variable, which is the base class
    undummied <- ifelse(
      test = is.na(undummied),
      yes = unique_vals,
      no = undummied
    )

    return(undummied)
  }
}

########################################################################################################################
# PPC for logistic regression

target <- "logistic_housing"


# Show these plots just for B/W Personal/iBuyer
logistic_buyer_ppc <- ppc_bars_grouped(
  models[[target]]$model$y,
  ypred[[target]],
  ifelse(get_variable(
    "ib_intermediary",
    models[[target]]
  ), "Intermediated by iBuyer", "Not Intermediated"),
  prob = 0.95,
  freq=FALSE
) + ggplot2::scale_y_continuous(labels = scales::comma) + ggplot2::scale_x_continuous(breaks = c(0, 1))

ggplot2::ggsave("analysis/figures/ppc/logistic_by_buyer_identity.pdf", logistic_buyer_ppc, , width = 5, height = 4)

sellers <- get_variable(
  "race_desc_seller",
  models[[target]]
)
logistic_seller_ppc <- ppc_bars_grouped(
  models[[target]]$model$y[sellers %in% c("White", "Black/African American")],
  ypred[[target]][, sellers %in% c("White", "Black/African American")],
  sellers[sellers %in% c("White", "Black/African American")],
  prob = 0.95,
  freq=FALSE
) + # Format axis labels with commas
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_continuous(breaks = c(0, 1))
ggplot2::ggsave("analysis/figures/ppc/logistic_by_seller_race.pdf", logistic_seller_ppc, width = 5, height = 4)


sellers_and_buyers <- paste0(sellers, " (", ifelse(get_variable(
    "ib_intermediary",
    models[[target]]
  ),"Intermediated by iBuyer", "Not Intermediated"), ")")
sellers_and_buyers_idx <- sellers_and_buyers %in% c(
  "Black/African American to Non-iBuyer (Personal)",
  "White to Non-iBuyer (Personal)",
  "Black/African American to iBuyer",
  "White to iBuyer"
)
seller_and_buyers_ppc <- ppc_bars_grouped(
  models[[target]]$model$y[sellers %in% c("White", "Black/African American")],
  ypred[[target]][, sellers %in% c("White", "Black/African American")],
  sellers_and_buyers[sellers %in% c("White", "Black/African American")],
  prob=0.95,
  freq=FALSE
) + # Format axis labels with commas
  ggplot2::scale_y_continuous(labels = scales::comma) +
  ggplot2::scale_x_continuous(breaks = c(0, 1))

ggplot2::ggsave("analysis/figures/ppc/logistic_by_seller_and_buyer.pdf", seller_and_buyers_ppc, width = 10, height = 4)


########################################################################################################################
# PPC for sale price and log sale price models

target <- "sp"

buyers <- get_variable(
  "buyer_full_entitity_type",
  models[[target]]
)


# Show these plots just for B/W Personal/iBuyer
buyer_ppc <- ppc_stat_grouped(
  models[[target]]$model$y[buyers %in% c("iBuyer", "Non-iBuyer (Personal)")],
  ypred[[target]][, buyers %in% c("iBuyer", "Non-iBuyer (Personal)")],
  get_variable(
    "buyer_full_entitity_type",
    models[[target]]
  )[buyers %in% c("iBuyer", "Non-iBuyer (Personal)")],
  stat = "mean"
) + # Format y-axis labels with commas
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

ggplot2::ggsave("analysis/figures/ppc/means_by_buyer_identity.pdf", buyer_ppc, , width = 5, height = 4)

sellers <- get_variable(
  "race_desc_seller",
  models[[target]]
)
seller_ppc <- ppc_stat_grouped(
  models[[target]]$model$y[sellers %in% c("White", "Black/African American")],
  ypred[[target]][, sellers %in% c("White", "Black/African American")],
  sellers[sellers %in% c("White", "Black/African American")],
  stat = "mean"
) + # Format y-axis labels with commas
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
ggplot2::ggsave("analysis/figures/ppc/means_by_seller_race.pdf", seller_ppc, width = 5, height = 4)


sellers_and_buyers <- paste(sellers, "to", buyers)
sellers_and_buyers_idx <- sellers_and_buyers %in% c(
  "Black/African American to Non-iBuyer (Personal)",
  "White to Non-iBuyer (Personal)",
  "Black/African American to iBuyer",
  "White to iBuyer"
)
seller_and_buyers_ppc <- ppc_stat_grouped(
  models[[target]]$model$y[sellers_and_buyers_idx],
  ypred[[target]][, sellers_and_buyers_idx],
  sellers_and_buyers[sellers_and_buyers_idx],
  stat = "mean"
) + # Format y-axis labels with commas
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

ggplot2::ggsave("analysis/figures/ppc/means_by_seller_and_buyer.pdf", seller_and_buyers_ppc, width = 10, height = 4)

# Overall distribution
sp_density <- ppc_dens_overlay(models[[target]]$model$y, ypred[[target]][1:500, ]) +
  ggplot2::scale_x_continuous(labels = scales::comma, limits = c(0, 1000000))
ggplot2::ggsave("analysis/figures/ppc/pp_dense_linear.pdf", sp_density, width = 5, height = 4)
log_sp_density <- ppc_dens_overlay(models[["log_sp"]]$model$y, ypred[["log_sp"]][1:500, ]) +
  ggplot2::scale_x_continuous(labels = scales::comma, limits = c(0, 1000000))
ggplot2::ggsave("analysis/figures/ppc/pp_dense_log.pdf", log_sp_density, width = 5, height = 4)
