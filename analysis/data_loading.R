### Read Data
library(dplyr)
library(readr)
library(tidyr)

nth_rt <- function(x, n) {
  # nth root function, for fitting nth-root transformed models
  x^(1 / n)
}

# Load and preprocess data
data <- read_csv("data/analysis/complete_transfer_info_20240111.csv") %>%
  mutate(
    Purchaser = case_when(ibuyer_buyer == 1 ~ "iBuyer", ibuyer_buyer == 0 ~ "Non-iBuyer"),
    Seller = case_when(ibuyer_seller == 1 ~ "iBuyer", ibuyer_seller == 0 ~ "Non-iBuyer"),
    profit = next_sale_price - sale_price
  ) %>%
  unite(buyer_full_entitity_type, c("Purchaser", "entity_type_coarse_buyer"), sep = " ", remove = FALSE) %>%
  unite(seller_full_entitity_type, c("Seller", "entity_type_coarse_seller"), sep = " ", remove = FALSE) %>%
  mutate(buyer_full_entitity_type = case_match(
    buyer_full_entitity_type,
    "Non-iBuyer Personal" ~ "Non-iBuyer (Personal)",
    "Non-iBuyer Institutional" ~ "Non-iBuyer (Institutional)",
    "Non-iBuyer Multiple" ~ "Non-iBuyer (Multiple)",
    "Non-iBuyer Unknown" ~ "Non-iBuyer (Unknown Entity Type)",
    "iBuyer Institutional" ~ "iBuyer"
  )) %>%
  mutate(seller_full_entitity_type = case_match(
    seller_full_entitity_type,
    "Non-iBuyer Personal" ~ "Non-iBuyer (Personal)",
    "Non-iBuyer Institutional" ~ "Non-iBuyer (Institutional)",
    "Non-iBuyer Multiple" ~ "Non-iBuyer (Multiple)",
    "Non-iBuyer Unknown" ~ "Non-iBuyer (Unknown Entity Type)",
    "iBuyer Institutional" ~ "iBuyer"
  )) %>%
  mutate(
    buyer_entity =
      case_when(
        cleaned_name_buyer == "OPENDOOR" ~ "OPENDOOR",
        cleaned_name_buyer == "OFFERPAD" ~ "OFFERPAD",
        cleaned_name_buyer == "ZILLOW" ~ "ZILLOW",
        cleaned_name_buyer == "RIBBON" ~ "RIBBON",
        buyer_full_entitity_type == "iBuyer" ~ "OTHER IBUYER",
        cleaned_name_buyer == "CERBERUS SFR HOLDINGS" ~ "CERBERUS SFR HOLDINGS",
        cleaned_name_buyer == "SFR JV PROPERTY" ~ "SFR JV PROPERTY",
        cleaned_name_buyer == "PROGRESS RESIDENTIAL" ~ "PROGRESS RESIDENTIAL",
        cleaned_name_buyer == "AMHERST" ~ "AMHERST",
        sfr_buyer == 1 ~ "OTHER SFR",
        cleaned_name_buyer == "D.R. HORTON" ~ "D.R. HORTON",
        cleaned_name_buyer == "NVR" ~ "NVR",
        cleaned_name_buyer == "MATTAMY HOMES" ~ "MATTAMY HOMES",
        cleaned_name_buyer == "LENNAR CAROLINAS" ~ "LENNAR CAROLINAS",
        cleaned_name_buyer == "MERITAGE HOMES OF THE CAROLINAS" ~ "MERITAGE HOMES OF THE CAROLINAS",
        cleaned_name_buyer == "PULTE HOME" ~ "PULTE HOME",
        cleaned_name_buyer == "LGI HOMES-NC" ~ "LGI HOMES-NC",
        cleaned_name_buyer == "TRUE HOMES" ~ "TRUE HOMES",
        buyer_full_entitity_type == "Non-iBuyer (Institutional)" ~ "OTHER INSTIUTIONAL",
        buyer_full_entitity_type == "Non-iBuyer (Personal)" ~ "PERSONAL",
        buyer_full_entitity_type == "Non-iBuyer (Multiple)" ~ "MULTIPLE",
        buyer_full_entitity_type == "Non-iBuyer (Unknown Entity Type)" ~ "UNKNOWN ENTITY TYPE"
      )
  ) %>%
  mutate(
    seller_entity =
      case_when(
        cleaned_name_seller == "OPENDOOR" ~ "OPENDOOR",
        cleaned_name_seller == "OFFERPAD" ~ "OFFERPAD",
        cleaned_name_seller == "ZILLOW" ~ "ZILLOW",
        cleaned_name_seller == "RIBBON" ~ "RIBBON",
        seller_full_entitity_type == "iBuyer" ~ "OTHER IBUYER",
        cleaned_name_seller == "CERBERUS SFR HOLDINGS" ~ "CERBERUS SFR HOLDINGS",
        cleaned_name_seller == "SFR JV PROPERTY" ~ "SFR JV PROPERTY",
        cleaned_name_seller == "PROGRESS RESIDENTIAL" ~ "PROGRESS RESIDENTIAL",
        cleaned_name_seller == "AMHERST" ~ "AMHERST",
        sfr_seller == 1 ~ "OTHER SFR",
        cleaned_name_seller == "D.R. HORTON" ~ "D.R. HORTON",
        cleaned_name_seller == "NVR" ~ "NVR",
        cleaned_name_seller == "MATTAMY HOMES" ~ "MATTAMY HOMES",
        cleaned_name_seller == "LENNAR CAROLINAS" ~ "LENNAR CAROLINAS",
        cleaned_name_seller == "MERITAGE HOMES OF THE CAROLINAS" ~ "MERITAGE HOMES OF THE CAROLINAS",
        cleaned_name_seller == "PULTE HOME" ~ "PULTE HOME",
        cleaned_name_seller == "LGI HOMES-NC" ~ "LGI HOMES-NC",
        cleaned_name_seller == "TRUE HOMES" ~ "TRUE HOMES",
        seller_full_entitity_type == "Non-iBuyer (Institutional)" ~ "OTHER INSTIUTIONAL",
        seller_full_entitity_type == "Non-iBuyer (Personal)" ~ "PERSONAL",
        seller_full_entitity_type == "Non-iBuyer (Multiple)" ~ "MULTIPLE",
        seller_full_entitity_type == "Non-iBuyer (Unknown Entity Type)" ~ "UNKNOWN ENTITY TYPE"
      )
  ) %>%
  mutate(x311_request_rate = `311_request_rate`) %>%
  mutate_if(is.character, function(x) {
    replace_na(x, "NA")
  })

# Get rows where property was sold *from* a human
ds1 <- data %>%
  dplyr::filter(entity_type_coarse_seller == "Personal") %>%
  dplyr::filter(entity_type_coarse_buyer %in% c("Personal", "Institutional"))

# Get rows where property was sold *by* a human
ds2 <- data %>%
  dplyr::filter(entity_type_coarse_buyer == "Personal") %>%
  dplyr::filter(entity_type_coarse_seller %in% c("Personal", "Institutional"))


# Create data of resales
resales <- read_csv("data/analysis/resales_20240111.csv") %>%
  mutate(
    Purchaser = case_when(ibuyer_buyer == 1 ~ "iBuyer", ibuyer_buyer == 0 ~ "Non-iBuyer"),
    Seller = case_when(ibuyer_seller == 1 ~ "iBuyer", ibuyer_seller == 0 ~ "Non-iBuyer"),
    profit = next_sale_price - sale_price
  ) %>%
  unite(buyer_full_entitity_type, c("Purchaser", "entity_type_coarse_buyer"), sep = " ", remove = FALSE) %>%
  unite(seller_full_entitity_type, c("Seller", "entity_type_coarse_seller"), sep = " ", remove = FALSE) %>%
  mutate(buyer_full_entitity_type = case_match(
    buyer_full_entitity_type,
    "Non-iBuyer Personal" ~ "Non-iBuyer (Personal)",
    "Non-iBuyer Institutional" ~ "Non-iBuyer (Institutional)",
    "Non-iBuyer Multiple" ~ "Non-iBuyer (Multiple)",
    "Non-iBuyer Unknown" ~ "Non-iBuyer (Unknown Entity Type)",
    "iBuyer Institutional" ~ "iBuyer"
  )) %>%
  mutate(seller_full_entitity_type = case_match(
    seller_full_entitity_type,
    "Non-iBuyer Personal" ~ "Non-iBuyer (Personal)",
    "Non-iBuyer Institutional" ~ "Non-iBuyer (Institutional)",
    "Non-iBuyer Multiple" ~ "Non-iBuyer (Multiple)",
    "Non-iBuyer Unknown" ~ "Non-iBuyer (Unknown Entity Type)",
    "iBuyer Institutional" ~ "iBuyer"
  )) %>%
  mutate(
    buyer_entity =
      case_when(
        cleaned_name_buyer == "OPENDOOR" ~ "OPENDOOR",
        cleaned_name_buyer == "OFFERPAD" ~ "OFFERPAD",
        cleaned_name_buyer == "ZILLOW" ~ "ZILLOW",
        cleaned_name_buyer == "RIBBON" ~ "RIBBON",
        buyer_full_entitity_type == "iBuyer" ~ "OTHER IBUYER",
        cleaned_name_buyer == "CERBERUS SFR HOLDINGS" ~ "CERBERUS SFR HOLDINGS",
        cleaned_name_buyer == "SFR JV PROPERTY" ~ "SFR JV PROPERTY",
        cleaned_name_buyer == "PROGRESS RESIDENTIAL" ~ "PROGRESS RESIDENTIAL",
        cleaned_name_buyer == "AMHERST" ~ "AMHERST",
        sfr_buyer == 1 ~ "OTHER SFR",
        cleaned_name_buyer == "D.R. HORTON" ~ "D.R. HORTON",
        cleaned_name_buyer == "NVR" ~ "NVR",
        cleaned_name_buyer == "MATTAMY HOMES" ~ "MATTAMY HOMES",
        cleaned_name_buyer == "LENNAR CAROLINAS" ~ "LENNAR CAROLINAS",
        cleaned_name_buyer == "MERITAGE HOMES OF THE CAROLINAS" ~ "MERITAGE HOMES OF THE CAROLINAS",
        cleaned_name_buyer == "PULTE HOME" ~ "PULTE HOME",
        cleaned_name_buyer == "LGI HOMES-NC" ~ "LGI HOMES-NC",
        cleaned_name_buyer == "TRUE HOMES" ~ "TRUE HOMES",
        buyer_full_entitity_type == "Non-iBuyer (Institutional)" ~ "OTHER INSTIUTIONAL",
        buyer_full_entitity_type == "Non-iBuyer (Personal)" ~ "PERSONAL",
        buyer_full_entitity_type == "Non-iBuyer (Multiple)" ~ "MULTIPLE",
        buyer_full_entitity_type == "Non-iBuyer (Unknown Entity Type)" ~ "UNKNOWN ENTITY TYPE"
      )
  ) %>%
  mutate(
    seller_entity =
      case_when(
        cleaned_name_seller == "OPENDOOR" ~ "OPENDOOR",
        cleaned_name_seller == "OFFERPAD" ~ "OFFERPAD",
        cleaned_name_seller == "ZILLOW" ~ "ZILLOW",
        cleaned_name_seller == "RIBBON" ~ "RIBBON",
        seller_full_entitity_type == "iBuyer" ~ "OTHER IBUYER",
        cleaned_name_seller == "CERBERUS SFR HOLDINGS" ~ "CERBERUS SFR HOLDINGS",
        cleaned_name_seller == "SFR JV PROPERTY" ~ "SFR JV PROPERTY",
        cleaned_name_seller == "PROGRESS RESIDENTIAL" ~ "PROGRESS RESIDENTIAL",
        cleaned_name_seller == "AMHERST" ~ "AMHERST",
        sfr_seller == 1 ~ "OTHER SFR",
        cleaned_name_seller == "D.R. HORTON" ~ "D.R. HORTON",
        cleaned_name_seller == "NVR" ~ "NVR",
        cleaned_name_seller == "MATTAMY HOMES" ~ "MATTAMY HOMES",
        cleaned_name_seller == "LENNAR CAROLINAS" ~ "LENNAR CAROLINAS",
        cleaned_name_seller == "MERITAGE HOMES OF THE CAROLINAS" ~ "MERITAGE HOMES OF THE CAROLINAS",
        cleaned_name_seller == "PULTE HOME" ~ "PULTE HOME",
        cleaned_name_seller == "LGI HOMES-NC" ~ "LGI HOMES-NC",
        cleaned_name_seller == "TRUE HOMES" ~ "TRUE HOMES",
        seller_full_entitity_type == "Non-iBuyer (Institutional)" ~ "OTHER INSTIUTIONAL",
        seller_full_entitity_type == "Non-iBuyer (Personal)" ~ "PERSONAL",
        seller_full_entitity_type == "Non-iBuyer (Multiple)" ~ "MULTIPLE",
        seller_full_entitity_type == "Non-iBuyer (Unknown Entity Type)" ~ "UNKNOWN ENTITY TYPE"
      )
  ) %>%
  mutate(x311_request_rate = `311_request_rate`) %>%
  mutate_if(is.character, function(x) {
    replace_na(x, "NA")
  }) %>%
  dplyr::filter(entity_type_coarse_seller == "Personal") %>%
  dplyr::filter(entity_type_coarse_buyer %in% c("Personal", "Institutional")) %>%
  mutate(sold_to_institutional = case_when(
    sold_to == "Institutional" ~ TRUE,
    TRUE ~ FALSE
  ))


# List all variables
house_features <- c(
  "finished_area", "year_built", "built_use_style", "grade", "story",
  "heat", "fuel", "foundation", "external_wall", "fireplaces", "full_baths",
  "half_baths", "bedrooms", "total_sqft"
)

neighborhood <- c(
  "population_density",
  "proficiency_elementary_school",
  "proficiency_middle_school", "student_absenteeism", "neighborhood_school_attendance",
  "fire_call_rate", "x311_request_rate",
  "grocery_proximity", "early_care_proximity", "low_cost_healthcare_proximity",
  "fincancial_services_proximity", "park_proximity", "pharmacy_proximity",
  "school_age_proximity", "transit_proximity",
  "library_card_prevalence", "voter_participation", "board_committee_participation",
  "job_density",
  "public_health_insurance", "public_nutrition_assistance", "subsidized_housing",
  "property_crime_rate", "violent_crime_rate",
  "age_of_death", "births_to_adolescents", "low_birthweight", "prenatal_care",
  "commercial_construction", "foreclosures", "new_residential", "rental_houses",
  "housing_age", "housing_density", "housing_size", "vacant_land", "tree_canopy", "residential_demolitions",
  "single_family_housing", "residential_renovation", "residential_tree_canopy",
  "bicycle_friendliness"
)

neighborhood_demographics <- c(
  "asian_population", "white_population", "hispanic_latino",
  "black_population"
  # 'all_other_races' remove this for multicolinearity
)

# Function for reading a model that has been fit
read_model <- function(model_name) {
  model_path <- file.path("analysis", "models", model_name)
  model <- readRDS(model_path)
  return(model)
}
