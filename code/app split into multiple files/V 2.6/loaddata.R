library(tidyverse)

#######-------------------#########-------------------------###########
#Load data files
#######-------------------#########-------------------------###########


#Load files from Github repository ----
#csv_file_trs <- "report_env_trs_053123.csv"
csv_file_trs <- "report_env_trs_110423.csv"
csv_file_box <- "report_env_box_110423.csv" 
csv_file_cons <- "cons_compare_012823.csv"
csv_file_FBSintake <- "FBS_intake_socio_all-a_051523.csv"

data_box <- read.csv(csv_file_box)
data_box$value <- round(data_box$value, 2)
df <- data_box 

#Rename values in env_itm column to include unit of measurements for each environmental dimension
df <- df %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (MT CO2eq)",
    env_itm == "water" ~ "Water use (millions of m3)",
    env_itm == "land" ~ "Land use (thousands of Km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of Km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of Km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (MT PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm  # Keep the original value if it doesn't match any condition
  ),
  cns_prsp = case_when(
    cns_prsp == "actl" ~ "Actual Consumption",
    cns_prsp == "norm" ~ "Consumption normalised to 2,000 kcal/day",
    TRUE ~ cns_prsp
  ),
  measure = case_when(
    measure == "abs" ~ "Absolute",
    measure == "cap" ~ "Per capita",
    measure == "pct_abs_WLD" ~ "Ratio to World average (absolute)",
    measure == "pct_abs_RGS" ~ "Ratio to Regional average (absolute)",
    measure == "pct_cap_RGS" ~ "Ratio to Regional average (capita)",
    measure == "pct_cap_WLD" ~ "Ratio to World average (capita)",
    TRUE ~ measure
  ))

data_trs <- read.csv(csv_file_trs)
data_trs$value <- round(data_trs$value, 2)
df_trs <- data_trs 

# Create a new dataset, data_trs_category, by adding a column labelled 'category' to the _trs dataset, to group different labels in the variable age/sociodem to subgroups (if useful)
data_trs_category <- data_trs %>%
  mutate(category = case_when(
    age %in% c("FML", "MLE", "BTH") ~ "Sex",
    age %in% c("low", "medium", "high", "all-e") ~ "Edu. level",
    age %in% c("rural", "urban", "all-u") ~ "Urb. level",
    age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a") ~ "Age group"
  ))

df_trs_category <- data_trs_category

df_trs_category <- df_trs_category %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (MT CO2eq)",
    env_itm == "water" ~ "Water use (millions of m3)",
    env_itm == "land" ~ "Land use (thousands of Km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of Km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of Km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (MT PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm),
    cns_prsp = case_when(
      cns_prsp == "actl" ~ "Actual Consumption",
      cns_prsp == "norm" ~ "Consumption normalised to 2,000 kcal/day",
      TRUE ~ cns_prsp
    ),
    measure = case_when(
      measure == "abs" ~ "Absolute",
      measure == "cap" ~ "Per capita",
      measure == "pct_abs_WLD" ~ "Ratio to World average (absolute)",
      measure == "pct_abs_RGS" ~ "Ratio to Regional average (absolute)",
      measure == "pct_cap_RGS" ~ "Ratio to Regional average (capita)",
      measure == "pct_cap_WLD" ~ "Ratio to World average (capita)",
      TRUE ~ measure
    ))

#Create another dataset, data_trs_macrofoods by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
# namely ASF, Staples, Other, Total. This dataset includes a column for the category, and a column for the macrofoods.
data_trs_macrofoods <- data_trs_category %>%
  mutate(macrofoods = case_when(
    food_group %in% c("beef_lamb", "dairy", "othr_ani", "othr_meat", "pork", "fish") ~ "ASF",
    food_group %in% c("rice", "grains", "roots") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

#Create a third dataset, df_trs_macrof, by adding a column labelled 'macrofoods' to the main df dataframe. This dataset only has one additional column, to group macrofoods, if the user is not
#interested in grouping sociodem categories
df_trs_macrof <- df_trs %>%
  mutate(macrofoods = case_when(
    food_group %in% c("beef_lamb", "dairy", "othr_ani", "othr_meat", "pork", "fish") ~ "ASF",
    food_group %in% c("rice", "grains", "roots") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

df_trs_macrof <- df_trs_macrof %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (MT CO2eq)",
    env_itm == "water" ~ "Water use (millions of m3)",
    env_itm == "land" ~ "Land use (thousands of Km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of Km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of Km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (MT PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm),
    cns_prsp = case_when(
      cns_prsp == "actl" ~ "Actual Consumption",
      cns_prsp == "norm" ~ "Consumption normalised to 2,000 kcal/day",
      TRUE ~ cns_prsp
    ),
    measure = case_when(
      measure == "abs" ~ "Absolute",
      measure == "cap" ~ "Per capita",
      measure == "pct_abs_WLD" ~ "Ratio to World average (absolute)",
      measure == "pct_abs_RGS" ~ "Ratio to Regional average (absolute)",
      measure == "pct_cap_RGS" ~ "Ratio to Regional average (capita)",
      measure == "pct_cap_WLD" ~ "Ratio to World average (capita)",
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    ))


data_cons <- read.csv(csv_file_cons)
data_cons$Intake <- round(data_cons$Intake, 2)
df_cons <- data_cons[-7]

data_FBSintake <- read.csv(csv_file_FBSintake)
data_FBSintake$Value <- round(data_FBSintake$Value, 2)
df_FBSintake <- data_FBSintake
