#=============================
# loaddata.R file, loading data for the sociodem shiny app
# author: Sebastiano Caleffi (github: scaleffi)
#=============================

# Load libraries ----
library(tidyverse)
library(readr) # needed for the read_csv function, which is faster than the base read.csv

# Load files ----
csv_file_trs <- "report_env_trs_011824.csv"
csv_file_box <- "report_env_box_011824.csv"
csv_file_sel <- "report_env_sel_011824.csv"
csv_file_trs_rgsage <- "report_env_trs_norgs_011824.csv"

# Read in the 'box' file, which groups the 4 sociodem dimensions in two variables, age.education and sex.urbanisation ----
data_box <- read_csv(csv_file_box) # read_csv reads column names and data formats differently from read.csv, make sure they are as expected

## Format the data as needed ----
data_box$value <- round(data_box$value, 0)

df <- data_box %>%
  #Rename values in env_itm column to include unit of measurements for each environmental dimension
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average environmental impact",
    env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
    TRUE ~ env_itm  # Keep the original value if it doesn't match any condition
  ),
  dmd_scn = case_when(
    dmd_scn == "actl" ~ "actual demand",
    dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
    TRUE ~ dmd_scn
  ),
  food_group = case_when(
    food_group == "othr_ani" ~ "eggs&fats",
    food_group == "othr_meat" ~ "other meats",
    food_group == "fruit_veg" ~ "fruit&veg",
    food_group == "nuts_seeds" ~ "nuts&seeds",
    TRUE ~ food_group
  ),
  measure = case_when(
    measure == "abs" ~ "absolute",
    measure == "cap" ~ "per capita",
    measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
    measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
    measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
    measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
    TRUE ~ measure
  )) %>%
  rename("age.education" = "age-education") %>%
  rename("sex.urbanisation" = "sex-urbanisation")

# Read in the 'transposed' file, that lists all sociodem dimensions in a single column/variable ----
data_trs <- read_csv(csv_file_trs)

## Format the data as needed ----
data_trs$value <- round(data_trs$value, 0)
df_trs <- data_trs

# Create a new dataset, data_trs_category, by adding a column labelled 'category' to the _trs dataset, to group different labels in the variable age/sociodem to subgroups (if useful)
data_trs_category <- data_trs %>%
  mutate(category = case_when(
    age %in% c("FML", "MLE", "BTH") ~ "Sex",
    age %in% c("low", "medium", "high", "all-e") ~ "Education",
    age %in% c("rural", "urban", "all-u") ~ "Residence",
    age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a") ~ "Age"
  ),
  food_group = case_when(
    food_group == "othr_ani" ~ "eggs&fats",
    food_group == "othr_meat" ~ "other meats",
    food_group == "fruit_veg" ~ "fruit&veg",
    food_group == "nuts_seeds" ~ "nuts&seeds",
    TRUE ~ food_group
  ))

df_trs_category <- data_trs_category

df_trs_category <- df_trs_category %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average environmental impact",
    env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    food_group = case_when(
      food_group == "othr_ani" ~ "eggs&fats",
      food_group == "othr_meat" ~ "other meats",
      food_group == "fruit_veg" ~ "fruit&veg",
      food_group == "nuts_seeds" ~ "nuts&seeds",
      TRUE ~ food_group
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
      measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
      measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
      TRUE ~ measure
    ))

#Create another dataset, data_trs_macrofoods by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
# namely ASF, Staples, Other, Total. This dataset includes a column for the category, and a column for the macrofoods.
data_trs_macrofoods <- data_trs_category %>%
  mutate(food_group = case_when(
    food_group == "othr_ani" ~ "eggs&fats",
    food_group == "othr_meat" ~ "other meats",
    food_group == "fruit_veg" ~ "fruit&veg",
    food_group == "nuts_seeds" ~ "nuts&seeds",
    TRUE ~ food_group
  ),
    macrofoods = case_when(
    food_group %in% c("beef","lamb", "dairy", "eggs&fats", "othr_meat", "pork", "fish") ~ "ASF",
    food_group %in% c("rice", "grains", "roots") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

#Create a third dataset, df_trs_macrof, by adding a column labelled 'macrofoods' to the main df dataframe. This dataset only has one additional column, to group macrofoods, if the user is not
#interested in grouping sociodem categories
df_trs_macrof <- df_trs %>%
  mutate(
  #   food_group = case_when(
  #   food_group == "othr_ani" ~ "eggs&fats",
  #   food_group == "othr_meat" ~ "other meats",
  #   food_group == "fruit_veg" ~ "fruit&veg",
  #   food_group == "nuts_seeds" ~ "nuts&seeds",
  #   TRUE ~ food_group
  # ),
    macrofoods = case_when(
    food_group %in% c("beef", "lamb", "dairy", "othr_ani", "othr_meat", "pork", "fish") ~ "ASF",
    food_group %in% c("rice", "grains", "roots") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

df_trs_macrof <- df_trs_macrof %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average environmental impact",
    env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
    TRUE ~ env_itm),
    food_group = case_when(
      food_group == "othr_ani" ~ "eggs&fats",
      food_group == "othr_meat" ~ "other meats",
      food_group == "fruit_veg" ~ "fruit&veg",
      food_group == "nuts_seeds" ~ "nuts&seeds",
      TRUE ~ food_group
    ),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
      measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
      measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    ))


# Read in the 'sel' file, which has all sociodem dimensions split into separate variables ----
data_sel <- read_csv(csv_file_sel)

## Format the data as needed ----
data_sel$value <- round(data_sel$value, 0)
df_sel <- data_sel %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average environmental impact",
    env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    food_group = case_when(
      food_group == "othr_ani" ~ "eggs&fats",
      food_group == "othr_meat" ~ "other meats",
      food_group == "fruit_veg" ~ "fruit&veg",
      food_group == "nuts_seeds" ~ "nuts&seeds",
      TRUE ~ food_group
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
      measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
      measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    ))

# Read in the transposed+regions file, which combines all the sociodem dimensions AND the regions in a single variable ----
data_trs_rgsage <- read_csv(csv_file_trs_rgsage)
data_trs_rgsage$value <- round(data_trs_rgsage$value, 0)
df_trs_rgsage <- data_trs_rgsage