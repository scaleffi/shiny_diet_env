# Header ------------------------------------------------------------------
# Author: Sebastiano Caleffi (github: scaleffi)


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readr) # needed for the read_csv function, which is faster than the base read.csv
library(magrittr)

# Load .csv data files ---------------------------------------------------------
csv_file_trs <- "report_env_trs_011824.csv"
csv_file_box <- "report_env_box_011824.csv"
csv_file_sel <- "report_env_sel_011824.csv"
csv_file_trs_rgsage <- "report_env_trs_norgs_011824.csv"


# Create dataframe 'Box' (age-sex and edu-urb) ----------------------------
df_box <- read_csv(csv_file_box)
df_box$value <- round(df_box$value, 0)
df <- df_box %>%
  #Rename values in env_itm column to include unit of measurements for each environmental dimension
  mutate(
    env_itm = case_when(
      env_itm == "GHG" ~ "GHG (Mt CO2eq)",
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
    )
  ) %>%
  rename("age.education" = "age-education") %>%
  rename("sex.urbanisation" = "sex-urbanisation")


# Create dataframe 'Trs' (age-sex-edu-urb) --------------------------------
df_trs <- read_csv(csv_file_trs)
df_trs$value <- round(df_trs$value, 0)
df_trs <- df_trs %>%
  mutate(
    env_itm = case_when(
      env_itm == "GHG" ~ "GHG (Mt CO2eq)",
      env_itm == "water" ~ "water use (km3)",
      env_itm == "land" ~ "land use (thousands of km2)",
      env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
      env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
      env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
      env_itm == "avg" ~ "average environmental impact",
      env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
      TRUE ~ env_itm
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
    )
  )

df_trs_category <-
  df_trs %>% # create new dataset with variable 'category' for grouping in plots
  mutate(
    category = case_when(
      age %in% c("FML", "MLE", "BTH") ~ "Sex",
      age %in% c("low", "medium", "high", "all-e") ~ "Education",
      age %in% c("rural", "urban", "all-u") ~ "Residence",
      age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a") ~ "Age"
    )
  )

df_trs_macrofoods <-
  df_trs_category %>% # create a new dataset with both variables 'category' and 'macrofoods', for grouping in plots
  mutate(
    macrofoods = case_when(
      food_group %in% c(
        "beef",
        "lamb",
        "dairy",
        "eggs&fats",
        "othr_meat",
        "pork",
        "fish"
      ) ~ "ASF",
      food_group %in% c("rice", "grains", "roots") ~ "Staples",
      food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
      food_group %in% c("total") ~ "Total"
    )
  )

df_trs_macrof <-
  df_trs %>% # create a new datasets with variable 'macrofoods', for grouping in plots
  mutate(
    macrofoods = case_when(
      food_group %in% c("beef", "lamb", "dairy", "othr_ani", "othr_meat", "pork", "fish") ~ "ASF",
      food_group %in% c("rice", "grains", "roots") ~ "Staples",
      food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
      food_group %in% c("total") ~ "Total"
    )
  )


# Create dataframe 'Sel' (age,sex,edu,urb) --------------------------------
df_sel <- read_csv(csv_file_sel)
df_sel$value <- round(df_sel$value, 0)
df_sel <- df_sel %>%
  mutate(
    env_itm = case_when(
      env_itm == "GHG" ~ "GHG (Mt CO2eq)",
      env_itm == "water" ~ "water use (km3)",
      env_itm == "land" ~ "land use (thousands of km2)",
      env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
      env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
      env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
      env_itm == "avg" ~ "average environmental impact",
      env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
      TRUE ~ env_itm
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
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    )
  )


# Create dataframe 'Trsage' (age-sex-edu-urb-region) ----------------------
df_trs_rgsage <- read_csv(csv_file_trs_rgsage)
df_trs_rgsage$value <- round(df_trs_rgsage$value, 0)
df_trs_rgsage <- df_trs_rgsage %>%
  mutate(
    env_itm = case_when(
      env_itm == "GHG" ~ "GHG (Mt CO2eq)",
      env_itm == "water" ~ "water use (km3)",
      env_itm == "land" ~ "land use (thousands of km2)",
      env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
      env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
      env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
      env_itm == "avg" ~ "average environmental impact",
      env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
      TRUE ~ env_itm
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
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    )
  )