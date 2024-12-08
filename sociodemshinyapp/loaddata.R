# Header ------------------------------------------------------------------
# Author: Sebastiano Caleffi (github: scaleffi)


# Load libraries ----------------------------------------------------------
library(tidyverse)
#library(readr) # for the read_csv function, faster than the base read.csv
library(data.table)


# Create replacement labels -----------------------------------------------
# This works as if substituting all possible values in a column. If substituting
# only some, as we do for the food groups, then this notation is not sufficient.
# We must first precompute the substitution and generate a vector with all the 
# possible options using ifelse. As it ends up adding a more computational heavy
# process, I decided to stick with manual substitution in those cases.

labels_env_itm <- c(
  "GHG" = "GHG (Mt CO\u2082eq)",  # Subscript 2
  "water" = "water use (km\u00B3)",
  "land" = "land use (thousands of km\u00B2)",
  "land_crop" = "land use, crops (thousands of km\u00B2)",
  "land_pstr" = "land use, pasture (thousands of km\u00B2)",
  "eutr" = "eutrophication pot. (kt PO\u2084eq)",
  "avg" = "average environmental impact",
  "avg_pb" = "average environmental impact (pb weighted)"
)

labels_dmd_scn <- c(
  "actl" = "actual demand",
  "norm" = "demand normalised to 2,000 kcal/day"
)

labels_measure <- c(
  "abs" = "absolute",
  "cap" = "per capita",
  "pct_abs_WLD" = "ratio to global avg (absolute)",
  "pct_abs_RGS" = "ratio to regional avg (absolute)",
  "pct_cap_RGS" = "ratio to regional mean (capita)",
  "pct_cap_WLD" = "ratio to global mean (capita)"
)

# Create dataframe 'Box' (age-sex and edu-urb) ----------------------------
df <- fread("data/report_env_box_011824.csv")

# Apply the replacements using a named vector and fcase
df[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

# Manual substitution instead of using a vector, because we are operating only on
# some of the labels.
df[food_group %in% c("othr_ani",
                     "othr_meat",
                     "fruit_veg",
                     "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                       food_group == "othr_meat", "other meat",
                                                       food_group == "fruit_veg", "fruit&veg",
                                                       food_group == "nuts_seeds", "nuts&seeds")]


df[,value := round(value,0)]

# Change column names (this could be done in data.table, haven't had time to implement yet)
setnames(df, old = c("age-education", "sex-urbanisation"), new = c("age.education", "sex.urbanisation"))


# Create dataframe 'Trs' (age-sex-edu-urb) --------------------------------
df_trs <- fread("data/report_env_trs_011824.csv")

df_trs[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_trs[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_trs[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_trs[food_group %in% c("othr_ani",
                     "othr_meat",
                     "fruit_veg",
                     "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                       food_group == "othr_meat", "other meat",
                                                       food_group == "fruit_veg", "fruit&veg",
                                                       food_group == "nuts_seeds", "nuts&seeds")]

df_trs[,value := round(value,0)]


# create new dataset with variable 'category' for grouping in plots
df_trs_category <- fread("data/report_env_trs_011824.csv")

df_trs_category[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_trs_category[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_trs_category[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_trs_category[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meat",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]

df_trs_category[,value := round(value,0)]


df_trs_category[, category := fcase(age %in% c("FML", "MLE", "BTH"), "Sex",
                                    age %in% c("low", "medium", "high", "all-e"), "Education",
                                    age %in% c("rural", "urban", "all-u"), "Residence",
                                    age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), "Age")]


# dataset with new variable 'macrofoods'
df_trs_macrof <- fread("data/report_env_trs_011824.csv")

df_trs_macrof[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_trs_macrof[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_trs_macrof[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_trs_macrof[food_group %in% c("othr_ani",
                                  "othr_meat",
                                  "fruit_veg",
                                  "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                                    food_group == "othr_meat", "other meat",
                                                                    food_group == "fruit_veg", "fruit&veg",
                                                                    food_group == "nuts_seeds", "nuts&seeds")]

df_trs_macrof[,macrofoods := fcase(food_group %in% c("beef",
                                                     "lamb",
                                                     "dairy",
                                                     "eggs&fats",
                                                     "other meat",
                                                     "pork",
                                                     "fish"), "ASF",
                                   food_group %in% c("rice",
                                                     "grains",
                                                     "roots"), "Staples",
                                   food_group %in% c("fruit&veg",
                                                     "oils",
                                                     "sugar",
                                                     "legumes",
                                                     "nuts&seeds",
                                                     "other"), "Other",
                                   food_group == "total", "Total")]

df_trs_macrof[,value := round(value,0)]

# new dataset with variables 'category' and 'macrofoods'
df_trs_macrofoods <- fread("data/report_env_trs_011824.csv")

df_trs_macrofoods[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_trs_macrofoods[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_trs_macrofoods[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_trs_macrofoods[food_group %in% c("othr_ani",
                                  "othr_meat",
                                  "fruit_veg",
                                  "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                                    food_group == "othr_meat", "other meat",
                                                                    food_group == "fruit_veg", "fruit&veg",
                                                                    food_group == "nuts_seeds", "nuts&seeds")]

df_trs_macrofoods[,macrofoods := fcase(food_group %in% c("beef",
                                                     "lamb",
                                                     "dairy",
                                                     "eggs&fats",
                                                     "other meat",
                                                     "pork",
                                                     "fish"), "ASF",
                                   food_group %in% c("rice",
                                                     "grains",
                                                     "roots"), "Staples",
                                   food_group %in% c("fruit&veg",
                                                     "oils",
                                                     "sugar",
                                                     "legumes",
                                                     "nuts&seeds",
                                                     "other"), "Other",
                                   food_group == "total", "Total")]

df_trs_macrofoods[, category := fcase(age %in% c("FML", "MLE", "BTH"), "Sex",
                                      age %in% c("low", "medium", "high", "all-e"), "Education",
                                      age %in% c("rural", "urban", "all-u"), "Residence",
                                      age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), "Age")]

df_trs_macrofoods[,value := round(value,0)]

# Create dataframe 'Sel' (age,sex,edu,urb) --------------------------------
df_sel <- fread("data/report_env_sel_011824.csv")

df_sel[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_sel[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_sel[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_sel[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meat",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]
df_sel[,value := round(value,0)]


# Create dataframe 'Trsage' (age-sex-edu-urb-region) ----------------------
df_trs_rgsage <- fread("data/report_env_trs_norgs_011824.csv")

df_trs_rgsage[, env_itm := fcase(
  env_itm %in% names(labels_env_itm), labels_env_itm[env_itm])]

df_trs_rgsage[, dmd_scn := fcase(dmd_scn %in% names(labels_dmd_scn), labels_dmd_scn[dmd_scn])]

df_trs_rgsage[, measure := fcase(measure %in% names(labels_measure), labels_measure[measure])]

df_trs_rgsage[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meat",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]
df_trs_rgsage[,value := round(value,0)]
