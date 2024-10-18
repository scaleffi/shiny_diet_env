# Header ------------------------------------------------------------------
# Author: Sebastiano Caleffi (github: scaleffi)


# Load libraries ----------------------------------------------------------
library(tidyverse)
#library(readr) # for the read_csv function, faster than the base read.csv
library(data.table)

# Create dataframe 'Box' (age-sex and edu-urb) ----------------------------
df <- fread("data/report_env_box_011824.csv")
df[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                        env_itm == "water","water use (km3)",
                        env_itm == "land", "land use (thousands of km2)",
                        env_itm == "land_crop", "land use, crops (thousands of km2)",
                        env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                        env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                        env_itm == "avg", "average environmental impact",
                        env_itm == "avg_pb", "average environmental impact (pb weighted)")]
df[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                        dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]
df[food_group %in% c("othr_ani",
                        "othr_meat",
                        "fruit_veg",
                        "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                          food_group == "othr_meat", "other meats",
                                                          food_group == "fruit_veg", "fruit&veg",
                                                          food_group == "nuts_seeds", "nuts&seeds")]
df[,measure := fcase(measure == "abs", "absolute",
                        measure == "cap", "per capita",
                        measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                        measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                        measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                        measure == "pct_cap_WLD", "ratio to global mean (capita)")]
df[,value := round(value,0)]
setnames(df, old = c("age-education", "sex-urbanisation"), new = c("age.education", "sex.urbanisation"))


# Create dataframe 'Trs' (age-sex-edu-urb) --------------------------------
df_trs <- fread("data/report_env_trs_011824.csv")
df_trs[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                     env_itm == "water","water use (km3)",
                     env_itm == "land", "land use (thousands of km2)",
                     env_itm == "land_crop", "land use, crops (thousands of km2)",
                     env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                     env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                     env_itm == "avg", "average environmental impact",
                     env_itm == "avg_pb", "average environmental impact (pb weighted)")]

df_trs[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                     dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]

df_trs[food_group %in% c("othr_ani",
                     "othr_meat",
                     "fruit_veg",
                     "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                       food_group == "othr_meat", "other meats",
                                                       food_group == "fruit_veg", "fruit&veg",
                                                       food_group == "nuts_seeds", "nuts&seeds")]

df_trs[,measure := fcase(measure == "abs", "absolute",
                     measure == "cap", "per capita",
                     measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                     measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                     measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                     measure == "pct_cap_WLD", "ratio to global mean (capita)")]

df_trs[,value := round(value,0)]


# create new dataset with variable 'category' for grouping in plots

df_trs_category <- fread("data/report_env_trs_011824.csv")

df_trs_category[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                         env_itm == "water","water use (km3)",
                         env_itm == "land", "land use (thousands of km2)",
                         env_itm == "land_crop", "land use, crops (thousands of km2)",
                         env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                         env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                         env_itm == "avg", "average environmental impact",
                         env_itm == "avg_pb", "average environmental impact (pb weighted)")]

df_trs_category[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                         dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]

df_trs_category[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meats",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]

df_trs_category[,measure := fcase(measure == "abs", "absolute",
                         measure == "cap", "per capita",
                         measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                         measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                         measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                         measure == "pct_cap_WLD", "ratio to global mean (capita)")]

df_trs_category[, category := fcase(age %in% c("FML", "MLE", "BTH"), "Sex",
                                              age %in% c("low", "medium", "high", "all-e"), "Education",
                                              age %in% c("rural", "urban", "all-u"), "Residence",
                                              age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), "Age")]
df_trs_category[,value := round(value,0)]

# dataset with new variable 'macrofoods'

df_trs_macrof <- fread("data/report_env_trs_011824.csv")

df_trs_macrof[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                                  env_itm == "water","water use (km3)",
                                  env_itm == "land", "land use (thousands of km2)",
                                  env_itm == "land_crop", "land use, crops (thousands of km2)",
                                  env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                                  env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                                  env_itm == "avg", "average environmental impact",
                                  env_itm == "avg_pb", "average environmental impact (pb weighted)")]

df_trs_macrof[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                                  dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]

df_trs_macrof[food_group %in% c("othr_ani",
                                  "othr_meat",
                                  "fruit_veg",
                                  "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                                    food_group == "othr_meat", "other meats",
                                                                    food_group == "fruit_veg", "fruit&veg",
                                                                    food_group == "nuts_seeds", "nuts&seeds")]

df_trs_macrof[,measure := fcase(measure == "abs", "absolute",
                                  measure == "cap", "per capita",
                                  measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                                  measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                                  measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                                  measure == "pct_cap_WLD", "ratio to global mean (capita)")]

df_trs_macrof[,macrofoods := fcase(food_group %in% c("beef",
                                                                            "lamb",
                                                                            "dairy",
                                                                            "eggs&fats",
                                                                            "other meats","pork",
                                                                            "fish"), "ASF",
                                                          food_group %in% c("rice", "grains", "roots"), "Staples",
                                                          food_group %in% c("fruit&veg", "oils", "sugar",
                                                                            "legumes", "nuts&seeds", "other"), "Other",
                                                          food_group == "total", "Total")]

df_trs_macrof[,value := round(value,0)]

# new dataset with variables 'category' and 'macrofoods'
df_trs_macrofoods <- fread("data/report_env_trs_011824.csv")

df_trs_macrofoods[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                                  env_itm == "water","water use (km3)",
                                  env_itm == "land", "land use (thousands of km2)",
                                  env_itm == "land_crop", "land use, crops (thousands of km2)",
                                  env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                                  env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                                  env_itm == "avg", "average environmental impact",
                                  env_itm == "avg_pb", "average environmental impact (pb weighted)")]

df_trs_macrofoods[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                                  dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]

df_trs_macrofoods[food_group %in% c("othr_ani",
                                  "othr_meat",
                                  "fruit_veg",
                                  "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                                    food_group == "othr_meat", "other meats",
                                                                    food_group == "fruit_veg", "fruit&veg",
                                                                    food_group == "nuts_seeds", "nuts&seeds")]

df_trs_macrofoods[,measure := fcase(measure == "abs", "absolute",
                                  measure == "cap", "per capita",
                                  measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                                  measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                                  measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                                  measure == "pct_cap_WLD", "ratio to global mean (capita)")]

df_trs_macrofoods[,macrofoods := fcase(food_group %in% c("beef",
                                                                            "lamb",
                                                                            "dairy",
                                                                            "eggs&fats",
                                                                            "other meats","pork",
                                                                            "fish"), "ASF",
                                                          food_group %in% c("rice", "grains", "roots"), "Staples",
                                                          food_group %in% c("fruit&veg", "oils", "sugar",
                                                                            "legumes", "nuts&seeds", "other"), "Other",
                                                          food_group == "total", "Total")]

df_trs_macrofoods[, category := fcase(age %in% c("FML", "MLE", "BTH"), "Sex",
                                      age %in% c("low", "medium", "high", "all-e"), "Education",
                                      age %in% c("rural", "urban", "all-u"), "Residence",
                                      age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), "Age")]

df_trs_macrofoods[,value := round(value,0)]

# Create dataframe 'Sel' (age,sex,edu,urb) --------------------------------
df_sel <- fread("data/report_env_sel_011824.csv")
df_sel[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                         env_itm == "water","water use (km3)",
                         env_itm == "land", "land use (thousands of km2)",
                         env_itm == "land_crop", "land use, crops (thousands of km2)",
                         env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                         env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                         env_itm == "avg", "average environmental impact",
                         env_itm == "avg_pb", "average environmental impact (pb weighted)")]
df_sel[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                         dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]
df_sel[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meats",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]
df_sel[,measure := fcase(measure == "abs", "absolute",
                         measure == "cap", "per capita",
                         measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                         measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                         measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                         measure == "pct_cap_WLD", "ratio to global mean (capita)")]
df_sel[,value := round(value,0)]


# Create dataframe 'Trsage' (age-sex-edu-urb-region) ----------------------
df_trs_rgsage <- fread("data/report_env_trs_norgs_011824.csv")
df_trs_rgsage[,env_itm := fcase(env_itm == "GHG","GHG (Mt CO2eq)",
                         env_itm == "water","water use (km3)",
                         env_itm == "land", "land use (thousands of km2)",
                         env_itm == "land_crop", "land use, crops (thousands of km2)",
                         env_itm == "land_pstr", "land use, pasture (thousands of km2)",
                         env_itm == "eutr", "eutrophication pot. (kt PO4eq)",
                         env_itm == "avg", "average environmental impact",
                         env_itm == "avg_pb", "average environmental impact (pb weighted)")]
df_trs_rgsage[,dmd_scn := fcase(dmd_scn == "actl", "actual demand",
                         dmd_scn == "norm", "demand normalised to 2,000 kcal/day")]
df_trs_rgsage[food_group %in% c("othr_ani",
                         "othr_meat",
                         "fruit_veg",
                         "nuts_seeds"),food_group := fcase(food_group == "othr_ani", "eggs&fats",
                                                           food_group == "othr_meat", "other meats",
                                                           food_group == "fruit_veg", "fruit&veg",
                                                           food_group == "nuts_seeds", "nuts&seeds")]
df_trs_rgsage[,measure := fcase(measure == "abs", "absolute",
                         measure == "cap", "per capita",
                         measure == "pct_abs_WLD", "ratio to global avg (absolute)",
                         measure == "pct_abs_RGS", "ratio to regional avg (absolute)",
                         measure == "pct_cap_RGS", "ratio to regional mean (capita)",
                         measure == "pct_cap_WLD", "ratio to global mean (capita)")]
df_trs_rgsage[,value := round(value,0)]