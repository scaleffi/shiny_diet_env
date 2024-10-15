library(data.table)
library(tidyverse)
#import with dplyr
csv_file_box <- "data/report_env_box_011824.csv"
df_box <- read_csv(csv_file_box, show_col_types = FALSE)
df_box$value <- round(df_box$value, 0)
df <- df_box %>%
  # include explicit units of measurement
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
      TRUE ~ env_itm  #keep the original value if it doesn't match any condition
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

# df_trs <- read_csv(csv_file_trs, show_col_types = FALSE)
# df_trs$value <- round(df_trs$value, 0)
# df_trs <- df_trs %>%
#   mutate(
#     env_itm = case_when(
#       env_itm == "GHG" ~ "GHG (Mt CO2eq)",
#       env_itm == "water" ~ "water use (km3)",
#       env_itm == "land" ~ "land use (thousands of km2)",
#       env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
#       env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
#       env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
#       env_itm == "avg" ~ "average environmental impact",
#       env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
#       TRUE ~ env_itm
#     ),
#     dmd_scn = case_when(
#       dmd_scn == "actl" ~ "actual demand",
#       dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
#       TRUE ~ dmd_scn
#     ),
#     food_group = case_when(
#       food_group == "othr_ani" ~ "eggs&fats",
#       food_group == "othr_meat" ~ "other meats",
#       food_group == "fruit_veg" ~ "fruit&veg",
#       food_group == "nuts_seeds" ~ "nuts&seeds",
#       TRUE ~ food_group
#     ),
#     measure = case_when(
#       measure == "abs" ~ "absolute",
#       measure == "cap" ~ "per capita",
#       measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
#       measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
#       measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
#       measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
#       TRUE ~ measure
#     )
#   )

# df_sel <- read_csv(csv_file_sel, show_col_types = FALSE)
# df_sel$value <- round(df_sel$value, 0)
# df_sel <- df_sel %>%
#   mutate(
#     env_itm = case_when(
#       env_itm == "GHG" ~ "GHG (Mt CO2eq)",
#       env_itm == "water" ~ "water use (km3)",
#       env_itm == "land" ~ "land use (thousands of km2)",
#       env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
#       env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
#       env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
#       env_itm == "avg" ~ "average environmental impact",
#       env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
#       TRUE ~ env_itm
#     ),
#     dmd_scn = case_when(
#       dmd_scn == "actl" ~ "actual demand",
#       dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
#       TRUE ~ dmd_scn
#     ),
#     food_group = case_when(
#       food_group == "othr_ani" ~ "eggs&fats",
#       food_group == "othr_meat" ~ "other meats",
#       food_group == "fruit_veg" ~ "fruit&veg",
#       food_group == "nuts_seeds" ~ "nuts&seeds",
#       TRUE ~ food_group
#     ),
#     measure = case_when(
#       measure == "abs" ~ "absolute",
#       measure == "cap" ~ "per capita",
#       measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
#       measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
#       measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
#       measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
#       TRUE ~ measure# Keep the original value if it doesn't match any condition
#     )
#   )

# df_trs_rgsage <- read_csv(csv_file_trs_rgsage, show_col_types = FALSE)
# df_trs_rgsage$value <- round(df_trs_rgsage$value, 0)
# df_trs_rgsage <- df_trs_rgsage %>%
#   mutate(
#     env_itm = case_when(
#       env_itm == "GHG" ~ "GHG (Mt CO2eq)",
#       env_itm == "water" ~ "water use (km3)",
#       env_itm == "land" ~ "land use (thousands of km2)",
#       env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
#       env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
#       env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
#       env_itm == "avg" ~ "average environmental impact",
#       env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
#       TRUE ~ env_itm
#     ),
#     dmd_scn = case_when(
#       dmd_scn == "actl" ~ "actual demand",
#       dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
#       TRUE ~ dmd_scn
#     ),
#     food_group = case_when(
#       food_group == "othr_ani" ~ "eggs&fats",
#       food_group == "othr_meat" ~ "other meats",
#       food_group == "fruit_veg" ~ "fruit&veg",
#       food_group == "nuts_seeds" ~ "nuts&seeds",
#       TRUE ~ food_group
#     ),
#     measure = case_when(
#       measure == "abs" ~ "absolute",
#       measure == "cap" ~ "per capita",
#       measure == "pct_abs_WLD" ~ "ratio to global avg (absolute)",
#       measure == "pct_abs_RGS" ~ "ratio to regional avg (absolute)",
#       measure == "pct_cap_RGS" ~ "ratio to regional mean (capita)",
#       measure == "pct_cap_WLD" ~ "ratio to global mean (capita)",
#       TRUE ~ measure# Keep the original value if it doesn't match any condition
#     )
#   )



# Creating a data.table directly
DT <- data.table(id = 1:5, name = c("Alice", "Bob", "Carol", "David", "Eva"), age = c(25, 30, 22, 35, 29))

# Print the data.table
print(DT)

DT[age > 25]. 
DT[,.(name,age)]
DT[,age_group := ifelse(age > 30, "Above 30", "Below 30")]
DT[, .(average_age = mean(age)), by = age_group]
# You can chain operations together
DT_modified <- DT[age > 28][age_group == "Above 30"]

DT[i ,j, by] # syntax of DT




