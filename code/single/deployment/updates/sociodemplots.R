library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(DT)
library(rsconnect)
library(shiny)
library(scales)
#library(shinythemes)
#library(periscope)
library(fmsb)
#library(gghighlight)
library(ggthemes)
library(ggsci)
#library(patchwork)
#library(thematic)
library(plotly)

#logdebug("log", logger = "ss_userAction")

#To inspect the script one section at a time and make debugging easier, navigate to Edit --> Folding --> Collapse all. This will automatically
#nest each subsection within their upper-level section, as defined in the code through the use of # and - symbols. Then simply click on the
#small arrows that appear next to the line numbers, or on buttons with double-sided arrows at the end of each line, to expand the corresponding section of code.

#Prepare the environment for executing the dashboard ----
rm(list = ls()) #clear the environment

#Load files from Github repository ----
# csv_file_trs <- "report_env_trs_110423.csv"
# csv_file_box <- "report_env_box_110423.csv" 
# csv_file_sel <- "report_env_sel_110423.csv"

#Load files from Github repository ----
csv_file_trs <- "report_env_trs_122623.csv"
csv_file_box <- "report_env_box_122623.csv" 
csv_file_sel <- "report_env_sel_122623.csv"


data_box <- read.csv(csv_file_box)
data_box$value <- round(data_box$value, 2)
df <- data_box 

#Rename values in env_itm column to include unit of measurements for each environmental dimension
df <- df %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average env. impact",
    env_itm == "avg_pb" ~ "average env. impact (pb weighted)",
    TRUE ~ env_itm  # Keep the original value if it doesn't match any condition
  ),
  dmd_scn = case_when(
    dmd_scn == "actl" ~ "actual demand",
    dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
    TRUE ~ dmd_scn
  ),
  measure = case_when(
    measure == "abs" ~ "absolute",
    measure == "cap" ~ "per capita",
    measure == "pct_abs_WLD" ~ "ratio to global avg (abs.)",
    measure == "pct_abs_RGS" ~ "ratio to regional avg (abs.)",
    measure == "pct_cap_RGS" ~ "ratio to regional avg. (cap.)",
    measure == "pct_cap_WLD" ~ "ratio to global avg. (cap.)",
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
    age %in% c("0-9", "10-19", "20-39", "40-64", "65+", "all-a") ~ "Age"
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
    env_itm == "avg" ~ "average env. impact",
    env_itm == "avg_pb" ~ "average env. impact (pb weighted)",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (abs.)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (abs.)",
      measure == "pct_cap_RGS" ~ "ratio to regional avg. (cap.)",
      measure == "pct_cap_WLD" ~ "ratio to global avg. (cap.)",
      TRUE ~ measure
    ))

#Create another dataset, data_trs_macrofoods by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
# namely ASF, Staples, Other, Total. This dataset includes a column for the category, and a column for the macrofoods.
data_trs_macrofoods <- data_trs_category %>%
  mutate(macrofoods = case_when(
    food_group %in% c("beef","lamb", "dairy", "othr_ani", "othr_meat", "pork", "fish") ~ "ASF",
    food_group %in% c("rice", "grains", "roots") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds", "other") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

#Create a third dataset, df_trs_macrof, by adding a column labelled 'macrofoods' to the main df dataframe. This dataset only has one additional column, to group macrofoods, if the user is not
#interested in grouping sociodem categories
df_trs_macrof <- df_trs %>%
  mutate(macrofoods = case_when(
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
    env_itm == "avg" ~ "average env. impact",
    env_itm == "avg_pb" ~ "average env. impact (pb weighted)",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (abs.)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (abs.)",
      measure == "pct_cap_RGS" ~ "ratio to regional avg. (cap.)",
      measure == "pct_cap_WLD" ~ "ratio to global avg. (cap.)",
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    ))


data_sel <- read.csv(csv_file_sel)
data_sel$value <- round(data_sel$value, 2)
df_sel <- data_sel

df_sel <- df_sel %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "water use (km3)",
    env_itm == "land" ~ "land use (thousands of km2)",
    env_itm == "land_crop" ~ "land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "average env. impact",
    env_itm == "avg_pb" ~ "average env. impact (pb weighted)",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "actual demand",
      dmd_scn == "norm" ~ "demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
    ),
    measure = case_when(
      measure == "abs" ~ "absolute",
      #measure == "cap" ~ "per capita",
      measure == "pct_abs_WLD" ~ "ratio to global avg (abs.)",
      measure == "pct_abs_RGS" ~ "ratio to regional avg (abs.)",
      #measure == "pct_cap_RGS" ~ "ratio to regional avg. (cap.)",
      #measure == "pct_cap_WLD" ~ "ratio to global avg. (cap.)",
      TRUE ~ measure# Keep the original value if it doesn't match any condition
    ))

#Prepare plotting objects ----

lshtm_theme_few <- function(){
  theme_few() +
    #%+replace%
    theme(
      axis.title.x = element_text(
        vjust = -1,
        size = 12,
        face = "bold"),
      axis.title.y = element_text(size = 12,
                                  face = "bold",
                                  #angle = 90
                                  vjust = 1.5
      ),
      strip.text.x = element_text(size = 12, face = "bold"),
      panel.spacing = unit(0,"lines"),
      strip.text.y = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12,
                                 #angle = 45,
                                 #vjust = 0.5,
                                 hjust = 1
      ),
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, vjust = 1),
      plot.subtitle = element_text(size = 12, face = "bold"),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
      strip.placement = "outside"
    )
}

# #Control shading by setting alpha values through a new vector named alpha_vals; changing the range of shading helps with displaying some images that have several colors.
# alpha_max <- 1
# alpha_min <- 0.7
# alpha_vals <- c(
#   seq(alpha_max, alpha_min, length.out = 8), 
#   seq(alpha_min, alpha_max, length.out = 8)[-1]
# )
# alpha_vals
# 

#create a vector named 'colors_macro' made up of three colors from the Set1 ColorBrewer palette. This can be used to assign specific colors to values in the macrofoods variable.
colors_macro <- c(
  "ASF" = "#922b21",
  "Staples" = "#f1c40f",
  "Other" = "#85929e"
)

#create vectors to assign colors to the sociodem characteristics
colors_sex <- c(
  "FML" = "#E41A1C",
  "MLE" = "#377EB8",
  "BTH" = "#4DAF4A"
)

colors_urban <- c(
  "urban" = "#D95F02",
  "rural" = "#66A61E",
  "all-u" = "#f4d03f"
)

colors_sociodem <- c(
  "urban" = "#D95F02",
  "rural" = "#66A61E",
  "FML" = "#E41A1C",
  "MLE" = "#377EB8",
  "low" = "#fcf3cf",
  "medium" = "#f39c12",
  "high" = "#873600",
  "0-9" = "#ebdef0",
  "10-19" = "#a9cce3",
  "20-39" = "#48c9b0",
  "40-64" = "#229954",
  "65+"= "#7d6608"
)

#Create a vector with specific color assigned to each food group
#Based on _trs_110423
colors_food <- c(
  "total" = "#a6a79b",
  "rice" = "#f9e79f",
  "roots" = "#eb984e",
  "sugar" = "#fad7a0",
  "legumes" = "#6e2c00",
  "beef" = "#cb4335",
  "lamb" = "#ec7063",
  "othr_meat" = "#d98880",
  "pork" = "#f5a5b5",
  "othr_ani" = "#fae5d3",
  "other" = "#fdedec",
  "dairy" = "#f0ebe2",
  "fish" = "#8fbad3",
  "grains" = "#ecdb54",
  "fruit_veg" = "#229954",
  "nuts_seeds" = "#7d6608",
  "oils" = "#abebc6")

# Create a vector to rename facet plots with the full names of the environmental dimensions
env_itm.labs <- c("GHG (Mt CO2eq)", "Freshwater use (km3)", "Eutrophication pot. (Mt PO4eq)", "land use (thousands of km2)", "Land use_pasture (thousands of km2)", "Land use_crops (thousands of km2)")
names(env_itm.labs) <- c("GHG (Mt CO2eq)", "water use (km3)", "eutrophication pot. (kt PO4eq)", "land use (thousands of km2)", "land use, pasture (thousands of km2)", "land use, crops (thousands of km2)")

# env_itm == "GHG" ~ "GHG (kt CO2eq)",
# env_itm == "water" ~ "water use (km3)",
# env_itm == "land" ~ "land use (thousands of km2)",
# env_itm == "land_crop" ~ "Land use,crops (thousands of km2)",
# env_itm == "land_pstr" ~ "Land use,pasture (thousands of km2)",
# env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",


# Create a vector to rename facet plots with the full names of the regions
region.labs <-
  c(
    "World",
    "Low Income",
    "Lower Middle Income",
    "Upper Middle Income",
    "High Income",
    "East Asia and Pacific",
    "Europe & C. Asia",
    "Latin America & Caribbean",
    "Middle East and North Africa",
    "North America",
    "South Asia",
    "Sub-Saharan Africa"
  )
names(region.labs) <-
  c("WLD",
    "LIC",
    "LMC",
    "UMC",
    "HIC",
    "EAS",
    "ECS",
    "LCN",
    "MEA",
    "NAC",
    "SAS",
    "SSF"
  )

custom_order_region <-
  c("WLD",
    "HIC",
    "UMC",
    "LMC",
    "LIC",
    "ECS",
    "MEA",
    "EAS",
    "SAS",
    "NAC",
    "LCN",
    "SSF")

custom_labels_region <-
  c(
    "World",
    "High Income",
    "Upper Middle Income",
    "Lower Middle Income",
    "Low Income",
    "Europe & C. Asia",
    "Middle East & N. Africa",
    "E. Asia & Pacific",
    "South Asia",
    "North America",
    "Latin Am. & Caribbean",
    "Sub-Saharan Africa"
  )

filtered_data <- df_trs_category %>%
    filter(measure == "ratio to global avg. (cap.)",
           env_itm == "average env. impact",
           food_group == "total",
           #food_group == "total",
           #category %in% input$category_8,
           dmd_scn == "actual demand",
           age %in% c("low", "medium", "high"),
           region %in% c("WLD", "HIC", "UMC", "LMC", "LIC")
    )


plot_radar1 <- ggplot(filtered_data,
                      aes(
                        x = region,
                          #factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                        y = value,
                        fill = food_group
                      )) +
  geom_col() +
  coord_polar() +
  facet_wrap(~ age) +
  lshtm_theme_few()

plot_radar1

filtered_data_regionradar <- reactive({
  df %>%
    filter(measure == input$measure_10,
           env_itm %in% input$env_dimensions_10,
           food_group == "total",
           box == "age-sex",
           age.education == "all-a",
           sex.urbanisation == "BTH",
           dmd_scn == input$dmd_scn_10,
           region %in% input$region_10
    )
})  

p_regionradar <- plot_ly(
  type = 'scatterpolar',
  r = data$value,
  theta = colnames(data),
  fill = 'toself',
  
)