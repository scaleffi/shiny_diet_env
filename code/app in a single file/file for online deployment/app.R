library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(DT)
library(rsconnect)
library(shiny)
#library(shinythemes)
#library(periscope)

#library(gghighlight)
#library(ggthemes)
#library(patchwork)
#library(thematic)
#library(plotly)

#logdebug("log", logger = "ss_userAction")

#To inspect the script one section at a time and make debugging easier, navigate to Edit --> Folding --> Collapse all. This will automatically
#nest each subsection within their upper-level section, as defined in the code through the use of # and - symbols. Then simply click on the
#small arrows that appear next to the line numbers, or on buttons with double-sided arrows at the end of each line, to expand the corresponding section of code.

#Prepare the environment for executing the dashboard ----
rm(list = ls()) #clear the environment

#Load files from Github repository ----
#csv_file_trs <- "report_env_trs_053123.csv"
csv_file_trs <- "report_env_trs_110423.csv"
csv_file_box <- "report_env_box_110423.csv" 

data_box <- read.csv(csv_file_box)
data_box$value <- round(data_box$value, 2)
df <- data_box 

#Rename values in env_itm column to include unit of measurements for each environmental dimension
df <- df %>%
  mutate(env_itm = case_when(
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "Water use (thousands of km3)",
    env_itm == "land" ~ "Land use (thousands of km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm  # Keep the original value if it doesn't match any condition
  ),
          dmd_scn = case_when(
    dmd_scn == "actl" ~ "Actual Demand",
    dmd_scn == "norm" ~ "Demand normalised to 2,000 kcal/day",
    TRUE ~ dmd_scn
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
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "Water use (thousands of km3)",
    env_itm == "land" ~ "Land use (thousands of km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "Actual Demand",
      dmd_scn == "norm" ~ "Demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
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
    env_itm == "GHG" ~ "GHG (Mt CO2eq)",
    env_itm == "water" ~ "Water use (thousands of km3)",
    env_itm == "land" ~ "Land use (thousands of km2)",
    env_itm == "land_crop" ~ "Land use, crops (thousands of km2)",
    env_itm == "land_pstr" ~ "Land use, pasture (thousands of km2)",
    env_itm == "eutr" ~ "Eutrophication pot. (kt PO4eq)",
    env_itm == "avg" ~ "Average",
    TRUE ~ env_itm),
    dmd_scn = case_when(
      dmd_scn == "actl" ~ "Actual Demand",
      dmd_scn == "norm" ~ "Demand normalised to 2,000 kcal/day",
      TRUE ~ dmd_scn
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


# data_cons <- read.csv(csv_file_cons)
# data_cons$Intake <- round(data_cons$Intake, 2)
# df_cons <- data_cons[-7]
# 
# data_FBSintake <- read.csv(csv_file_FBSintake)
# data_FBSintake$Value <- round(data_FBSintake$Value, 2)
# df_FBSintake <- data_FBSintake



#UI ----

ui <- dashboardPage(skin = "black",
  dashboardHeader(
    title = "The environmental footprints of global diets",
    titleWidth = 450
  ),
  # titlePanel("The environmental footprints of global diets"),----
  dashboardSidebar(
    sidebarMenu(
               menuItem("Compare by socio-demographic", tabName = "sociodem", icon = icon("person-half-dress")),
               menuItem("Compare by region", tabName = "food_groups", icon = icon("earth-africa")),
               menuItem("Compare by food group", tabName = "categories", icon = icon("wheat-awn")),
               menuItem("ReadMe", tabName = "readme", icon = icon("info-circle")
                        , selected = TRUE
                        ) 
    ), collapsed = FALSE
  ),
  # mainPanel(
  dashboardBody(
    tabItems(
      ###First item----
      tabItem(
        tabName = "sociodem",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data", HTML(
              "These tabs allow you to compare diet-related environmental footprints across different sociodemographics.<br><br>
                      Use the first tab, 'Sex-age', if you want to explore how diet-related environmental footprints differ among sexes and age groups. Open the second tab instead, 'Edu-urb', if you want
                      to explore how the footprints change across education and urbanisation levels. <br><br>
                      If you are interested in seeing how absolute and per capita impacts differ across sociodemographics, regions, and environmental dimensions, open the third tab, 'Sociodem'.")
          )),
          tabPanel(
            #Sexage----
            "Sex-age",
            fluidPage(title = "Compare footprints by sex and age group",
                      box(width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                        fluidRow(
                          column(4,
                               selectInput("dmd_scn_2", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                               selectInput("measure_2", "Select Measure:", choices = c("Ratio to World average (capita)","Ratio to Regional average (capita)"), selected = "Ratio to World average (capita)")
                        ),
                          column(4,
                               selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "Average"),
                               selectInput("region_2", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                        ),
                          column(4,
                               selectInput("age.education_2", "Select age group:", choices = c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-9", "10-19", "20-39", "40-64", "65+")),
                               selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML")),
                               downloadButton("download_csv_sexage", "Download table"),
                               downloadButton("download_plot_sexage", "Download plot")
                        )
                      )
                      ),
                      box(
                        width = 12, title = "Output" , collapsible = T, solidHeader = TRUE, status = "primary",
                        tabsetPanel(
                          tabPanel("Plot", plotOutput("plot_sexage"
                                                      #, height = 400
                          )),
                          tabPanel("Table",tableOutput("sexage_table"))
                        )
                      )
                    )
          ),
          tabPanel(
            #Eduurb ----
            "Edu-urb",
            fluidPage(title = "Compare footprints by urbanisation and education levels",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_3", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_3", "Select Measure:", choices = c("Ratio to World average (capita)","Ratio to Regional average (capita)"), selected = "Ratio to World average (capita)")
                        ),
                 column(4,
                        selectInput("env_dimensions_3", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "Average"),
                        selectInput("region_3", "Select Region:", choices = unique(df$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                        ),
                 column(4,
                        selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high"), multiple = TRUE, selected = c("low", "medium", "high")),
                        selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural"), multiple = TRUE, selected = c("urban", "rural")),
                        downloadButton("download_csv_eduurb", "Download table"),
                        downloadButton("download_plot_eduurb", "Download plot")
                        )
                      )
                ),
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_eduurb"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("eduurb_table"))
                )
              )
            )
          ),
          tabPanel(
            #Sociodem ----
            "Sociodem",
            fluidPage(title = "Compare footprints across multiple dimensions",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_7", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_7", "Select Measure:", choices = c("Absolute","Per capita"), selected = "Per capita"),
                        downloadButton("download_csv_sociodem", "Download table"),
                        downloadButton("download_plot_sociodem", "Download plot")
                        ),
                 column(4,
                        selectInput("env_dimensions_7", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), "Average"), selected = "GHG (Mt CO2eq)"),
                        selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = c(
                          "beef_lamb",
                          "dairy",
                          "pork",
                          "othr_meat",
                          "fish",
                          "othr_ani",
                          "rice",
                          "grains",
                          "fruit_veg",
                          "oils",
                          "sugar",
                          "roots",
                          "legumes",
                          "nuts_seeds",
                          "other"))
                        ),
                 column(4,
                        selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                        selectInput("age_7", "Select sociodemographic:", choices = setdiff(unique(df_trs_category$age), c("all-a", "all-e", "BTH", "all-u")), multiple = TRUE, selected = c(
                          "low",
                          "medium",
                          "high",
                          "urban",
                          "rural",
                          "FML",
                          "MLE",
                          "0-9",
                          "10-19",
                          "20-39",
                          "40-64",
                          "65+"))
                 )
               )
              ),  
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_sociodem"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("sociodem_table"))
                  
                )
              )
            )
          )
        )
      ),
      tabItem(
        ###Second item ----
        tabName = "food_groups",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data",HTML(
              "These tabs allow you to compare absolute and per capita diet-related environmental footprints of diets across income regions and geographical regions.<br><br>
                      In both tabs, you can see how much each food group contributes to the environmental footprint across six different dimensions: GHG (Mt CO2eq) emissions, Eutrophication, Land Use, Land Use - Pasture, Land Use - Crops,
                      Freshwater Withdrawals.<br><br> If you want to 
                      compare across income regions (Low Income Countries, Low-Middle Income Countries, Upper-Middle Income Countries, High Income Countries), open the first tab. Explore the second tab instead
                      if your focus is on geographical groupings."
            )
          )),
          tabPanel(
            #Region ----
            "Impacts by income region",
            fluidPage(title = "Compare footprints across income regions",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_1", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_1", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita")
                        ),
                 column(4,
                        selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"),multiple = TRUE, selected = c(
                          "GHG (Mt CO2eq)",
                          "Land use (thousands of km2)",
                          "Water use (thousands of km3)",
                          "Eutrophication pot. (kt PO4eq)"
                        )),
                        selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                        ),
                 column(4,
                        selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c(
                          "beef_lamb",
                          "dairy",
                          "pork",
                          "othr_meat",
                          "fish",
                          "othr_ani",
                          "rice",
                          "grains",
                          "fruit_veg",
                          "oils",
                          "sugar",
                          "roots",
                          "legumes",
                          "nuts_seeds",
                          "other"
                        )),
                        downloadButton("download_csv_region", "Download table"),
                        downloadButton("download_plot_region", "Download plot")
                 )
               )
              ),
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_region"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("region_table"))
                )
              )
            )
          ),
          tabPanel(
            #Regiongeo ----
            "Impacts by geographical region",
            fluidPage(title = "Compare footprints across geographical regions",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_5", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_5", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita")
                        ),
                 column(4,
                        selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"),multiple = TRUE, selected = c(
                          "GHG (Mt CO2eq)",
                          "Land use (thousands of km2)",
                          "Water use (thousands of km3)",
                          "Eutrophication pot. (kt PO4eq)"
                        )),
                        selectInput("region_5", "Select Region:", choices = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"), multiple = TRUE, selected = c(
                          "WLD",
                          "NAC",
                          "SAS",
                          "SSF",
                          "LCN",
                          "ECS",
                          "MEA",
                          "EAS"
                        ))
                        ),
                 column(4,
                        selectInput("food_group_5", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c(
                          "beef_lamb",
                          "dairy",
                          "pork",
                          "othr_meat",
                          "fish",
                          "othr_ani",
                          "rice",
                          "grains",
                          "fruit_veg",
                          "oils",
                          "sugar",
                          "roots",
                          "legumes",
                          "nuts_seeds",
                          "other"
                        )),
                        downloadButton("download_csv_regiongeo", "Download table"),
                        downloadButton("download_plot_regiongeo", "Download plot")
                 )
               )
              ),
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_regiongeo"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("regiongeo_table"))
                )
              )  
            )
          )
        )
      ),
      tabItem(
        ###Third item ----
        tabName = "categories",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data", HTML(
              "These tabs allow you to compare absolute and per capita environmental footprints associated with Demand of fifteen food groups and three macrocategories, across regions.<br><br>
                      We grouped the fifteen food groups into three macrocategories: Animal Source Foods (ASF), Staples, and Other. This is an arbitrary classification made for the
                      purpose of this dashboard, with the goal of making it easier for the user to make comparisons at a glance. By combining information on both the food group
                      and the macrocategory, the user can explore how different categories of food impact each environmental dimension, while also seeing how individual food groups contribute within each category.<br><br>
                      Open the first tab if you want to focus on the fifteen separate food groups, and compare their impacts across regions. Select the second tab if instead you want to focus on the three macrocategories."
            )
          )),
          tabPanel(
            #Category ----
            "Impacts by food group",
            fluidPage(title = "Compare footprints by individual food groups",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_4", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_4", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita"),
                        ),
                 column(4,
                        selectInput("env_dimensions_4", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"), selected = "GHG (Mt CO2eq)"),
                        selectInput("region_4", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                        ),
                 column(4,
                        selectInput("food_group_4", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c(
                          "beef_lamb",
                          "dairy",
                          "pork",
                          "othr_meat",
                          "fish",
                          "othr_ani",
                          "rice",
                          "grains",
                          "fruit_veg",
                          "oils",
                          "sugar",
                          "roots",
                          "legumes",
                          "nuts_seeds",
                          "other"
                        )),
                        downloadButton("download_csv_category", "Download table"),
                        downloadButton("download_plot_category", "Download plot")
                 )
               )
              ), 
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_category"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("category_table"))
                )
              )
            )
        ),
          tabPanel(
            #Categorymacro ----
            "Impacts by food category",
            fluidPage(title = "Compare footprints by food macro-categories",
              box(
                width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
               fluidRow(
                 column(4,
                        selectInput("dmd_scn_6", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "Actual Demand"),
                        selectInput("measure_6", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita")
                        ),
                 column(4,
                        selectInput("env_dimensions_6", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("Average", "Land use, pasture (thousands of km2)", "Land use, crops (thousands of km2)")), selected = "GHG (Mt CO2eq)"),
                        selectInput("region_6", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                        ),
                 column(4,
                        selectInput("food_group_6", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c(
                          "beef_lamb",
                          "dairy",
                          "pork",
                          "othr_meat",
                          "fish",
                          "othr_ani",
                          "rice",
                          "grains",
                          "fruit_veg",
                          "oils",
                          "sugar",
                          "roots",
                          "legumes",
                          "nuts_seeds",
                          "other"
                        )),
                        downloadButton("download_csv_categorymacro", "Download table"),
                        downloadButton("download_plot_categorymacro", "Download plot")
                 )
               )
              ),
              box(
                width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                tabsetPanel(
                  tabPanel("Plot", plotOutput("plot_categorymacro"
                                              #, height = 400
                  )),
                  tabPanel("Table",tableOutput("categorymacro_table"))
                )
              )
            )
          )
        )
      ),
      tabItem(
        ###Fourth item ----
        tabName = "readme",
        fluidRow(
          box(
            title = "ReadMe",
            div(
              HTML(
                        "This dashboard allows you to explore data on the environmental footprints of global diets. 
                        It uses new datasets and estimates, developed by Prof. Marco Springmann at the London School of Hygiene and Tropical Medicine.<br><br> 
                        The data can be filtered by region, country, and by characteristics such as Sex, Age Group, Education Level, etc.
                        To better manage the level of detail available in these new estimates, we have divided the dashboard in 
                        distinct sections that focus on a specific number of dimensions at a time. Within each section,
                        the user is free to choose among several combinations of filters and generate plots of their interest.
                        All data can be visualised and downloaded as both a plot or a table. Plots and tables are reproducible, as long as their source is clearly and correctly cited.<br><br>
                        For feedback or
                        questions please contact Sebastiano Caleffi at sebastiano.caleffi@lshtm.ac.uk"
              )
              #style = "width:100%;"
              
            )
          )
        )
      )
    )
  )
)

#SERVER ----

server <- function(input, output) {
  
  #Create filters----
  #NOTE: The filters need to be coherent with the options for data selection given to users in the UI using the command 'selectInput'. If the user is
  #given the freedom to choose specific inputs in the UI, but then the corresponding filters are not set appropriately in the Server, 
  #the app may work but it won't display any plots, or the plots may be empty. So if plots are not showing or not showing correctly, first ensure that
  #the inputs available to the user in the UI correctly match the corresponding filters in the Server function. Users should be given
  #the option to only choose inputs that are included in the subset created for that tab through the filters.
  
  
  
  ##Create filters for tabs in the first menu item, 'compare by sociodemographic'.---- 
  
  #Note on filter notation: 
  #The notation == means that for the variable x, the app will take as input the specific value selected for that variable by the user through the UI interface. It implies that the user can select only one value at a time for that variable.
  #The notation %in% means that for the variable x, the app will take as input any values selected for that variable by the user through the UI interface. It implies that the user can select multiple values within that variable.
  
  filtered_data_sexage <- reactive({
    # For debugging purposes: Print out the values of the reactive function to check that they are as expected 
    # print(input$dmd_scn_2)
    # print(input$measure_2)
    # print(input$env_dimensions_2)
    # print(input$region_2)
    # print(input$age.education_2)
    # print(input$sex.urbanisation_2)
    
    df %>%
      filter(measure == input$measure_2,
             env_itm %in% input$env_dimensions_2,
             food_group == "total",
             box == "age-sex",
             age.education %in% input$age.education_2,
             sex.urbanisation %in% input$sex.urbanisation_2,
             dmd_scn == input$dmd_scn_2,
             region %in% input$region_2)
  })
  
  filtered_data_eduurb <- reactive({
    df %>%
      filter(measure == input$measure_3,
             env_itm %in% input$env_dimensions_3,
             food_group == "total",
             box == "edu-urb",
             age.education %in% input$age.education_3,
             sex.urbanisation %in% input$sex.urbanisation_3,
             dmd_scn == input$dmd_scn_3,
             region %in% input$region_3)
  })
  
  filtered_data_sociodem <- reactive({
    df_trs_category %>%
      filter(measure == input$measure_7,
             env_itm %in% input$env_dimensions_7,
             food_group %in% input$food_group_7,
             age %in% input$age_7,
             dmd_scn == input$dmd_scn_7,
             region %in% input$region_7)
  })
  
  
  ##Create filters for tabs in the second menu item, 'compare by region'----
  
  filtered_data_region <- reactive({
    df %>%
      filter(measure == input$measure_1,
             env_itm %in% input$env_dimensions_1,
             food_group %in% input$food_group_1,
             box == "age-sex",
             age.education == "all-a",
             sex.urbanisation == "BTH",
             dmd_scn == input$dmd_scn_1,
             region %in% input$region_1)
  })
  
  filtered_data_regiongeo <- reactive({
    df %>%
      filter(measure == input$measure_5,
             env_itm %in% input$env_dimensions_5,
             food_group %in% input$food_group_5,
             box == "age-sex",
             age.education == "all-a",
             sex.urbanisation == "BTH",
             dmd_scn == input$dmd_scn_5,
             region %in% input$region_5)
  })
  
  
  ##Create filters for tabs in the third menu item, 'compare by categories'----
  
  filtered_data_category <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_4,
             env_itm %in% input$env_dimensions_4,
             food_group %in% input$food_group_4,
             age == "all-a",
             dmd_scn == input$dmd_scn_4,
             region %in% input$region_4,
             #macrofoods %in% c("ASF", "Staples", "Other")
      )
  })
  
  filtered_data_categorymacro <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_6,
             env_itm %in% input$env_dimensions_6,
             food_group %in% input$food_group_6,
             age == "all-a",
             dmd_scn == input$dmd_scn_6,
             region %in% input$region_6
             #macrofoods %in% c("ASF", "Staples", "Other")
      )
  })
  
  #Prepare graphic objects and labels that will be used to create the plots below ----
  
  
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
    "rural" = "#66A61E"
  )
  
  #Create a vector with specific color assigned to each food group
   #Based on _trs_110423
   colors_food <- c(
     "total" = "#a6a79b",
     "rice" = "#f9e79f",
     "roots" = "#eb984e",
     "sugar" = "#fad7a0",
     "legumes" = "#6e2c00",
     "beef_lamb" = "#cb4335",
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
  env_itm.labs <- c("GHG (Mt CO2eq)", "Freshwater use (thousands of km3)", "Eutrophication pot. (Mt PO4eq)", "Land use (thousands of km2)", "Land use_pasture (thousands of km2)", "Land use_crops (thousands of km2)")
  names(env_itm.labs) <- c("GHG (Mt CO2eq)", "Water use (thousands of km3)", "Eutrophication pot. (kt PO4eq)", "Land use (thousands of km2)", "Land use, pasture (thousands of km2)", "Land use, crops (thousands of km2)")
  
  # env_itm == "GHG" ~ "GHG (kt CO2eq)",
  # env_itm == "water" ~ "Water use (thousands of km3)",
  # env_itm == "land" ~ "Land use (thousands of km2)",
  # env_itm == "land_crop" ~ "Land use,crops (thousands of km2)",
  # env_itm == "land_pstr" ~ "Land use,pasture (thousands of km2)",
  # env_itm == "eutr" ~ "Eutrophication pot. (kt PO4eq)",
  
  
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
  
  
  ##Draw plots for tabs in the first item (sociodem) ----

#The dashboard works if the plots are directly called using the renderPlot call, which means that R generates a new plot dynamically
#each time the inputs change. HOWEVER, this means the plot object is never stored anywhere in R's working memory/environment. This makes
#it impossible to download the plot, because R doesn't accept the result of a renderPlot call as the input to create a file for download.
#I suspect this creates other issues as well, so in keeping with best practice I've seen online, since 16/11/2023 I've changed the code
#so that the plots are first created as a reactive object and temporarily stored in R's working environment, AND ONLY THEN they are
#rendered using the renderPlot call. This makes it possible to download the plots (an issue I have been trying to fix for weeks, if not
#months), and likely streamlines the execution of the code as well.
  reactive_plot_sexage <- reactive({
    data <- filtered_data_sexage()
    
    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
    
    p_sexage <- ggplot(
      data,
      aes(
        x = age.education,
        y = value,
        color = sex.urbanisation,
      )
    ) +
      geom_point(size = 3) +
      scale_color_manual(values = colors_sex,
                        breaks = c("MLE",
                                   "FML",
                                   "BTH"),
                        labels = c("Male",
                                   "Female",
                                   "Both"),
                        name = "Sex:") +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom, ncol = 5,
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
                  )+
      geom_hline(yintercept = 100, alpha = 0.3) +
      theme_linedraw() +
      labs(x = "Age group", y = "Diet-related env. impact expressed relative\nto global average  (100 = world average)") +
      theme(
        axis.title.x = element_text(vjust = -1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  output$plot_sexage <- renderPlot({
    print(reactive_plot_sexage())
  })
  
  reactive_plot_eduurb <- reactive({
    data <- filtered_data_eduurb()
    
    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
    
    p_eduurb <- ggplot(data,
           aes(
             x = factor(age.education, level = c("low", "medium", "high")),
             y = value,
             color = sex.urbanisation,
           )) +
      geom_point(size = 3) +
      scale_color_manual(values = colors_urban,
                         breaks = c("urban",
                                    "rural"
                                    ),
                         labels = c("Urban",
                                    "Rural"
                                    ),
                         name = "Urbanisation:") +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom, ncol = 5, 
                  labeller = labeller(region_custom = label_wrap_gen(width = 15)) 
                  ) +
      geom_hline(yintercept = 100, alpha = 0.3) +
      labs(x = "Education level", y = "Diet-related env. impact expressed relative\nto global average  (100 = world average)") +
      theme_linedraw() +
      theme(
        axis.title.x = element_text(vjust = -1, size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1.5),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
    return(p_eduurb)
    #theme(axis.title.x = element_text(vjust = -1),axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
    #theme(plot.title=element_text(hjust = 0.5, size = 20), axis.title.x = element_text(face = "bold"), strip.text = element_text(size=12), legend.position = "top", legend.text = element_text(size = 15), axis.text.x = element_text(face = "bold"))
  })
  
  output$plot_eduurb <- renderPlot({
    print(reactive_plot_eduurb())
  })
  
  
  reactive_plot_sociodem <- reactive({
    data <- filtered_data_sociodem()
    
    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
    
    selected_env_itm <- input$env_dimensions_7
    
    p_sociodem <- ggplot(data, 
                aes(
      x = factor(
        age,
        level = c(
          "MLE",
          "FML",
          "0-9",
          "10-19",
          "20-39",
          "40-64",
          "65+",
          "low",
          "medium",
          "high",
          "urban",
          "rural"
        )
      ),
      y = value,
      label = value
    )) +
      #fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
      geom_col(aes(fill = factor(
        food_group,
        level = c(
          "beef_lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )), color = "white") +
      #facet_wrap(~ factor(region, levels=c("LIC","LMC","UMC","HIC","ECS","MEA","EAS","SAS","NAC","LCN","SSF","WLD")),ncol = 4) +
      
      #coord_flip() is an easy way to swtich the x and y axis. Depending on what we want the user to focus on, each vis has its advantages. To see
      #the graph with the impacts on the y axis and the sociodem/age variables on the x axis, comment the coord_flip() call, AND switch the arguments in facet_grid to scales = "free_x", space = "free", switch = "x".
      coord_flip() +
      facet_grid(category ~ region_custom, scales = "free_y", space = "free", switch = "y",
                 labeller = labeller(region_custom = label_wrap_gen(width = 15),
                                     category = label_wrap_gen(width = 5))
                 ) +
      theme_linedraw() +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      #scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      scale_fill_manual(values = colors_food) +
      labs(x = NULL, y = paste(selected_env_itm), fill = "Food group") +
      scale_y_continuous(guide = guide_axis(n.dodge=2)) +
      theme(axis.text.x = element_text(size=12), 
            axis.title.y = element_text(size = 12, face = "bold"),
            axis.title.x = element_text(size = 12, face = "bold"),
            strip.text.x = element_text(size = 12, face = "bold"),
            strip.text.y = element_text(size = 12, face = "bold"),
            axis.text.y = element_text(size = 12),
            legend.position = "right",
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"))
    return(p_sociodem)
  })
  
  output$plot_sociodem <- renderPlot({
    print(reactive_plot_sociodem())
  })
  
  ##Draw plots for tabs in the second item (region) ----
  
  
  
  reactive_plot_region <- reactive({
    data <- filtered_data_region()
    p_region <- ggplot(data, aes(
      x = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef_lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      scale_fill_manual(values = colors_food) +
      scale_x_discrete(
        labels = c(
          "World",
          "High Income",
          "Upper Middle Income",
          "Lower Middle Income",
          "Low Income"
        )
      ) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      facet_wrap(
        ~ env_itm,
        scales = "free_y",
        ncol = 4,
        labeller = labeller(env_itm = label_wrap_gen(width = 20))
      ) +
      labs(x = "Income Region" , y = "Impact",
           fill = "Food group") +
      theme_linedraw() +
      theme(
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1),
        axis.title.x = element_text(size = 12, face = "bold", vjust = -0.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(
          size = 12,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  output$plot_region <- renderPlot({
    print(reactive_plot_region())
  })
  
  reactive_plot_regiongeo <- reactive({
    data <- filtered_data_regiongeo()
    p_regiongeo <- ggplot(data, aes(
      x = factor(
        region,
        level = c(
          "WLD", 
          "NAC", 
          "ECS", 
          "LCN", 
          "MEA",
          "EAS",
          "SAS", 
          "SSF"
        )
      ),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef_lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      scale_fill_manual(values = colors_food) +
      scale_x_discrete(
        breaks = c("WLD", "NAC", "ECS", "LCN", "MEA", "EAS", "SAS", "SSF"),
        labels = c(
          "World",
          "North America",
          "Europe & C. Asia",
          "Latin Am. & Caribbean",
          "Middle East & N. Africa",
          "E. Asia & Pacific",
          "South Asia",
          "Sub-Saharan Africa"
        )
      ) +
      facet_wrap(
        ~ env_itm,
        scales = "free_y",
        ncol = 4,
        labeller = labeller(env_itm = label_wrap_gen(width = 20))
      ) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      labs(x = "Geographical Region" , y = "Impact",
           fill = "Food group") +
      theme_linedraw() +
      theme(
        axis.title.y = element_text(size = 12, face = "bold", vjust = 1),
        axis.title.x = element_text(size = 12, face = "bold", vjust = -0.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(
          size = 12,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  output$plot_regiongeo <- renderPlot({
    print(reactive_plot_regiongeo())
  })
  
  ##Draw plots for tabs in the third item (category) ----
  
  reactive_plot_category <- reactive({
    
    selected_env_itm <- input$env_dimensions_4
    
    data <- filtered_data_category()
    
    data$region_custom <-
      factor(data$region, levels = custom_order_region, labels = custom_labels_region)
    
    p_category <- ggplot(data, aes(
      x = factor(
        food_group,
        level = c(
          "beef_lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "roots",
          "fruit_veg",
          "oils",
          "sugar",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      ),
      y = value,
      fill = macrofoods
    )) +
      geom_col(color = "white", width = 0.6) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom, ncol = 5,
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
                  ) +
      scale_fill_manual(values = colors_macro, breaks = c("ASF", "Staples", "Other")) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      labs(x = "Food Group", y = paste(selected_env_itm), fill = "Category:") +
      theme_linedraw() +
      theme(
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold", vjust = 0.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(
          size=12,
          angle = 45,
          hjust = 1),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
    
  })
  
  output$plot_category <- renderPlot({
    print(reactive_plot_category())
  })
  
  
  reactive_plot_categorymacro <- reactive({
    
    selected_env_itm <- input$env_dimensions_6
    
    data <- filtered_data_categorymacro()
    
    data$region_custom <-
      factor(data$region, levels = custom_order_region, labels = custom_labels_region)
    
    p_categorymacro <- ggplot(data, aes(
      x = factor(macrofoods, level = c("ASF", "Staples", "Other")),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef_lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      facet_wrap( ~ region_custom , ncol = 5,
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
                  ) +
      scale_fill_manual(values = colors_food) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      labs(x = "Food Category", y = paste(selected_env_itm), fill = "Food group:") +
      theme_linedraw() +
      theme(
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold", vjust = 0.5),
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size=12),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  output$plot_categorymacro <- renderPlot({
    print(reactive_plot_categorymacro())
  })

  
  
  
  #Generate data tables ----
  
  #Generating a table from the user's data selection is not straightforward in cases where the plot is faceting using the
  #commands #face_wrap or #facet_grid. For example, in the sexage tab the code offers the user the chance to facet multiple plots
  #in a single image by selecting to display data for several regions at once. The age group is on the x axis, the impact value is on the
  #y axis, and the sex category drives the coloring of the data points. This adds another layer of data that a static table cannot handle.
  #The code below works around this issue by going through a few steps of data manipulation. The bulk of it was suggested by Chat GPT, but I had to make
  #adjustments to make it work in this context. There may be an easier way to code this, but I have not been able to find it yet.
  
  ##Generate data table for sexage ----
  #Create table for the sexage tab, starting from the subsection of the main dataset identified by the filtered_data_sexage element, that here is called as a function.
  sexage_table <- reactive({
    data <- filtered_data_sexage()
    
    #Add a unique identifier column based on all relevant variables. This is needed because otherwise the datatable command used further down will automatically
    #group into a single row instances where different combinations of variables in the dataset correspond to the same impact value on the y axis.
    #If for example FML aged 10-19 have an impact value of 1.19 in both HIC and LIC, the datatable command would display just one row for the value 1.19, and list
    #both HIC and LIC in the region column. Creating a unique identifier that is formed by the values taken by each variable in each occurrence ensures
    #that we can generate a table in which duplicate values are presented in distinct rows.
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
    
    #Pivot the data to long format including the facet variable 'region'. This basically means expanding the number of
    #rows in the data selection. The table is a reactive element that follows the inputs
    #chosen by the user, but datatable needs to have access to the full resolution of the underlying dataset or it won't work.
    #By using pivot_longer, we are creating a new data frame that includes all occurrences of each variable for all values of
    #age.education, the variable on the x axis, and the region, the variable that drives the plot faceting.
    data_long <- data %>%
      pivot_longer(cols = c(age.education, region),
                   names_to = "variable",
                   values_to = "values")
    
    #Pivot the long data to a wide format, including the facet variable 'region'. This builds on the previous step, expanding on the
    #columns rather than the rows, ensuring that the resulting dataframe includes all the possible occurrences of the variables
    #specified in the pivot_longer call.
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    #Convert the list column to character format, this is to avoid errors when creating the HTML table below.
    #Converting everything in the dataframe to characters ensures that R won't get confused by variables that are expressed in
    #different formats
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    #generate the dataframe that results from the previous permutations. This concludes the reactive call that results in sexage_table
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$sexage_table <- renderUI({
    sexage_data <- sexage_table()
    
    
    #Remove the 'unique_id' column from the data. We created this in the previous section to ensure that all occurrences of the value
    #on the y axis are assigned a distinct row, but we don't need to show that column to the user.
    sexage_data <- sexage_data[, !colnames(sexage_data) %in% c("unique_id", "box")]
    
    #Convert the list column 'age.education' to a character vector, ensuring there are no errors when R has to read it
    sexage_data$age.education <- sapply(sexage_data$age.education, paste, collapse = ", ")
    #This is the command that creates the actual table. The pageLength argument tells R to display a table that is as long
    #as there are rows in the originating dataset - which is defined dynamically by the user's choice of inputs.
    #The scrollX and scrollY arguments simply make the table scrollable across both axes. I set rownames to TRUE so that
    #R automatically assigns a number to each rowm to make it easier to count records.
    table_html <- datatable(sexage_data,
                            options = list(dom = 't', pageLength = nrow(sexage_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_sexage <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("sexage_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      sexage_data_export <- sexage_table()
      
      # Remove the 'unique_id' column
      sexage_data_export <- sexage_data_export[, !colnames(sexage_data_export) %in% c("unique_id", "box")]
      
      # Convert all columns to character
      sexage_data_export[] <- lapply(sexage_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(sexage_data_export, file, row.names = FALSE)
    }
  )
  
  
  
  ##Generate data table for eduurb ----
  #Create table for the eduurb tab, starting from the subsection of the main dataset identified by the filtered_data_eduurb element, that here is called as a function.
  eduurb_table <- reactive({
    data <- filtered_data_eduurb()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(sex.urbanisation, region),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$eduurb_table <- renderUI({
    
    eduurb_data <- eduurb_table()
    eduurb_data <- eduurb_data[, !colnames(eduurb_data) %in% c("unique_id", "box")]
    eduurb_data$sex.urbanisation <- sapply(eduurb_data$sex.urbanisation, paste, collapse = ", ")
    table_html <- datatable(eduurb_data,
                            options = list(dom = 't', pageLength = nrow(eduurb_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_eduurb <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("eduurb_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      eduurb_data_export <- eduurb_table()
      
      # Remove the 'unique_id' column
      eduurb_data_export <- eduurb_data_export[, !colnames(eduurb_data_export) %in% c("unique_id", "box")]
      
      # Convert all columns to character
      eduurb_data_export[] <- lapply(eduurb_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(eduurb_data_export, file, row.names = FALSE)
    }
  )
  
  ##Generate data table for sociodem ----
  #Create table for the sociodem tab, starting from the subsection of the main dataset identified by the filtered_data_sociodem element, that here is called as a function.
  sociodem_table <- reactive({
    data <- filtered_data_sociodem()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, category, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(food_group, region),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$sociodem_table <- renderUI({
    
    sociodem_data <- sociodem_table()
    sociodem_data <- sociodem_data[, !colnames(sociodem_data) %in% "unique_id"]
    sociodem_data$food_group <- sapply(sociodem_data$food_group, paste, collapse = ", ")
    table_html <- datatable(sociodem_data,
                            options = list(dom = 't', pageLength = nrow(sociodem_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_sociodem <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("sociodem_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      sociodem_data_export <- sociodem_table()
      
      # Remove the 'unique_id' column
      sociodem_data_export <- sociodem_data_export[, !colnames(sociodem_data_export) %in% "unique_id"]
      
      # Convert all columns to character
      sociodem_data_export[] <- lapply(sociodem_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(sociodem_data_export, file, row.names = FALSE)
    }
  )
  
  ##Generate data table for region ----
  #Create table for the region tab, starting from the subsection of the main dataset identified by the filtered_data_eduurb element, that here is called as a function.
  region_table <- reactive({
    data <- filtered_data_region()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(env_itm, food_group),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$region_table <- renderUI({
    
    region_data <- region_table()
    region_data <- region_data[, !colnames(region_data) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
    region_data$env_itm <- sapply(region_data$env_itm, paste, collapse = ", ")
    table_html <- datatable(region_data,
                            options = list(dom = 't', pageLength = nrow(region_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_region <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("region_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      region_data_export <- region_table()
      
      # Remove the 'unique_id' column
      region_data_export <- region_data_export[, !colnames(region_data_export) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
      
      # Convert all columns to character
      region_data_export[] <- lapply(region_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(region_data_export, file, row.names = FALSE)
    }
  )
  
  
  ##Generate data table for regiongeo ----
  #Create table for the regiongeo tab, starting from the subsection of the main dataset identified by the filtered_data_regiongeo element, that here is called as a function.
  regiongeo_table <- reactive({
    data <- filtered_data_regiongeo()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(env_itm, food_group),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$regiongeo_table <- renderUI({
    
    regiongeo_data <- regiongeo_table()
    regiongeo_data <- regiongeo_data[, !colnames(regiongeo_data) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
    regiongeo_data$env_itm <- sapply(regiongeo_data$env_itm, paste, collapse = ", ")
    table_html <- datatable(regiongeo_data,
                            options = list(dom = 't', pageLength = nrow(regiongeo_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_regiongeo <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("regiongeo_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      regiongeo_data_export <- regiongeo_table()
      
      # Remove the 'unique_id' column
      regiongeo_data_export <- regiongeo_data_export[, !colnames(regiongeo_data_export) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
      
      # Convert all columns to character
      regiongeo_data_export[] <- lapply(regiongeo_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(regiongeo_data_export, file, row.names = FALSE)
    }
  )
  
  
  ##Generate data table for category ----
  #Create table for the category tab, starting from the subsection of the main dataset identified by the filtered_data_category element, that here is called as a function.
  category_table <- reactive({
    data <- filtered_data_category()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, macrofoods, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(region, macrofoods),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$category_table <- renderUI({
    
    category_data <- category_table()
    category_data <- category_data[, !colnames(category_data) %in% "unique_id"]
    category_data$region <- sapply(category_data$region, paste, collapse = ", ")
    table_html <- datatable(category_data,
                            options = list(dom = 't', pageLength = nrow(category_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_category <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("category_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      category_data_export <- category_table()
      
      # Remove the 'unique_id' column
      category_data_export <- category_data_export[, !colnames(category_data_export) %in% "unique_id"]
      
      # Convert all columns to character
      category_data_export[] <- lapply(category_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(category_data_export, file, row.names = FALSE)
    }
  )
  
  ##Generate data table for category macro ----
  #Create table for the category macro tab, starting from the subsection of the main dataset identified by the filtered_data_categorymacro element, that here is called as a function.
  categorymacro_table <- reactive({
    data <- filtered_data_categorymacro()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, macrofoods, sep = "_"))
    
    data_long <- data %>%
      pivot_longer(cols = c(region, food_group),
                   names_to = "variable",
                   values_to = "values")
    
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
    
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
    
    return(data_wide)
  })
  
  #Now create the actual table based on the dataframe that results from the previous section
  output$categorymacro_table <- renderUI({
    
    categorymacro_data <- categorymacro_table()
    categorymacro_data <- categorymacro_data[, !colnames(categorymacro_data) %in% "unique_id"]
    categorymacro_data$region <- sapply(categorymacro_data$region, paste, collapse = ", ")
    table_html <- datatable(categorymacro_data,
                            options = list(dom = 't', pageLength = nrow(categorymacro_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers
    
    return(table_html)
  })
  
  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_categorymacro <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("categorymacro_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      categorymacro_data_export <- categorymacro_table()
      
      # Remove the 'unique_id' column
      categorymacro_data_export <- categorymacro_data_export[, !colnames(categorymacro_data_export) %in% "unique_id"]
      
      # Convert all columns to character
      categorymacro_data_export[] <- lapply(categorymacro_data_export, as.character)
      
      # Write the data to a CSV file
      write.csv(categorymacro_data_export, file, row.names = FALSE)
    }
  )
  
  
  ##Generate data table for cons proxy compare ----
  
  
  
  #Generate code to download the plots
  
  output$download_plot_sexage <- downloadHandler(
    filename = function() { paste("plot_sexage", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_sexage(),
                                        device = "png", width = 10) }  
  )
  
  output$download_plot_eduurb <- downloadHandler(
    filename = function() { paste("plot_eduurb", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_eduurb(),
                                      device = "png", width = 10) }  
  )
  
  output$download_plot_sociodem <- downloadHandler(
    filename = function() { paste("plot_sociodem", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_sociodem(),
                                      device = "png", width = 10) } 
  )
  
  output$download_plot_region <- downloadHandler(
    filename = function() { paste("plot_region", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_region(),
                                      device = "png", width = 10) } 
  )
  
  output$download_plot_regiongeo <- downloadHandler(
    filename = function() { paste("plot_regiongeo", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_regiongeo(),
                                      device = "png", width = 10) } 
  )
  
  output$download_plot_category <- downloadHandler(
    filename = function() { paste("plot_category", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_category(),
                                      device = "png", width = 10) } 
  )
  
  output$download_plot_categorymacro <- downloadHandler(
    filename = function() { paste("plot_categorymacro", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_categorymacro(),
                                      device = "png", width = 10) } 
  )
  
  
}

#Run the app ----

shinyApp(ui, server)