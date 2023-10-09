#This file includes all the instructions to launch the dashboard. It only requires loading in the correct data files.

library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(RColorBrewer)
library(ggthemes)
library(patchwork)
library(thematic)
library(bbplot)
library(plotly)
library(here)
library(DT)
library(tidyr)
#clear the environment
rm(list = ls())

#######-------------------#########-------------------------###########
                #Load data files
#######-------------------#########-------------------------###########

    #################
    #Load the _trs report, whith all sociodem charactestics listed in the same variable/vector, to create plots that display do not require comparing across more than one sociodem
    #characteristic at a time
    #################
  
    #Report_TRS Load the csv file from work computer, and make it into a dataframe
    #csv_file_trs <- "/Users/lshsc40/Documents/R files/report_env_trs_053123.csv"
    csv_file_trs <- "/Users/lshsc40/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM/Visualisation/Dashboard/Seb Dashboard/data/report_env_trs_053123.csv"
    #load from personal laptop
    #csv_file_trs <- "/Users/sebastianoc/Documents/R scripts/report_env_trs_053123.csv"
    

    data_trs <- read.csv(csv_file_trs)
      data_trs$value <- round(data_trs$value, 2)
        df_trs <- data_trs 
        

        # Create a new dataset, data_trs_category, by adding a column labelled 'category' to the _trs dataset, to group different labels in the variable age/sociodem to subgroups (if useful)
        data_trs_category <- data_trs %>%
        mutate(category = case_when(
          age %in% c("FML", "MLE", "BTH") ~ "Sex",
          age %in% c("low", "medium", "high", "all-e") ~ "Education level",
          age %in% c("rural", "urban", "all-u") ~ "Urbanisation level",
          age %in% c("0-10", "11-19", "20-39", "40-64", "65+", "all-a") ~ "Age group"
        ))
      
        #Create another dataset, data_trs_macrofoods by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
        # namely ASF, Staples, Other, Total. This dataset includes a column for the category, and a column for the macrofoods.
        data_trs_macrofoods <- data_trs_category %>%
          mutate(macrofoods = case_when(
            food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
            food_group %in% c("rice", "grains") ~ "Staples",
            food_group %in% c("fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds") ~ "Other",
            food_group %in% c("total") ~ "Total"
          ))
  
        #Create a third dataset, df_trs_macrof, by adding a column labelled 'macrofoods' to the main df dataframe. This dataset only has one additional column, to group macrofoods, if the user is not
        #interested in grouping sociodem categories
        df_trs_macrof <- df_trs %>%
          mutate(macrofoods = case_when(
            food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
            food_group %in% c("rice", "grains", "roots") ~ "Staples",
            food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds") ~ "Other",
            food_group %in% c("total") ~ "Total"
          ))
        
        df_trs_category <- data_trs_category
        
        
    #################
    #Load the _box report file, which has the 'box' variable, to create plots that display age-sex and edu-urb combinations.
    #################
        
    #Report_BOX Load the csv file on work laptop
    #csv_file_box <- "/Users/lshsc40/Documents/R files/report_env_box_060123.csv"
    csv_file_box <- "/Users/lshsc40/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM/Visualisation/Dashboard/Seb Dashboard/data/report_env_box_060123.csv"
    #Report_BOX Load the csv file on personal laptop
    #csv_file_box <- "/Users/sebastianoc/Documents/R scripts/report_env_box_060123.csv"
    #generate a dataset from the _box file as source for the dashboard
    data_box <- read.csv(csv_file_box)
    data_box$value <- round(data_box$value, 2)
    df <- data_box 
    
    ########################################
    #Load the cons_compare file, which contains daily intakes for several food groups across countries and for different intake proxies (GDD, FBS, etc)
    ########################################
    
    #Load from work laptop
    #csv_file_cons <- "/Users/lshsc40/Documents/R files/cons_compare_012823.csv"
    csv_file_cons <- "/Users/lshsc40/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM/Visualisation/Dashboard/Seb Dashboard/data/cons_compare_012823.csv"
    #Load from personal laptop
    #csv_file_cons <- "/Users/sebastianoc/Documents/R scripts/cons_compare_012823.csv"
    
    data_cons <- read.csv(csv_file_cons)
    data_cons$Intake <- round(data_cons$Intake, 2)
    df_cons <- data_cons

    ########################################
    #Load the FBS_intake file, which contains FBS daily intakes for across different demographics
    ########################################
    
    #Load from work laptop
    csv_file_FBSintake <- "/Users/lshsc40/Library/CloudStorage/OneDrive-LondonSchoolofHygieneandTropicalMedicine/LSHTM/Visualisation/Dashboard/Seb Dashboard/data/FBS_intake_socio_all-a_051523.csv"
    #Load from personal laptop
    #csv_file_FBSintake <- "/Users/sebastianoc/Documents/R scripts/FBS_intake_socio_all-a_051523.csv"
    
    data_FBSintake <- read.csv(csv_file_FBSintake)
    data_FBSintake$Value <- round(data_FBSintake$Value, 2)
    df_FBSintake <- data_FBSintake
    
#######-------------------#########-------------------------###########
#Start the dashboard. A dashboard in Shiny is composed of a UI, a Server Function, and a command to run the app.
#######-------------------#########-------------------------###########

#Notes on Shiny and dashboards. Shiny allows the user to create an app, or dashboard, to quickly visualise datasets. When using the app, and changing the data selections,
#the app cycles through both the UI and the Server function in order to display the data. The UI tells R how the dashboard should look like and what
#options the user should have. The Server function starts from the original data sources to build and visualise the data selection that results
#from the user's choices. Both UI and Server function are called on when running the app, so errors must be looked for in both. 


    ########################################
    #Start the UI (User Interface). This section gives instructions on the visual display and structure of the dashboard. It generates a Shiny object.
    ########################################
    
    ui <- dashboardPage(
            dashboardHeader(
              title = "Explore the environmental footprint of global diets",
              titleWidth = 450
              ),
        # titlePanel("The environmental footprints of global diets"),
            dashboardSidebar(
              sidebarMenu(
                menuItem("Data on env. footprints", tabName = "diet_footprints", icon = icon("seedling"),
                  menuSubItem("Compare by sociodemographic", tabName = "sociodem", icon = icon("person-half-dress")),
                  menuSubItem("Compare by geo/income region", tabName = "food_groups", icon = icon("earth-africa")),
                  menuSubItem("Compare by food group/category", tabName = "categories", icon = icon("wheat-awn"))
                  ),
                menuItem("Data on consumption", tabName = "intake_sociodem", icon = icon("wheat-awn"),
                  menuSubItem("Consumption by proxy", tabName = "consumption", icon = icon("bowl-food")),
                  menuSubItem("Consumption by sociodem", tabName = "consumption_sociodem", icon = icon("bowl-rice"))
                  ),
                menuItem("ReadMe", tabName = "readme", icon = icon("info-circle"), selected = TRUE)
        ), collapsed = FALSE
      ),
      # mainPanel(
            dashboardBody(
              tabItems(
          ###First item
                tabItem(
                  tabName = "sociodem",
                  tabsetPanel(
                    tabPanel("About", box(
                      title = "About this data", HTML(
                      "These tabs allow you to compare diet-related environmental footprints across different sociodemographics.<br><br>
                      Use the first tab, 'Sex-age', if you want to explore how diet-related environmental footprints differ among sexes and age groups. Open the second tab instead, 'Edu-urb', if you want
                      to explore how the footprints change across education and urbanisation levels. Both tabs display the impacts as a ratio to those of an average global diet, which is set equal to 1 in the plots.
                      This means that the impacts in the first two tabs are not expressed in absolute values, but in relative terms, and represent an average across all environmental dimensions included in our data.<br><br>
                      If you are interested in seeing instead how absolute and per capita impacts differ across sociodemographics, regions, and environmental dimensions, open the third tab, 'Sociodem'.")
                    )),
                    tabPanel(
                      "Sex-age",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("cns_prsp_2", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_2", "Select Measure:", choices = c("cap-avg_WLD","cap-avg_RGS"), selected = "cap-avg_WLD"),
                          selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "avg"),
                          selectInput("region_2", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = "WLD"),
                          selectInput("age.education_2", "Select age group:", choices = c("0-10", "11-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-10", "11-19", "20-39", "40-64", "65+")),
                          selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML")),
                          downloadButton("download_csv_sexage", "Download table"),
                          #downloadButton("download_plot_sexage", "Download plot")
                          #downloadButton("sexage_download_plot", "Download plot", class = "butt1")
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                      "Edu-urb",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary", 
                          selectInput("cns_prsp_3", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_3", "Select Measure:", choices = c("cap-avg_WLD","cap-avg_RGS"), selected = "cap-avg_WLD"),
                          selectInput("env_dimensions_3", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "avg"),
                          selectInput("region_3", "Select Region:", choices = unique(df$region), multiple = TRUE, selected = "WLD"),
                          selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high"), multiple = TRUE, selected = c("low", "medium", "high")),
                          selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural"), multiple = TRUE, selected = c("urban", "rural")),
                          downloadButton("download_csv_eduurb", "Download table"),
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                      "Sociodem",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",  
                          selectInput("cns_prsp_7", "Select Consumption Perspective:", choices = unique(df_trs_category$cns_prsp), selected = "avb"),
                          selectInput("measure_7", "Select Measure:", choices = c("abs","cap"), selected = "cap"),
                          selectInput("env_dimensions_7", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), "avg"), selected = "GHG"),
                          selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = "WLD"),
                          selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = "total"),
                          selectInput("age_7", "Select sociodemographic:", choices = setdiff(unique(df_trs_category$age), c("all-a", "all-e", "BTH", "all-u")), multiple = TRUE, selected = c("low", "medium", "high", "urban", "rural"))
                          ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_sociodem"
                                     #, height = 400
                                     )
                        )
                    )
                  )
                )
                ),
                tabItem(
            ###Second item
                  tabName = "food_groups",
                  tabsetPanel(
                    tabPanel("About", box(
                      title = "About this data",HTML(
                      "These tabs allow you to compare absolute and per capita diet-related environmental footprints of diets across income and geographical regions.<br><br>
                      In both tabs, you can see how much each food group contributes to the environmental footprint across six different dimensions: GHG emissions, Eutrophication, Land Use, Land Use - Pasture, Land Use - Crops,
                      Freshwater Withdrawals.<br><br> If you want to 
                      compare across income regions (Low Income Countries, Low-Middle Income Countries, Upper-Middle Income Countries, High Income Countries), open the first tab. Explore the second tab instead
                      if your focus is on geographic groupings."
                      )
                      )),
                    tabPanel(
                      "Impacts by income region",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",  
                          selectInput("cns_prsp_1", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_1", "Select Measure:", choices = c("abs", "cap"), selected = "cap"),
                          selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"),multiple = TRUE, selected = "GHG"),
                          selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = "total"),
                          selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("LIC", "LMC", "UMC", "HIC"))
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_region"
                                     #, height = 400
                          )
                        )
                      )
                    ),
                    tabPanel(
                    "Impacts by geographical region",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("cns_prsp_5", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_5", "Select Measure:", choices = c("abs", "cap"), selected = "cap"),
                          selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"),multiple = TRUE, selected = "GHG"),
                          selectInput("food_group_5", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c("beef", "milk", "rice", "roots")),
                          selectInput("region_5", "Select Region:", choices = c("NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF", "WLD"), multiple = TRUE, selected = c("NAC", "SAS", "SSF"))
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_regiongeo"
                                     #, height = 400
                          )
                        )  
                      )
                    )
                  )
                ),
                tabItem(
            ###Third item
                  tabName = "categories",
                  tabsetPanel(
                    tabPanel("About", box(
                      title = "About this data", HTML(
                      "These tabs allow you to compare absolute and per capita environmental footprints associated with consumption of fifteen food groups and three macrocategories, across regions.<br><br>
                      We grouped the fifteen food groups into three macrocategories: Animal Source Foods (ASF), Staples, and Other. This is an arbitrary classification made for the
                      purpose of this dashboard, with the goal of making it easier for the user to make comparisons at a glance. By combining information on both the food group
                      and the macrocategory, the user can explore how different categories of food impact each environmental dimension, while also seeing how individual food groups contribute within each category.<br><br>
                      Open the first tab if you want to focus on the fifteen separate food groups, and compare their impacts across regions. Select the second tab if instead you want to focus on the three macrocategories."
                      )
                      )),
                    tabPanel(
                    "Impacts by food group",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("cns_prsp_4", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_4", "Select Measure:", choices = c("abs", "cap"), selected = "abs"),
                          selectInput("env_dimensions_4", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"), selected = "GHG"),
                          selectInput("food_group_4", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef", "lamb", "rice", "grains", "fruit_veg", "legumes")),
                          selectInput("region_4", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = "WLD")
                           ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_category"
                                     #, height = 400
                          )
                        )
                      )
                    ),
                    tabPanel(
                    "Impacts by food category",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("cns_prsp_6", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                          selectInput("measure_6", "Select Measure:", choices = c("abs", "cap"), selected = "abs"),
                          selectInput("env_dimensions_6", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("avg", "land_pstr", "land_crop")), selected = "GHG"),
                          selectInput("food_group_6", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef", "pork", "milk", "legumes", "roots", "rice", "grains")),
                          #selectInput("category_6", "Select Food Category:", choices = setdiff(unique(df_trs_macrof$macrofoods), "Total"), multiple = TRUE),
                          selectInput("region_6", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("LIC", "HIC"))
                            ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_categorymacro"
                                     #, height = 400
                          )
                        )
                      )
                    )
                  )
                ),
                tabItem(
            ###Fourth item
                  tabName = "consumption",
                  tabsetPanel(
                    tabPanel("About", box(
                      title = "About this data", HTML(
                      "It is impossible to know exactly what each person on the planet eats on a given day. But we can generate fairly accurate estimates using different data sources and techniques.
                      In this section we present data from four different proxies for daily intakes of ten different food groups, to show how estimation can vary - or not vary - across sources.<br><br> The first proxy, or estimate, is based 
                      on the FAO's country Balance Sheets (FBS). FBS record how much of a given food is produced, imported, exported, wasted, and ultimately made available for human consumption in each country.<br><br>
                      The second proxy, or estimate, is based on the Global Dietary Dataset, a large-scale effort to combine data from thousands of surveys on the diets
                      of people all over the globe.<br><br> The third and fourth proxy are simply the FBS and the GDD-derived estimates adjusted by energy intake. Use this section if you want to focus on 
                      how representations of global diets can change - or not change - depending on what consumption data you use. 
                      "
                      )
                      )),
                    tabPanel(
                      "Compare consumption proxies",
                        fluidRow(
                          box(
                            width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                            selectInput("measure_8", "Select Measure:", choices = unique(df_cons$Measure), selected = "abs"),
                            selectInput("indicator_8", "Select Proxy:", choices = c("GDD", "FBS", "GDD_adj_IOM", "FBS_adj_IOM"), multiple = TRUE, selected = "GDD"),
                            selectInput("region_8", "Select Region:", choices = unique(df_cons$Region),multiple = TRUE, selected = "WLD"),
                            selectInput("food_group_8", "Select Food Group:", choices = unique(df_cons$Food.group), multiple = TRUE, selected = c("dairy", "fruits", "nuts")),
                            selectInput("stats_8", "Select Statistic of interest:", choices = unique(df_cons$Stats), selected = "mean")
                          ),
                          box(
                            width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                            plotOutput("plot_consumption"
                                       #, height = 400
                             )
                           )
                        )
                      )
                    )
                  ),
                tabItem(
                  ####Fifth Item
                  tabName = "consumption_sociodem",
                  tabsetPanel(
                    tabPanel("About", box(
                      title = "About this data", HTML(
                      "These tabs allow you to compare average daily intakes of 27 different food groups across regions, countries, and sociodemographic characteristics. Intakes are based on
                      availability data from the FAO's FBS, or Food Balance Sheets. FBS record how much of a given food is produced, imported, exported, wasted, and ultimately made available for human consumption in each country.<br><br>
                      Use the first tab if you want to focus on how total daily intakes differ by sociodemographic in each country or region. Open the second tab if you instead want to compare how
                      daily intakes of individual food groups in populations with defined sociodemographic attributes differ across regions. Finally, use the third and last tab if you wish to compare
                      intakes across sociodemographics, as in the first tab, but prefer to see separate intakes for each food group rather than have them all added up to a cumulative total.<br><br>
                      Intake data in this section is available for up to 27 food groups, which means that plots can become harder to read if you select all of them at once. While we've made efforts to keep the 
                      plots legible even at full resolution, we recommend carefully choosing which food groups you want to focus on."
                      )
                      )),
                    tabPanel(
                      "Cumulative intake by sociodemographic",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("unit_9", "Select Unit:", choices = unique(df_FBSintake$Unit), selected = "g/d_w"),
                          selectInput("food_group_9", "Select Food Group:", unique(df_FBSintake$Food.group), multiple = TRUE, selected = c("rice", "wheat", "roots")),
                          selectInput("region_9", "Select Region:", choices = unique(df_FBSintake$Region), selected = "WLD"),
                          #selectInput("age_9", "Select Age Group:", choices = unique(df_FBSintake$Age), multiple = TRUE, selected = "all-a"),
                          selectInput("sex_9", "Select Sex:", choices = unique(df_FBSintake$Sex),multiple = TRUE, selected = c("FML", "MLE")),
                          selectInput("urbanisation_9", "Select Urbanisation Level:", choices = unique(df_FBSintake$Urbanisation),multiple = TRUE, selected = c("rural", "urban")),
                          selectInput("education_9", "Select Education Level:", choices = unique(df_FBSintake$Education),multiple = TRUE, selected = c("low", "medium", "high"))
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_FBSintake"
                                     #, height = 400
                          )
                        )
                      )
                    ),
                    tabPanel(
                      "Intake by food group, across Regions",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("unit_10", "Select Unit:", choices = unique(df_FBSintake$Unit), selected = "g/d_w"),
                          selectInput("food_group_10", "Select Food Group:", unique(df_FBSintake$Food.group), multiple = TRUE, selected = c("rice", "wheat", "roots")),
                          selectInput("region_10", "Select Region:", choices = unique(df_FBSintake$Region),multiple = TRUE, selected = "WLD"),
                          #selectInput("age_9", "Select Age Group:", choices = unique(df_FBSintake$Age), multiple = TRUE, selected = "all-a"),
                          selectInput("sex_10", "Select Sex:", choices = unique(df_FBSintake$Sex), selected = "BTH"),
                          selectInput("urbanisation_10", "Select Urbanisation Level:", choices = unique(df_FBSintake$Urbanisation), selected = "rural"),
                          selectInput("education_10", "Select Education Level:", choices = unique(df_FBSintake$Education), selected = "low")
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_FBSintake_fg"
                                     #, height = 400
                          )
                        )
                      )
                    ),
                    tabPanel(
                      "Intake by food group, across sociodemographics",
                      fluidRow(
                        box(
                          width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                          selectInput("unit_11", "Select Unit:", choices = unique(df_FBSintake$Unit), selected = "g/d_w"),
                          selectInput("food_group_11", "Select Food Group:", unique(df_FBSintake$Food.group), multiple = TRUE, selected = c("rice", "wheat", "roots")),
                          selectInput("region_11", "Select Region:", choices = unique(df_FBSintake$Region), selected = "WLD"),
                          #selectInput("age_9", "Select Age Group:", choices = unique(df_FBSintake$Age), multiple = TRUE, selected = "all-a"),
                          selectInput("sex_11", "Select Sex:", choices = unique(df_FBSintake$Sex),multiple = TRUE, selected = c("FML", "MLE")),
                          selectInput("urbanisation_11", "Select Urbanisation Level:", choices = unique(df_FBSintake$Urbanisation),multiple = TRUE, selected = c("rural", "urban")),
                          selectInput("education_11", "Select Education Level:", choices = unique(df_FBSintake$Education),multiple = TRUE, selected = c("low", "medium", "high"))
                        ),
                        box(
                          width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                          plotOutput("plot_FBSintake_fg_socio"
                                     #, height = 400
                          )
                        )
                      )
                    )
                  )
                ),
                tabItem(
            ###Fourth item
                  tabName = "readme",
                  fluidRow(
                    box(
                      title = "ReadMe",
                      div(
                        HTML(
                        "This dashboard offers users the chance to explore data on the composition of global diets, and on their related environmental footprints. 
                        It uses new datasets and estimates, developed by Prof. Marco Springmann at the London School of Hygiene and Tropical Medicine, and is divided in two main sections. One focuses on consumption
                        data, expressed in daily intakes, and the other on diet-related environmental footprints.<br><br> 
                        The data can be filtered by region, country, and by characteristics such as Sex, Age Group, Education Level, etc.
                        To better manage the level of detail available in these new estimates, we have divided the dashboard in 
                        distinct sections that focus on a specific number of dimensions at a time. Within each section,
                        the user is free to choose among several combinations of filters and generate plots of their interest.
                        All plots are downloadable and reproducible, as long as their source is clearly and correctly cited.<br><br>
                        We hope that this tool can be useful to all who wish to know more about how diets differ among peoples, and what impact they have on the environment. For feedback or
                        questions please contact Sebastiano Caleffi at sebastiano.caleffi@lshtm.ac.uk",
                            )
                        #style = "width:100%;"
                            
                        )
                      )
                    )
                  #   box(
                  #     title = "ReadMe",
                  #     "This dashboard offers users the chance to explore data on the environmental footprints generated by global diets. All data refers to the year 2018 (CHECK!)
                  #     and is built on the most comprehensive estimates of daily intakes and average footprints by food group currently available. Etc Etc Etc"
                  #   )
                  # )
                )
              )
            )
          )
    
    
    ########################################
    #Start the Server function. This section gives instructions on how the data should be filtered and visualised. It generates a function.
    ########################################


    server <- function(input, output, session) {
  
    ###########
    #Create filters for tabs in the first menu item, 'compare by sociodemographic'. 
    #NOTE: The filters need to be coherent with the options for data selection given to users in the UI using the command 'selectInput'. If the user is
    #given the freedom to choose specific inputs in the UI, but then the corresponding filters are not set appropriately in the Server, 
    #the app may work but it won't display any plots, or the plots may be empty. So if plots are not showing or not showing correctly, first ensure that
    #the inputs available to the user in the UI correctly match the corresponding filters in the Server function.
    ###########
      
    #Note on filter notation: 
      #The notation == means that for the variable x, the app will take as input the specific value selected for that variable by the user through the UI interface. It implies that the user can select only one value at a time for that variable.
      #The notation %in% means that for the variable x, the app will take as input any values selected for that variable by the user through the UI interface. It implies that the user can select multiple values within that variable.
      
    filtered_data_sexage <- reactive({
      df %>%
        filter(measure == input$measure_2,
               env_itm %in% input$env_dimensions_2,
               food_group == "total",
               box == "age-sex",
               age.education %in% input$age.education_2,
               sex.urbanisation %in% input$sex.urbanisation_2,
               cns_prsp == input$cns_prsp_2,
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
               cns_prsp == input$cns_prsp_3,
               region %in% input$region_3)
    })
    
    filtered_data_sociodem <- reactive({
      df_trs_category %>%
        filter(measure == input$measure_7,
               env_itm %in% input$env_dimensions_7,
               food_group %in% input$food_group_7,
               age %in% input$age_7,
               cns_prsp == input$cns_prsp_7,
               region %in% input$region_7)
    })
    
    #############
    #Create filters for tabs in the second menu item, 'compare by food groups'
    #############
    filtered_data_region <- reactive({
      df %>%
        filter(measure == input$measure_1,
               env_itm %in% input$env_dimensions_1,
               food_group %in% input$food_group_1,
               box == "age-sex",
               age.education == "all-a",
               sex.urbanisation == "BTH",
               cns_prsp == input$cns_prsp_1,
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
               cns_prsp == input$cns_prsp_5,
               region %in% input$region_5)
    })
    
    #############
    #Create filters for tabs in the third menu item, 'compare by categories'
    #############
    filtered_data_category <- reactive({
      df_trs_macrof %>%
        filter(measure == input$measure_4,
               env_itm %in% input$env_dimensions_4,
               food_group %in% input$food_group_4,
               age == "all-a",
               cns_prsp == input$cns_prsp_4,
               region %in% input$region_4,
               macrofoods %in% c("ASF", "Staples", "Other"))
    })
    
    filtered_data_categorymacro <- reactive({
      df_trs_macrof %>%
        filter(measure == input$measure_6,
               env_itm %in% input$env_dimensions_6,
               food_group %in% input$food_group_6,
               age == "all-a",
               cns_prsp == input$cns_prsp_6,
               region %in% input$region_6,
               macrofoods %in% c("ASF", "Staples", "Other"))
    })
    
    
    #############
    #Create filters for tabs in the fourth menu item, 'consumption data'
    #############
    filtered_data_consumption <- reactive({
      df_cons %>%
        filter(Measure == input$measure_8,
               Indicator %in% input$indicator_8,
               Region %in% input$region_8,
               Food.group %in% input$food_group_8,
               Stats == input$stats_8,
               Year == "2015"
              )
    })
    
    filtered_data_FBSintake <- reactive({
      df_FBSintake %>%
        filter(Unit == input$unit_9,
               Food.group %in% input$food_group_9,
               Region %in% input$region_9,
               Age == "all-a",
               Sex %in% input$sex_9,
               Urbanisation %in% input$urbanisation_9,
               Education %in% input$education_9,
               Year == "2020"
        )
    })
    
    filtered_data_FBSintake_fg <- reactive({
      df_FBSintake %>%
        filter(Unit == input$unit_10,
               Food.group %in% input$food_group_10,
               Region %in% input$region_10,
               Age == "all-a",
               Sex %in% input$sex_10,
               Urbanisation %in% input$urbanisation_10,
               Education %in% input$education_10,
               Year == "2020"
        )
    })
    
    filtered_data_FBSintake_fg_socio <- reactive({
      df_FBSintake %>%
        filter(Unit == input$unit_11,
               Food.group %in% input$food_group_11,
               Region %in% input$region_11,
               Age == "all-a",
               Sex %in% input$sex_11,
               Urbanisation %in% input$urbanisation_11,
               Education %in% input$education_11,
               Year == "2020"
        )
    })
  
    ##################
    #Draw the plots
    ##################
      
      ###############
      #Prepare graphic objects and labels that will be used to create the plots below.
      ###############
    
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
      colors_macro <- c("#922b21", "#85929e", "#f1c40f")
    
      #create a vector named colors_food made up of fifteen colors, to assign specific ones to each food in the food_group variable. This helps with consistency across plots.
      colors_food <- c(
        "total" = "#a6a79b",
        "rice" = "#f9e79f",
        "roots" = "#eb984e",
        "sugar" = "#fad7a0",
        "legumes" = "#6e2c00",
        "beef" = "#cb4335",
        "lamb" = "#d98880",
        "pork" = "#f5a5b5",
        "poultry" = "#fae5d3",
        "eggs" = "#fdedec",
        "milk" = "#f0ebe2",
        "fish" = "#8fbad3",
        "grains" = "#ecdb54",
        "fruit_veg" = "#229954",
        "nuts_seeds" = "#7d6608",
        "oils" = "#abebc6")
      
      # Create a vector to rename facet plots with the full names of the environmental dimensions
      env_itm.labs <- c("GHG (Mt CO2eq)", "Freshwater use (Cubic meters)", "Eutrophication (Mt PO4eq)", "Land use (thousands of sqKm)", "Land use_pasture (thousands of sqKm)", "Land use_crops (thousands of sqKm)")
      names(env_itm.labs) <- c("GHG", "water", "eutr", "land", "land_pstr", "land_crop")
      
      # Create a vector to rename facet plots with the full names of the regions
      region.labs <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "East Asia and Pacific", "Europe & C. Asia", "Latin America & Caribbean", "Middle East and North Africa", "North America", "South Asia", "Sub-Saharan Africa", "World")
      names(region.labs) <- c("LIC", "LMC", "UMC", "HIC", "EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF", "WLD")
      
      ###########
      #Draw plots for tabs in the first item
      ###########
      output$plot_sexage <- renderPlot({
        data <- filtered_data_sexage()
        
        custom_order_region <- c("LIC", "LMC", "UMC", "HIC", "ECS", "MEA", "EAS", "SAS", "NAC", "LCN", "SSF", "WLD")
        custom_labels_region <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "Europe & C. Asia", "Middle East & N. Africa", "E. Asia & Pacific", "South Asia", "North America", "Latin Am. & Caribbean", "Sub-Saharan Africa", "World")
        
        data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
        
        ggplot(data, aes(x = age.education, y = value, color = sex.urbanisation, shape = sex.urbanisation)) +
          geom_point(size=3) +
          scale_color_brewer(palette = "Set1", name = "Sex:", labels = c("Female", "Male")) +
          scale_shape(name = "Sex:", labels = c("Female", "Male")) +
          geom_text_repel(aes(label = value), show.legend = FALSE) +
          #facet_wrap(~ factor(region, levels=c("LIC","LMC","UMC","HIC","ECS","MEA","EAS","SAS","NAC","LCN","SSF","WLD")),ncol = 4, labeller = labeller(region = region.labs)) +
          facet_wrap(~ region_custom,ncol = 4) +
          geom_hline(yintercept = 1, alpha = 0.3) +
          #guides(shape = "none") +
          theme_linedraw() +
          labs(x = "Age group", y = "Diet-related env. impact expressed relative\nto global average  (1 = world average)") + 
          theme(axis.title.x = element_text(vjust = -1, face = "bold"), axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold", vjust = 1.5), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(plot.title=element_text(hjust = 0.5, size = 20), axis.title.x = element_text(face = "bold"), strip.text = element_text(size=12), legend.position = "top", legend.text = element_text(size = 12), axis.text.x = element_text(face = "bold"))
      })
      
      output$plot_eduurb <- renderPlot({
        data <- filtered_data_eduurb()
        
        custom_order_region <- c("LIC", "LMC", "UMC", "HIC", "ECS", "MEA", "EAS", "SAS", "NAC", "LCN", "SSF", "WLD")
        custom_labels_region <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "Europe & C. Asia", "Middle East & N. Africa", "E. Asia & Pacific", "South Asia", "North America", "Latin Am. & Caribbean", "Sub-Saharan Africa", "World")
        
        data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
        
        ggplot(data, aes(x = factor(age.education, level=c("low", "medium", "high")), y = value, color = sex.urbanisation, shape = sex.urbanisation)) +
          geom_point(size=3) +
          scale_color_brewer(palette = "Dark2", name="Urbanisation:", labels=c("Rural", "Urban")) +
          scale_shape(name = "Urbanisation:", labels = c("Rural", "Urban")) +
          geom_text_repel(aes(label = value), show.legend = FALSE) +
          facet_wrap(~ region_custom,ncol = 4) +
          geom_hline(yintercept = 1, alpha = 0.3) +
          #guides(shape = "none") +
          labs(x = "Education level", y = "Diet-related env. impact expressed relative\nto global average  (1 = world average)") + 
          theme_linedraw() + 
          theme(axis.title.x = element_text(vjust = -1, face = "bold"), axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold", vjust = 1.5), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(axis.title.x = element_text(vjust = -1),axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(plot.title=element_text(hjust = 0.5, size = 20), axis.title.x = element_text(face = "bold"), strip.text = element_text(size=12), legend.position = "top", legend.text = element_text(size = 15), axis.text.x = element_text(face = "bold"))
      })
      
      output$plot_sociodem <- renderPlot({
        data <- filtered_data_sociodem()
        
        # custom_order_region <- c("LIC", "LMC", "UMC", "HIC", "ECS", "MEA", "EAS", "SAS", "NAC", "LCN", "SSF", "WLD")
        # custom_labels_region <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "Europe & C. Asia", "Middle East & N. Africa", "E. Asia & Pacific", "South Asia", "North America", "Latin Am. & Caribbean", "Sub-Saharan Africa", "World")
        # 
        # data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
         
        ggplot(data, aes(x = factor(age, level=c("MLE", "FML", "0-10", "11-19", "20-39", "40-64", "65+", "low", "medium", "high", "urban", "rural")), y = value, label = value)) +
                         #fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
          geom_col(aes(fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total"))), color = "white") +
          #facet_wrap(~ factor(region, levels=c("LIC","LMC","UMC","HIC","ECS","MEA","EAS","SAS","NAC","LCN","SSF","WLD")),ncol = 4) +
          #coord_flip() +
          facet_grid(factor(region, levels=c("LIC","LMC","UMC","HIC","ECS","MEA","EAS","SAS","NAC","LCN","SSF","WLD")) ~ category , scales = "free_x", space = "free_x", switch = "x") +
          theme_linedraw() +
          #geom_text_repel(aes(label = value), show.legend = FALSE) +
          scale_x_discrete(guide = guide_axis(n.dodge=2)) +
          scale_fill_manual(values = colors_food) +
          labs(x = NULL, y = "Impact ", fill = "Food group") +
          theme(axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          
          
      })
      
      ###########
      #Draw plots for tabs in the second item
      ###########
      
      
      output$plot_region <- renderPlot({
        data <- filtered_data_region()
        ggplot(data, aes(x = factor(region, level=c("LIC", "LMC", "UMC", "HIC", "WLD")), y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total")))) +
          geom_col(color = "white", width = 0.6) +
          scale_fill_manual(values = colors_food) +
          scale_x_discrete(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "World")) +
          #geom_text_repel(aes(label = value), show.legend = FALSE) +
          facet_wrap(~ env_itm, scales = "free_y", ncol = 2, labeller = labeller(env_itm = env_itm.labs)) +
          labs(x = NULL ,y = "Impact",
               fill = "Food group") +
          theme_linedraw() +
          theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output$plot_regiongeo <- renderPlot({
        data <- filtered_data_regiongeo()
        ggplot(data, aes(x = region, y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total")))) +
          geom_col(color = "white", width = 0.6) +
          scale_fill_manual(values = colors_food) +
          scale_x_discrete(breaks = c("EAS", "ECS", "LCN", "MEA", "NAC", "SAS", "SSF", "WLD"),labels = c("E. Asia & Pacific", "Europe & C. Asia", "Latin Am. & Caribbean", "Middle East & N. Africa", "North America", "South Asia", "Sub-Saharan Africa", "World")) +
          facet_wrap(~ env_itm, scales = "free_y", ncol = 2, labeller = labeller(env_itm = env_itm.labs)) +
          #geom_text_repel(aes(label = value), show.legend = FALSE) +
          labs(x = NULL ,y = "Impact",
               fill = "Food group") +
          theme_linedraw() +
          theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      
      ###########
      #Draw plots for tabs in the third item
      ###########
      output$plot_category <- renderPlot({
        data <- filtered_data_category()
        
        custom_order_region <- c("LIC", "LMC", "UMC", "HIC", "ECS", "MEA", "EAS", "SAS", "NAC", "LCN", "SSF", "WLD")
        custom_labels_region <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "Europe & C. Asia", "Middle East & N. Africa", "E. Asia & Pacific", "South Asia", "North America", "Latin Am. & Caribbean", "Sub-Saharan Africa", "World")
        
        data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
        
        ggplot(data, aes(x = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")), y = value, fill = macrofoods)) +
          geom_col(color = "white", width = 0.6) +
          scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          facet_wrap(~ region_custom,ncol = 2) +
          scale_fill_manual(values = colors_macro) +
          #geom_text_repel(aes(label = value), show.legend = FALSE) +
          labs(x = NULL, y = "Impact", fill = "Category:") +
          theme_linedraw() +
          theme(axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(axis.text.x = element_text(face="bold"), axis.title.y = element_text(size = 12), strip.text.x = element_text(size = 12), legend.position = "right")
        
    })
      
      output$plot_categorymacro <- renderPlot({
        data <- filtered_data_categorymacro()
        
        custom_order_region <- c("LIC", "LMC", "UMC", "HIC", "ECS", "MEA", "EAS", "SAS", "NAC", "LCN", "SSF", "WLD")
        custom_labels_region <- c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income", "Europe & C. Asia", "Middle East & N. Africa", "E. Asia & Pacific", "South Asia", "North America", "Latin Am. & Caribbean", "Sub-Saharan Africa", "World")
        
        data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
        
        ggplot(data, aes(x = factor(macrofoods, level=c("ASF", "Staples", "Other")), y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
          geom_col(color = "white", width = 0.6) +
          #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          facet_wrap(~ region_custom ,ncol = 4) +
          scale_fill_manual(values = colors_food) +
          #geom_text_repel(aes(label = value), show.legend = FALSE) +
          labs(x = NULL, y = "Impact", fill = "Food group:") +
          theme_linedraw() +
          theme(axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          #theme(axis.text.x = element_text(face="bold"), axis.title.y = element_text(size = 12), strip.text.x = element_text(size = 12), legend.position = "right")
        
      })
      
      ###########
      #Draw plots for tabs in the fourth item
      ###########
      output$plot_consumption <- renderPlot({
        data <- filtered_data_consumption()
        ggplot(data, aes(x = Food.group, y = Intake, color = Indicator, shape = Indicator)) +
          geom_point(size = 4) +
          scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          #geom_text_repel(aes(label = Intake), show.legend = FALSE) +
          facet_wrap(~ Region,ncol = 2) +
          scale_color_discrete(name  ="Intake proxy",
                                breaks=c("GDD", "FBS", "GDD_adj_IOM", "FBS_adj_IOM"),
                                labels=c("Global Dietary Dataset (GDD)", "FAO Balance Sheet (FBS)", "GDD - energy adjusted", "FBS - energy adjusted")) +
          scale_shape_discrete(name  ="Intake proxy",
                                breaks=c("GDD", "FBS", "GDD_adj_IOM", "FBS_adj_IOM"),
                                labels=c("Global Dietary Dataset (GDD)", "FAO Balance Sheet (FBS)", "GDD - energy adjusted", "FBS - energy adjusted")) +
          # scale_fill_manual(values = colors_macro) +
          #If I assign different aesthetics to the same variable, labelling the legend with a common name will
          #force ggplot to create a single legend containing info on both. Here I assign the name
          #"Intake proxy" to the legend for both color and shape, which I mapped to the same variable
          #and ggplot automatically merges both in a single legend
          labs(x = NULL, y = "Intake (g/day)", color = "Intake proxy", shape = "Intake proxy") +
           theme_linedraw() +
            theme(axis.text.x = element_text(size=12), axis.title.y = element_text(size = 12, face = "bold"), strip.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12, face = "bold"))
          # 
      })
      
      ###########
      #Draw plots for tabs in the fifth item
      ###########
      
      output$plot_FBSintake <- renderPlot({
        data <- filtered_data_FBSintake()
        ggplot(data, aes(x = factor(Education, level=c("low", "medium", "high", "all-e")), y = Value, fill = Food.group)) +
          geom_col(color = "white", width = 0.6) +
          #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          facet_grid(Sex ~ Urbanisation) +
          labs(x = "Education Level", y = "Daily Intake", fill = "Food group:")
      })
      
      output$plot_FBSintake_fg <- renderPlot({
        data <- filtered_data_FBSintake_fg()
        ggplot(data, aes(x = Food.group, y = Value, color = Food.group)) +
          geom_point(size = 4) +
          scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          #geom_text_repel(aes(label = Value), show.legend = FALSE) +
          facet_wrap(~ Region, ncol = 2) +
          labs(x = "Food Group", y = "Daily Intake", color = "Food group:")
      })
      
      output$plot_FBSintake_fg_socio <- renderPlot({
        data <- filtered_data_FBSintake_fg_socio()
        ggplot(data, aes(x = Food.group, y = Value, color = Sex)) +
          geom_point(size = 4) +
          scale_x_discrete(guide = guide_axis(n.dodge=3)) +
          #geom_text_repel(aes(label = Value), show.legend = FALSE) +
          facet_grid(Education ~ Urbanisation) +
          labs(x = "Food Group", y = "Daily Intake", color = "Sex:")
      })
      
      ##################
      #Generate data tables
      ##################
      
      #Generating a table from the user's data selection is not straightforward in cases where the plot is faceting using the
      #commands #face_wrap or #facet_grid. For example, in the sexage tab the code offers the user the chance to facet multiple plots
      #in a single image by selecting to display data for several regions at once. The age group is on the x axis, the impact value is on the
      #y axis, and the sex category drives the coloring of the data points. This adds another layer of data that a static table cannot handle.
      #The code below works around this issue by going through a few steps of data manipulation. The bulk of it was suggested by Chat GPT, but I had to make
      #adjustments to make it work in this context. There may be an easier way to code this, but I have not been able to find it yet.
      
      
      #Create table for the sexage tab, starting from the subsection of the main dataset identified by the filtered_data_sexage element, that here is called as a function. 
        sexage_table <- reactive({
          data <- filtered_data_sexage()
        
          #Add a unique identifier column based on all relevant variables. This is needed because otherwise the datatable command used further down will automatically
          #group into a single row instances where different combinations of variables in the dataset correspond to the same impact value on the y axis.
          #If for example FML aged 11-19 have an impact value of 1.19 in both HIC and LIC, the datatable command would display just one row for the value 1.19, and list 
          #both HIC and LIC in the region column. Creating a unique identifier that is formed by the values taken by each variable in each occurrence ensures
          #that we can generate a table in which duplicate values are presented in distinct rows.
          data <- data %>%
          mutate(unique_id = paste(measure, env_itm, cns_prsp, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
        
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
          sexage_data <- sexage_data[, !colnames(sexage_data) %in% "unique_id"]
        
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
            sexage_data_export <- sexage_data_export[, !colnames(sexage_data_export) %in% "unique_id"]
          
              # Convert all columns to character
              sexage_data_export[] <- lapply(sexage_data_export, as.character)
          
              # Write the data to a CSV file
              write.csv(sexage_data_export, file, row.names = FALSE)
        }
      )
      
      
      
      
      #Create table for the eduurb tab, starting from the subsection of the main dataset identified by the filtered_data_eduurb element, that here is called as a function. 
      eduurb_table <- reactive({
        data <- filtered_data_eduurb()
          data <- data %>%
          mutate(unique_id = paste(measure, env_itm, cns_prsp, food_group, box, sex.urbanisation, age.education, region, sep = "_"))
        
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
          eduurb_data <- eduurb_data[, !colnames(eduurb_data) %in% "unique_id"]
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
          eduurb_data_export <- eduurb_data_export[, !colnames(eduurb_data_export) %in% "unique_id"]

          # Convert all columns to character
          eduurb_data_export[] <- lapply(eduurb_data_export, as.character)

          # Write the data to a CSV file
          write.csv(eduurb_data_export, file, row.names = FALSE)
        }
      )

      #Generate code to download the plot
      
      
      
      
      
      
      }
    
    
    ########################################
    #Run the Shiny app.
    ########################################
    
    shinyApp(ui = ui, server = server)
