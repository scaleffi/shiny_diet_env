library(shinydashboard)
library(tidyverse)
#----
#Start the UI (User Interface). This section gives instructions on the visual display and structure of the dashboard. It generates a Shiny object.


#UI ----

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "LSHTM - Global diets data explorer",
                      titleWidth = 450
                    ),
                    # titlePanel("The environmental footprints of global diets"),----
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
                        ###First item----
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
                              #Sexage----
                              "Sex-age",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("cns_prsp_2", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_2", "Select Measure:", choices = c("Ratio to World average (capita)","Ratio to Regional average (capita)"), selected = "Ratio to World average (capita)"),
                                  selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "Average"),
                                  selectInput("region_2", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("LIC", "LMC", "UMC", "HIC")),
                                  selectInput("age.education_2", "Select age group:", choices = c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-9", "10-19", "20-39", "40-64", "65+")),
                                  selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML")),
                                  downloadButton("download_csv_sexage", "Download table"),
                                  downloadButton("download_plot_sexage", "Download plot")
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
                              #Eduurb ----
                              "Edu-urb",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary", 
                                  selectInput("cns_prsp_3", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_3", "Select Measure:", choices = c("Ratio to World average (capita)","Ratio to Regional average (capita)"), selected = "Ratio to World average (capita)"),
                                  selectInput("env_dimensions_3", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "Average"),
                                  selectInput("region_3", "Select Region:", choices = unique(df$region), multiple = TRUE, selected = "WLD"),
                                  selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high"), multiple = TRUE, selected = c("low", "medium", "high")),
                                  selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural"), multiple = TRUE, selected = c("urban", "rural")),
                                  downloadButton("download_csv_eduurb", "Download table"),
                                  downloadButton("download_plot_eduurb", "Download plot")
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
                              #Sociodem ----
                              "Sociodem",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("cns_prsp_7", "Select Consumption Perspective:", choices = unique(df_trs_category$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_7", "Select Measure:", choices = c("Absolute","Per capita"), selected = "Per capita"),
                                  selectInput("env_dimensions_7", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), "Average"), selected = "GHG (MT CO2eq)"),
                                  selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = "WLD"),
                                  selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = "total"),
                                  selectInput("age_7", "Select sociodemographic:", choices = setdiff(unique(df_trs_category$age), c("all-a", "all-e", "BTH", "all-u")), multiple = TRUE, selected = c("low", "medium", "high", "urban", "rural")),
                                  downloadButton("download_csv_sociodem", "Download table"),
                                  downloadButton("download_plot_sociodem", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                                "These tabs allow you to compare absolute and per capita diet-related environmental footprints of diets across income and geographical regions.<br><br>
                      In both tabs, you can see how much each food group contributes to the environmental footprint across six different dimensions: GHG (MT CO2eq) emissions, Eutrophication, Land Use, Land Use - Pasture, Land Use - Crops,
                      Freshwater Withdrawals.<br><br> If you want to 
                      compare across income regions (Low Income Countries, Low-Middle Income Countries, Upper-Middle Income Countries, High Income Countries), open the first tab. Explore the second tab instead
                      if your focus is on geographic groupings."
                              )
                            )),
                            tabPanel(
                              #Region ----
                              "Impacts by income region",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",  
                                  selectInput("cns_prsp_1", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_1", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita"),
                                  selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"),multiple = TRUE, selected = "GHG (MT CO2eq)"),
                                  selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = "total"),
                                  selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("LIC", "LMC", "UMC", "HIC")),
                                  downloadButton("download_csv_region", "Download table"),
                                  downloadButton("download_plot_region", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("cns_prsp_5", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_5", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Per capita"),
                                  selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"),multiple = TRUE, selected = "GHG (MT CO2eq)"),
                                  selectInput("food_group_5", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c("beef_lamb", "dairy", "rice", "roots")),
                                  selectInput("region_5", "Select Region:", choices = c("NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF", "WLD"), multiple = TRUE, selected = c("NAC", "SAS", "SSF")),
                                  downloadButton("download_csv_regiongeo", "Download table"),
                                  downloadButton("download_plot_regiongeo", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                                "These tabs allow you to compare absolute and per capita environmental footprints associated with consumption of fifteen food groups and three macrocategories, across regions.<br><br>
                      We grouped the fifteen food groups into three macrocategories: Animal Source Foods (ASF), Staples, and Other. This is an arbitrary classification made for the
                      purpose of this dashboard, with the goal of making it easier for the user to make comparisons at a glance. By combining information on both the food group
                      and the macrocategory, the user can explore how different categories of food impact each environmental dimension, while also seeing how individual food groups contribute within each category.<br><br>
                      Open the first tab if you want to focus on the fifteen separate food groups, and compare their impacts across regions. Select the second tab if instead you want to focus on the three macrocategories."
                              )
                            )),
                            tabPanel(
                              #Category ----
                              "Impacts by food group",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("cns_prsp_4", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_4", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Absolute"),
                                  selectInput("env_dimensions_4", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "Average"), selected = "GHG (MT CO2eq)"),
                                  selectInput("food_group_4", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef_lamb", "rice", "grains", "fruit_veg", "legumes")),
                                  selectInput("region_4", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = "WLD"),
                                  downloadButton("download_csv_category", "Download table"),
                                  downloadButton("download_plot_category", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("cns_prsp_6", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "Actual Consumption"),
                                  selectInput("measure_6", "Select Measure:", choices = c("Absolute", "Per capita"), selected = "Absolute"),
                                  selectInput("env_dimensions_6", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("Average", "Land use, pasture (thousands of Km2)", "Land use, crops (thousands of Km2)")), selected = "GHG (MT CO2eq)"),
                                  selectInput("food_group_6", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef_lamb", "pork", "dairy", "legumes", "roots", "rice", "grains")),
                                  #selectInput("category_6", "Select Food Category:", choices = setdiff(unique(df_trs_macrof$macrofoods), "Total"), multiple = TRUE),
                                  selectInput("region_6", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("LIC", "HIC")),
                                  downloadButton("download_csv_categorymacro", "Download table"),
                                  downloadButton("download_plot_categorymacro", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
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
                              #Cons_compare ----
                              "Compare consumption proxies",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("measure_8", "Select Measure:", choices = unique(df_cons$Measure), selected = "Absolute"),
                                  selectInput("indicator_8", "Select Proxy:", choices = c("GDD", "FBS", "GDD_adj_IOM", "FBS_adj_IOM"), multiple = TRUE, selected = "GDD"),
                                  selectInput("region_8", "Select Region:", choices = unique(df_cons$Region),multiple = TRUE, selected = "WLD"),
                                  selectInput("food_group_8", "Select Food Group:", choices = unique(df_cons$Food.group), multiple = TRUE, selected = c("dairy", "fruits", "nuts")),
                                  selectInput("stats_8", "Select Statistic of interest:", choices = unique(df_cons$Stats), selected = "mean"),
                                  downloadButton("download_csv_consumption", "Download table"),
                                  downloadButton("download_plot_consumption", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                                  tabsetPanel(
                                    tabPanel("Plot", plotOutput("plot_consumption"
                                                                #, height = 400
                                    )),
                                    tabPanel("Table",tableOutput("consumption_table"))
                                  )
                                )
                              )
                            )
                          )
                        ),
                        tabItem(
                          ####Fifth Item ----
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
                              #FBSsociodem ----
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
                                  selectInput("education_9", "Select Education Level:", choices = unique(df_FBSintake$Education),multiple = TRUE, selected = c("low", "medium", "high")),
                                  downloadButton("download_csv_FBSintake", "Download table"),
                                  downloadButton("download_plot_FBSintake", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                                  tabsetPanel(
                                    tabPanel("Plot", plotOutput("plot_FBSintake"
                                                                #, height = 400
                                    )),
                                    tabPanel("Table",tableOutput("FBSintake_table"))
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              #FBSregion ----
                              "Cumulative Intake by food group, across Regions",
                              fluidRow(
                                box(
                                  width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "primary",
                                  selectInput("unit_10", "Select Unit:", choices = unique(df_FBSintake$Unit), selected = "g/d_w"),
                                  selectInput("food_group_10", "Select Food Group:", unique(df_FBSintake$Food.group), multiple = TRUE, selected = c("rice", "wheat", "roots")),
                                  selectInput("region_10", "Select Region:", choices = unique(df_FBSintake$Region),multiple = TRUE, selected = c("LIC", "LMC", "UMC", "HIC")),
                                  #selectInput("age_9", "Select Age Group:", choices = unique(df_FBSintake$Age), multiple = TRUE, selected = "all-a"),
                                  selectInput("sex_10", "Select Sex:", choices = unique(df_FBSintake$Sex), selected = "BTH"),
                                  selectInput("urbanisation_10", "Select Urbanisation Level:", choices = unique(df_FBSintake$Urbanisation), selected = "rural"),
                                  selectInput("education_10", "Select Education Level:", choices = unique(df_FBSintake$Education), selected = "low"),
                                  downloadButton("download_csv_FBSintake_fg", "Download table"),
                                  downloadButton("download_plot_FBSintake_fg", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                                  tabsetPanel(
                                    tabPanel("Plot", plotOutput("plot_FBSintake_fg"
                                                                #, height = 400
                                    )),
                                    tabPanel("Table",tableOutput("FBSintake_fg_table"))
                                  )
                                )
                              )
                            ),
                            tabPanel(
                              #FBS food groups ----
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
                                  selectInput("education_11", "Select Education Level:", choices = unique(df_FBSintake$Education),multiple = TRUE, selected = c("low", "medium", "high")),
                                  downloadButton("download_csv_FBSintake_fg_socio", "Download table"),
                                  downloadButton("download_plot_FBSintake_fg_socio", "Download plot")
                                ),
                                box(
                                  width = 9, collapsible = T, solidHeader = FALSE, status = "primary",
                                  tabsetPanel(
                                    tabPanel("Plot", plotOutput("plot_FBSintake_fg_socio"
                                                                #, height = 400
                                    )),
                                    tabPanel("Table",tableOutput("FBSintake_fg_socio_table"))
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
                                  "This dashboard offers users the chance to explore data on the composition of global diets, and on their related environmental footprints. 
                        It uses new datasets and estimates, developed by Prof. Marco Springmann at the London School of Hygiene and Tropical Medicine, and is divided in two main sections. One focuses on consumption
                        data, expressed in daily intakes, and the other on diet-related environmental footprints.<br><br> 
                        The data can be filtered by region, country, and by characteristics such as Sex, Age Group, Education Level, etc.
                        To better manage the level of detail available in these new estimates, we have divided the dashboard in 
                        distinct sections that focus on a specific number of dimensions at a time. Within each section,
                        the user is free to choose among several combinations of filters and generate plots of their interest.
                        All data can be visualised as a plot or a table; data can be downloaded in table format as a .csv file and is usable/reproducible, as long as their source is clearly and correctly cited.<br><br>
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