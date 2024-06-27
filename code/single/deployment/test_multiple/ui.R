library(shinydashboard)
# library(shiny)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "The environmental footprints of global diets",
                      titleWidth = 450
                    ),
                    # titlePanel("The environmental footprints of global diets"),----
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("View by sociodemographic", 
                                 #tabName = "sociodem",
                                 tabName = NULL,
                                 icon = icon("person-half-dress"),
                                 menuSubItem("About this data", tabName = "about_sociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Sex and age", tabName = "sexage", icon = icon("person-half-dress")),
                                 menuSubItem("Edu. and urb.", tabName = "eduurb", icon = icon("person-half-dress")),
                                 menuSubItem("Absolute impacts, by sociodem", tabName = "multisociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Relative impacts, by sociodem", tabName = "multisociodem_rel", icon = icon("person-half-dress")),
                                 menuSubItem("All sociodem", tabName = "all_sociodem", icon = icon("person-half-dress"))
                        ),
                        menuItem("View by region", tabName = NULL, icon = icon("earth-africa"),
                                 menuSubItem("About this data", tabName = "about_region", icon = icon("earth-africa")),
                                 menuSubItem("Income regions", tabName = "region", icon = icon("earth-africa")),
                                 menuSubItem("Geographical regions", tabName = "regiongeo", icon = icon("earth-africa"))
                        ),
                        menuItem("View by food group", tabName = NULL, icon = icon("wheat-awn"),
                                 menuSubItem("About this data", tabName = "about_categories",icon = icon("wheat-awn")),
                                 menuSubItem("Food groups", tabName = "foodgroups", icon = icon("wheat-awn")),
                                 menuSubItem("Food macrocategories", tabName = "foodmacro", icon = icon("wheat-awn"))
                        ),
                        #menuItem("Plots for paper", tabName = NULL,
                        # menuSubItem("Radar by region", tabName = "radar_region"),
                        # menuSubItem("Radar by region (geo)", tabName = "radar_regiongeo")),
                        menuItem("Info", tabName = "readme", icon = icon("info-circle")
                                 , selected = TRUE
                        ) 
                      ), collapsed = FALSE
                    ),
                    # mainPanel(
                    dashboardBody(
                      tabItems(
                        source('UI_aboutsociodem.R', local = TRUE)$value,
                        source('UI_sexage.R', local = TRUE)$value,
                        source('UI_eduurb.R', local = TRUE)$value,
                        source('UI_multisociodem.R', local = TRUE)$value,
                        source('UI_multisociodem_rel.R', local = TRUE)$value,
                        source('UI_all_sociodem.R', local = TRUE)$value,
                        source('UI_aboutregion.R', local = TRUE)$value,
                        source('UI_region.R', local = TRUE)$value,
                        source('UI_regiongeo.R', local = TRUE)$value,
                        source('UI_aboutcategories.R', local = TRUE)$value,
                        source('UI_foodgroups.R', local = TRUE)$value,
                        source('UI_foodmacro.R', local = TRUE)$value,
                        
                        
                        tabItem(
                          tabName = "radar_region",
                          fluidPage(title = "Impact by income region (radar)",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(4,
                                               selectInput("dmd_scn_10", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_10", "Select Measure:", choices = c(
                                                 "ratio to regional mean (capita)",
                                                 "ratio to global mean (capita)"
                                                 #"ratio to global avg (absolute)",
                                                 #"ratio to regional avg (absolute)"
                                               ),
                                               selected = "ratio to global mean (capita)")
                                        ),
                                        column(4,
                                               selectInput("env_dimensions_10", "Select Environmental Dimensions:", choices = unique(df$env_itm),
                                                           selected = "average environmental impact"),
                                               selectInput("age_10", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                                                 "65+")
                                               ),
                                               #selectInput("region_10", "Select Region:", choices = c("WLD", "HIC", "UMC", "LMC", "LIC"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                                        ),
                                        column(4,
                                               downloadButton("download_csv_regionradar", "Download table"),
                                               downloadButton("download_plot_regionradar", "Download plot")
                                        )
                                      )
                                    ), 
                                    box(
                                      width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                                      tabsetPanel(
                                        tabPanel("Plot", plotOutput("plot_regionradar"
                                        )),
                                        tabPanel("Table",tableOutput("regionradar_table"))
                                      )
                                    )
                          )
                        ),
                        tabItem(
                          tabName = "radar_regiongeo",
                          fluidPage(title = "Impact by geographical region (radar)",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(4,
                                               selectInput("dmd_scn_11", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_11", "Select Measure:", choices = c(
                                                 #"ratio to regional mean (capita)",
                                                 "ratio to global mean (capita)"
                                                 #"ratio to global avg (absolute)",
                                                 #"ratio to regional avg (absolute)"
                                               ),
                                               selected = "ratio to global mean (capita)")
                                        ),
                                        column(4,
                                               selectInput("env_dimensions_11", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), multiple = TRUE,
                                                           selected = "average environmental impact"),
                                               selectInput("region_11", "Select Region:", choices = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"), multiple = TRUE, selected = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"))
                                        ),
                                        column(4,
                                               downloadButton("download_csv_regionradargeo", "Download table"),
                                               downloadButton("download_plot_regionradargeo", "Download plot")
                                        )
                                      )
                                    ), 
                                    box(
                                      width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                                      tabsetPanel(
                                        tabPanel("Plot", plotOutput("plot_regionradargeo"
                                        )),
                                        tabPanel("Table",tableOutput("regionradargeo_table"))
                                      )
                                    )
                          )
                        ),
                        tabItem(
                          ###Fifth item ----
                          tabName = "readme",
                          fluidRow(
                            box(width = 12,
                                title = "ReadMe",
                                div(
                                  HTML(
                                    "This dashboard allows you to explore and compare diet-related environmental footprints of global diets in 2020. 
                    It uses new estimates developed by Prof. Marco Springmann.<br><br> 
                    The data can be filtered or compared by region, country, sex, age group, urbanicity, and education level.
                    The dashboard is divided in three sections, accessible through the tabs in the menu on the left.<br><br>
                    The first section focuses on how footprints differ across sociodemographics (sex, age, urbanicty, education level).<br>
                    The second, on how they differ across regions, both income and geographic.<br> 
                    The third, on how they differ across food groups and macrocategories.<br><br> 
                    Within each section, you can build plots and data tables by choosing among several filters and inputs. 
                    All data can be visualised and downloaded as both a plot or a table. Plots and tables are reproducible, as long as their source is clearly and correctly cited.<br><br>
                    For feedback or questions please contact Sebastiano Caleffi at s.caleffi@ucl.ac.uk"
                                  )
                                  #style = "width:100%;"
                                  
                                )
                            )
                          )
                        )
                      )
                    )
)
