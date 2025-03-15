tabItem(
  tabName = "multisociodem",
  #Sociodem ----
  fluidPage(title = "Compare footprints across multiple dimensions",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(3,
                       selectInput("dmd_scn_7", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_7", "Select Measure:",
                                   choices = c("absolute","per capita"),
                                   selected = "per capita")
                ),
                column(3,
                       selectInput("env_dimensions_7", "Select Environmental Dimensions:",
                                   choices = setdiff(unique(df_trs_category$env_itm), c("average environmental impact", "average environmental impact (pb weighted)"))),
                                   #,selected = "GHG (Mt CO\u2082eq)"),
                       selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = 
                                    c(
                                      "beef",
                                      "lamb",
                                      "pork",
                                      "other meat",
                                      "fish",
                                      "dairy",  
                                      "eggs&fats",
                                      "other",
                                      "sugar",
                                      "oils",
                                      "nuts&seeds",
                                      "fruit&veg",
                                      "legumes",
                                      "roots",
                                      "rice",
                                      "grains"
                                    )
                         )
                ),
                column(3,
                       selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                       selectInput("age_7", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                ),
                column(3,
                       downloadButton("download_csv_sociodem", "Download table"),
                       br(),
                       br(),
                       downloadButton("download_plot_sociodem", "Download plot")
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