tabItem(
  tabName = "multisociodem_rel",
  #Sociodem_rel ----
  fluidPage(title = "Compare relative footprints across multiple dimensions",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(3,
                       selectInput("dmd_scn_8", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_8", "Select Measure:", choices = c(
                         "ratio to global avg (absolute)", 
                         "ratio to regional avg (absolute)",
                         "ratio to regional mean (capita)",
                         "ratio to global mean (capita)"),
                         selected = "ratio to regional mean (capita)")
                ),
                column(3,
                       selectInput("env_dimensions_8", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), selected = "average environmental impact"),
                       selectInput("age_8", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                       # selectInput("category_8", "Select sociodem category:", choices = unique(df_trs_category$category), multiple = TRUE, selected = c(
                       #   "Age",
                       #   "Sex",
                       #   "Edu. level",
                       #   "Urb. level"
                       # ))
                       
                ),
                column(3,
                       selectInput("region_8", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                       selectInput("food_group_8", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = c(
                         "beef",
                         "lamb",
                         "pork",
                         "other meats",
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
                       ))
                ),
                column(3,
                       downloadButton("download_csv_sociodem_rel", "Download table"),
                       br(),
                       br(),
                       downloadButton("download_plot_sociodem_rel", "Download plot")
                )
              )
            ),  
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_sociodem_rel"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("sociodem_rel_table"))
                
              )
            )
  )
)