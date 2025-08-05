tabItem(
  tabName = "contribution",
  fluidPage(title = "Compare contribution to total impacts",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(3,
                       selectInput("dmd_scn_14", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_14", "Select Measure:", choices = c(
                         "ratio to global avg (absolute)", 
                         "ratio to regional avg (absolute)"),
                         selected = "ratio to global avg (absolute)")
                ),
                column(3,
                       selectInput("env_dimensions_14", "Select Environmental Dimensions:", choices = c(
                         "GHG emissions",  # Subscript 2
                         "water use",
                         "land use",
                         "land use, crops",
                         "land use, pasture",
                         "eutrophication pot.",
                         "average environmental impact",
                         "average environmental impact (pb weighted)"
                       ), selected = "average environmental impact"),
                       selectInput("age_14", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                       selectInput("region_14", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                       selectInput("food_group_14", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = c(
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
                       ))
                ),
                column(3,
                       downloadButton("download_csv_contribution", "Download table"),
                       br(),
                       br(),
                       downloadButton("download_plot_contribution", "Download plot")
                )
              )
            ),  
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_contribution"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("contribution_table"))
                
              )
            )
  )
)