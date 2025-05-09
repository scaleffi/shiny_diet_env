tabItem(
  #Region ----
  tabName = "region",
  fluidPage(title = "Compare footprints across income regions",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_1", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                       selectInput("measure_1", "Select Measure:", choices = c(
                         "absolute", 
                         "per capita",
                         "ratio to global avg (absolute)", 
                         #"ratio to regional avg (absolute)",
                         #"ratio to regional mean (capita)",
                         "ratio to global mean (capita)"), 
                         selected = "per capita")
                ),
                column(4,
                       selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")),multiple = TRUE, selected = c(
                         "GHG (Mt CO\u2082eq)",
                         "land use (thousands of km\u00B2)",
                         "water use (km\u00B3)",
                         "eutrophication pot. (kt PO\u2084eq)"
                       )),
                       selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c(
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
)