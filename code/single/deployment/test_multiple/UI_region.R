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
                         "ratio to regional avg (absolute)",
                         "ratio to regional mean (capita)",
                         "ratio to global mean (capita)"), selected = "per capita")
                ),
                column(4,
                       selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")),multiple = TRUE, selected = c(
                         "GHG (Mt CO2eq)",
                         "land use (thousands of km2)",
                         "water use (km3)",
                         "eutrophication pot. (kt PO4eq)"
                       )),
                       selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c(
                         "beef",
                         "lamb",
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
)