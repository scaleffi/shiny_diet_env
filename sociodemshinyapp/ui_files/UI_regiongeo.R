tabItem(
  #Regiongeo ----
  tabName = "regiongeo",
  fluidPage(title = "Compare footprints across geographical regions",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_5", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                       selectInput("measure_5", "Select Measure:", choices = c("absolute", "per capita"), selected = "per capita")
                ),
                column(4,
                       selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact",
                                                                                                                                   "average environmental impact (pb weighted)")),
                                   multiple = TRUE, selected = c(
                         "GHG (Mt CO\u2082eq)",
                         "land use (thousands of km\u00B2)",
                         "water use (km\u00B3)",
                         "eutrophication pot. (kt PO\u2084eq)"
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