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
                       selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")),multiple = TRUE, selected = c(
                         "GHG (Mt CO2eq)",
                         "land use (thousands of km2)",
                         "water use (km3)",
                         "eutrophication pot. (kt PO4eq)"
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