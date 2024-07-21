tabItem(
  #Categorymacro ----
  tabName = "foodmacro",
  fluidPage(title = "Compare footprints by food macro-categories",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_6", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                       selectInput("measure_6", "Select Measure:", choices = c("absolute", "per capita"), selected = "per capita")
                ),
                column(4,
                       selectInput("env_dimensions_6", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact","average environmental impact (pb weighted)", "land use, pasture (thousands of km2)", "land use, crops (thousands of km2)")), selected = "GHG (Mt CO2eq)"),
                       selectInput("region_6", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       selectInput("food_group_6", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c(
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
                       )),
                       downloadButton("download_csv_categorymacro", "Download table"),
                       downloadButton("download_plot_categorymacro", "Download plot")
                )
              )
            ),
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_categorymacro"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("categorymacro_table"))
              )
            )
  )
)