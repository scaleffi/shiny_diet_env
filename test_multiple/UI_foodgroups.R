tabItem(
  #Category ----
  tabName = "foodgroups",
  fluidPage(title = "Compare footprints by individual food groups",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_4", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                       selectInput("measure_4", "Select Measure:", choices = c("absolute", "per capita"), selected = "per capita")
                ),
                column(4,
                       selectInput("env_dimensions_4", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")), selected = "GHG (Mt CO2eq)"),
                       selectInput("region_4", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       selectInput("food_group_4", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c(
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
                       downloadButton("download_csv_category", "Download table"),
                       downloadButton("download_plot_category", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_category"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("category_table"))
              )
            )
  )
)