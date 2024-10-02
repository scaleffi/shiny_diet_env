tabItem(
  tabName = "radar_regionincome",
  fluidPage(title = "Impact by income region (radar)",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_12", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_12", "Select Measure:", choices = c(
                         #"ratio to regional mean (capita)",
                         "ratio to global mean (capita)"
                         #"ratio to global avg (absolute)",
                         #"ratio to regional avg (absolute)"
                       ),
                       selected = "ratio to global mean (capita)")
                ),
                column(4,
                       selectInput("env_dimensions_12", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), multiple = TRUE,
                                   selected = "average environmental impact")
                       #selectInput("region_12", "Select Region:", choices = c("WLD", "HIC", "UMC", "LMC", "LIC"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       downloadButton("download_csv_regionradarincome", "Download table"),
                       downloadButton("download_plot_regionradarincome", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_regionradarincome"
                )),
                tabPanel("Table",tableOutput("regionradarincome_table"))
              )
            )
  )
)