tabItem(
  tabName = "radar_region",
  fluidPage(title = "Impact by income region (radar)",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_10", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_10", "Select Measure:", choices = c(
                         "ratio to regional mean (capita)",
                         "ratio to global mean (capita)"
                         #"ratio to global avg (absolute)",
                         #"ratio to regional avg (absolute)"
                       ),
                       selected = "ratio to global mean (capita)")
                ),
                column(4,
                       selectInput("env_dimensions_10", "Select Environmental Dimensions:", choices = unique(df$env_itm),
                                   selected = "average environmental impact"),
                       selectInput("age_10", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                         "65+")
                       ),
                       #selectInput("region_10", "Select Region:", choices = c("WLD", "HIC", "UMC", "LMC", "LIC"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       downloadButton("download_csv_regionradar", "Download table"),
                       downloadButton("download_plot_regionradar", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_regionradar"
                )),
                tabPanel("Table",tableOutput("regionradar_table"))
              )
            )
  )
)