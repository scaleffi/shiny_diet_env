tabItem(
  tabName = "radar_envitm",
  fluidPage(title = "Impact by environmental dimension (radar)",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_13", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_13", "Select Measure:", choices = c(
                         #"ratio to regional mean (capita)",
                         "ratio to global mean (capita)"
                         #"ratio to global avg (absolute)",
                         #"ratio to regional avg (absolute)"
                       ),
                       selected = "ratio to global mean (capita)")
                ),
                column(4,
                       # selectInput("env_dimensions_13", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), multiple = TRUE,
                       #             selected = "average environmental impact")
                       selectInput("region_13", "Select Region:", choices = c("HIC", "UMC", "LMC", "LIC"), multiple = TRUE, selected = c("HIC", "UMC", "LMC", "LIC"))
                ),
                column(4,
                       downloadButton("download_csv_envitmradar", "Download table"),
                       downloadButton("download_plot_envitmradar", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_envitmradar"
                )),
                tabPanel("Table",tableOutput("envitmradar_table"))
              )
            )
  )
)