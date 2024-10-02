tabItem(
  tabName = "radar_regiongeo",
  fluidPage(title = "Impact by geographical region (radar)",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(4,
                       selectInput("dmd_scn_11", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                       selectInput("measure_11", "Select Measure:", choices = c(
                         #"ratio to regional mean (capita)",
                         "ratio to global mean (capita)"
                         #"ratio to global avg (absolute)",
                         #"ratio to regional avg (absolute)"
                       ),
                       selected = "ratio to global mean (capita)")
                ),
                column(4,
                       selectInput("env_dimensions_11", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), multiple = TRUE,
                                   selected = "GHG (Mt CO2eq)")
                       #selectInput("region_11", "Select Region:", choices = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"), multiple = TRUE, selected = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"))
                ),
                column(4,
                       downloadButton("download_csv_regionradargeo", "Download table"),
                       downloadButton("download_plot_regionradargeo", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_regionradargeo"
                )),
                tabPanel("Table",tableOutput("regionradargeo_table"))
              )
            )
  )
)