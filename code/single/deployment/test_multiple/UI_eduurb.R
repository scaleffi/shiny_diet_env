tabItem(
  tabName = "eduurb",
  #Eduurb ----
  fluidPage(title = "Compare footprints by urbanisation and education levels",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(3,
                       selectInput("dmd_scn_3", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                       selectInput("measure_3", "Select Measure:", choices = c("ratio to global mean (capita)","ratio to regional mean (capita)"), selected = "ratio to global mean (capita)")
                ),
                column(3,
                       selectInput("env_dimensions_3", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "average environmental impact"),
                       selectInput("region_3", "Select Region:", choices = unique(df$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                ),
                column(3,
                       selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high", "all-e"), multiple = TRUE, selected = c("low", "medium", "high")),
                       selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural", "all-u"), multiple = TRUE, selected = c("urban", "rural"))
                ),
                column(3,
                       downloadButton("download_csv_eduurb", "Download table"),
                       br(),
                       br(),
                       downloadButton("download_plot_eduurb", "Download plot")
                )
              )
            ), 
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_eduurb"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("eduurb_table"))
              )
            )
  )
)