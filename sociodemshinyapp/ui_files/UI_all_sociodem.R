tabItem(
  tabName = "all_sociodem",
  #Sociodem_all ----
  fluidPage(title = "Compare footprints across all dimensions",
            box(
              width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
              fluidRow(
                column(3,
                       selectInput("dmd_scn_9", "Select Demand Perspective:", choices = unique(df_sel$dmd_scn), selected = "actual demand"),
                       selectInput("measure_9", "Select Measure:", choices = c(
                         #"ratio to regional avg (absolute)",
                         "ratio to regional mean (capita)",
                         "ratio to global mean (capita)"),
                         selected = "ratio to regional mean (capita)")
                ),
                column(3,
                       selectInput("env_dimensions_9", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")), selected = "GHG (Mt CO2eq)"),
                       selectInput("age_9", "Select age:", choices = unique(df_sel$age), multiple = TRUE, selected = c(
                         "0-9",
                         "10-19",
                         "20-39",
                         "40-64",
                         "65+")),
                       selectInput("sex_9", "Select sex:", choices = unique(df_sel$sex), multiple = TRUE, selected = 
                                     c(
                         "MLE",
                         "FML"
                       ))
                ),
                column(3,
                       selectInput("region_9", "Select region:", choices = unique(df_sel$region), multiple = TRUE, selected = c("WLD")),
                       selectInput("urbanisation_9", "Select residence:", choices = unique(df_sel$urban), multiple = TRUE, selected = c(
                         "urban",
                         "rural"
                       )),
                       selectInput("education_9", "Select education:", choices = unique(df_sel$edu), multiple = TRUE, selected = c(
                         "low",
                         "medium",
                         "high"
                       ))
                ),
                column(3,
                       downloadButton("download_csv_all_sociodem", "Download table"),
                       br(),
                       br(),
                       downloadButton("download_plot_all_sociodem", "Download plot")
                )
              )
            ),  
            box(
              width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_all_sociodem"
                                            #, height = 400
                )),
                tabPanel("Table",tableOutput("all_sociodem_table"))
                
              )
            )
  )
)