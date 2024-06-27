tabItem(
  tabName = "sexage",
  #Sexage----
  fluidPage(title = "Compare footprints by sex and age group",
            box(width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                fluidRow(
                  column(3,
                         selectInput("dmd_scn_2", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                         selectInput("measure_2", "Select Measure:", choices = c("ratio to global mean (capita)","ratio to regional mean (capita)"), selected = "ratio to global mean (capita)")
                  ),
                  column(3,
                         selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "average environmental impact"),
                         selectInput("region_2", "Select Region:", choices = unique(df$region),multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                  ),
                  column(3,
                         selectInput("age.education_2", "Select age group:", choices = c("0-9", "10-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-9", "10-19", "20-39", "40-64", "65+")),
                         selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML"))
                  ),
                  column(3,
                         downloadButton("download_csv_sexage", "Download table"),
                         br(),
                         br(),
                         downloadButton("download_plot_sexage", "Download plot")
                  )
                )
            ), 
            box(
              width = 12, title = "Output" , collapsible = T, solidHeader = TRUE, status = "primary",
              tabsetPanel(
                tabPanel("Plot", plotOutput("plot_sexage"
                                            #, height = 100
                )),
                tabPanel("Table",tableOutput("sexage_table"))
              )
            )
  )
)