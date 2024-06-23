# library(shinydashboard)
# library(shiny)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(
                      title = "The environmental footprints of global diets",
                      titleWidth = 450
                    ),
                    # titlePanel("The environmental footprints of global diets"),----
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("View by sociodemographic", 
                                 #tabName = "sociodem",
                                 tabName = NULL,
                                 icon = icon("person-half-dress"),
                                 menuSubItem("About this data", tabName = "about_sociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Sex and age", tabName = "sexage", icon = icon("person-half-dress")),
                                 menuSubItem("Edu. and urb.", tabName = "eduurb", icon = icon("person-half-dress")),
                                 menuSubItem("Absolute impacts, by sociodem", tabName = "multisociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Relative impacts, by sociodem", tabName = "multisociodem_rel", icon = icon("person-half-dress"))
                                 #menuSubItem("All sociodem", tabName = "all_sociodem", icon = icon("person-half-dress"))
                        ),
                        menuItem("View by region", tabName = NULL, icon = icon("earth-africa"),
                                 menuSubItem("About this data", tabName = "about_region", icon = icon("earth-africa")),
                                 menuSubItem("Income regions", tabName = "region", icon = icon("earth-africa")),
                                 menuSubItem("Geographical regions", tabName = "regiongeo", icon = icon("earth-africa"))
                        ),
                        menuItem("View by food group", tabName = NULL, icon = icon("wheat-awn"),
                                 menuSubItem("About this data", tabName = "about_categories",icon = icon("wheat-awn")),
                                 menuSubItem("Food groups", tabName = "foodgroups", icon = icon("wheat-awn")),
                                 menuSubItem("Food macrocategories", tabName = "foodmacro", icon = icon("wheat-awn"))
                        ),
                        #menuItem("Plots for paper", tabName = NULL,
                        # menuSubItem("Radar by region", tabName = "radar_region"),
                        # menuSubItem("Radar by region (geo)", tabName = "radar_regiongeo")),
                        menuItem("Info", tabName = "readme", icon = icon("info-circle")
                                 , selected = TRUE
                        ) 
                      ), collapsed = FALSE
                    ),
                    # mainPanel(
                    dashboardBody(
                      tabItems(
                        ###First item----
                        tabItem(
                          #tabName = "sociodem",
                          # tabItem(
                          tabName = "about_sociodem", 
                          box(width = 12,
                              title = "About this data", HTML(
                                "These tabs allow you to compare diet-related environmental footprints across different sociodemographics.
              Use the first tab, 'Sex and age', if you want to explore how diet-related environmental footprints differ among sexes and age groups, relative to the global
              or regional average. Open the second tab instead, 'Edu-urb', if you want to explore how the footprints change across education and urbanisation levels, 
              relative to the global or regional average.<br><br> 
              In both tabs, the values can be interpreted as percentages which show how much higher or lower the impact of a 
              specific group is, compared to a global or regional average - set at 100%. For example, a value of 137 corresponds to a diet that is 37% higher than the mean.
              A value of 81 corresponds to a diet that is 19% lower than the mean.<br><br>
              If you are interested in seeing how absolute and relative impacts differ across sociodemographics, regions, and environmental dimensions, open the third and fourth tab.<br><br>
              In all tabs, you can visualise the data through two measures: actual demand, or demand normalised to 2,000 kcal/day. The first measure, actual demand, shows the footprints
              based on the real demand estimated for the underlying food. But comparing environmental footprints across sociodemographics using only this measure
              may be misleading: children, for example, eat less than adults in absolute terms, making their footprints invariably smaller in absolute terms. To control for these biophysical factors, we also calculated footprints
              based on demand normalised to 2,000 kcal/day for all groups, while maintaining dietary composition. This means that by selecting this second
              measure in the input parameters, users can, for example, compare the diet-related environmental footprints of children between 0-10 and adults
              aged 20-39 as if they were both eating 2,000 kcal/day - but maintaining their respective proportional dietary compositions. Many large differences across sexes and age groups
              disappear or lose significance when using this measure of normalised demand, suggesting that they may simply be driven by the different
              energy requirements of individuals in each group. 
              ")
                          )
                          #)
                        ),
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
                        ),
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
                                               selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high"), multiple = TRUE, selected = c("low", "medium", "high")),
                                               selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural"), multiple = TRUE, selected = c("urban", "rural"))
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
                        ),
                        tabItem(
                          tabName = "multisociodem",
                          #Sociodem ----
                          fluidPage(title = "Compare footprints across multiple dimensions",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(3,
                                               selectInput("dmd_scn_7", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_7", "Select Measure:", 
                                                           choices = c("absolute","per capita"),
                                                           selected = "per capita")
                                        ),
                                        column(3,
                                               selectInput("env_dimensions_7", "Select Environmental Dimensions:", 
                                                           choices = setdiff(unique(df_trs_category$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")),
                                                           selected = "GHG (Mt CO2eq)"),
                                               selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = c(
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
                                                 "other"))
                                        ),
                                        column(3,
                                               selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                                               selectInput("age_7", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                                                 "65+"))
                                        ),
                                        column(3,
                                               downloadButton("download_csv_sociodem", "Download table"),
                                               br(),
                                               br(),
                                               downloadButton("download_plot_sociodem", "Download plot")
                                        )
                                      )
                                    ),  
                                    box(
                                      width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                                      tabsetPanel(
                                        tabPanel("Plot", plotOutput("plot_sociodem"
                                                                    #, height = 400
                                        )),
                                        tabPanel("Table",tableOutput("sociodem_table"))
                                        
                                      )
                                    )
                          )
                        ),
                        tabItem(
                          tabName = "multisociodem_rel",
                          #Sociodem_rel ----
                          fluidPage(title = "Compare relative footprints across multiple dimensions",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(3,
                                               selectInput("dmd_scn_8", "Select Demand Perspective:", choices = unique(df_trs_category$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_8", "Select Measure:", choices = c(
                                                 "ratio to global avg (absolute)", 
                                                 "ratio to regional avg (absolute)",
                                                 "ratio to regional mean (capita)",
                                                 "ratio to global mean (capita)"),
                                                 selected = "ratio to regional avg (absolute)")
                                        ),
                                        column(3,
                                               selectInput("env_dimensions_8", "Select Environmental Dimensions:", choices = unique(df_trs_category$env_itm), selected = "average environmental impact"),
                                               selectInput("age_8", "Select sociodemographic:", choices = unique(df_trs_category$age), multiple = TRUE, selected = c(
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
                                                 "65+"))
                                               # selectInput("category_8", "Select sociodem category:", choices = unique(df_trs_category$category), multiple = TRUE, selected = c(
                                               #   "Age",
                                               #   "Sex",
                                               #   "Edu. level",
                                               #   "Urb. level"
                                               # ))
                                               
                                        ),
                                        column(3,
                                               selectInput("region_8", "Select Region:", choices = unique(df_trs_category$region), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                                               selectInput("food_group_8", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = c(
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
                                               ))
                                        ),
                                        column(3,
                                               downloadButton("download_csv_sociodem_rel", "Download table"),
                                               br(),
                                               br(),
                                               downloadButton("download_plot_sociodem_rel", "Download plot")
                                        )
                                      )
                                    ),  
                                    box(
                                      width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                                      tabsetPanel(
                                        tabPanel("Plot", plotOutput("plot_sociodem_rel"
                                                                    #, height = 400
                                        )),
                                        tabPanel("Table",tableOutput("sociodem_rel_table"))
                                        
                                      )
                                    )
                          )
                        ),
                        tabItem(
                          tabName = "all_sociodem",
                          #Sociodem_all ----
                          fluidPage(title = "Compare footprints across all dimensions",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(3,
                                               selectInput("dmd_scn_9", "Select Demand Perspective:", choices = unique(df_sel$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_9", "Select Measure:", choices = unique(df_sel$measure), selected = "absolute")
                                               #             c(
                                               # "ratio to global avg (absolute)", 
                                               # "ratio to regional avg (absolute)",
                                               # "ratio to regional mean (capita)",
                                               # "ratio to global mean (capita)",
                                               # "absolute",
                                               # "per capita"),
                                               # selected = "absolute")
                                        ),
                                        column(3,
                                               selectInput("env_dimensions_9", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")), selected = "GHG (Mt CO2eq)"),
                                               selectInput("age_9", "Select age:", choices = unique(df_sel$age), multiple = TRUE, selected = c(
                                                 "0-9",
                                                 "10-19",
                                                 "20-39",
                                                 "40-64",
                                                 "65+")),
                                               selectInput("sex_9", "Select sex:", choices = unique(df_sel$sex), multiple = TRUE, selected = c(
                                                 "MLE",
                                                 "FML"
                                               ))
                                        ),
                                        column(3,
                                               selectInput("region_9", "Select region:", choices = unique(df_sel$region), multiple = TRUE, selected = c("WLD")),
                                               selectInput("urbanisation_9", "Select urbanicity:", choices = unique(df_sel$urban), multiple = TRUE, selected = c(
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
                        ),
                        tabItem(
                          ###Second item ----
                          tabName = "about_region",
                          box(width = 12,
                              title = "About this data",HTML(
                                "These tabs allow you to compare absolute and per capita diet-related environmental footprints across income regions and geographical regions.<br><br>
              In both tabs, you can also see how much each food group contributes to the environmental footprint across the six different dimensions.<br><br> 
              If you want to compare across income regions (Low Income Countries, Low-Middle Income Countries, Upper-Middle Income Countries, High Income Countries), open the first tab. Explore the second tab instead
              if your focus is on geographical groupings."
                              )
                          )
                        ),
                        tabItem(
                          #Region ----
                          tabName = "region",
                          fluidPage(title = "Compare footprints across income regions",
                                    box(
                                      width = 12, title = "Select input parameters" , collapsible = T, solidHeader = TRUE, status = "primary",
                                      fluidRow(
                                        column(4,
                                               selectInput("dmd_scn_1", "Select Demand Perspective:", choices = unique(df$dmd_scn), selected = "actual demand"),
                                               selectInput("measure_1", "Select Measure:", choices = c(
                                                 "absolute", 
                                                 "per capita",
                                                 "ratio to global avg (absolute)", 
                                                 "ratio to regional avg (absolute)",
                                                 "ratio to regional mean (capita)",
                                                 "ratio to global mean (capita)"), selected = "per capita")
                                        ),
                                        column(4,
                                               selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("average environmental impact", "average environmental impact (pb weighted)")),multiple = TRUE, selected = c(
                                                 "GHG (Mt CO2eq)",
                                                 "land use (thousands of km2)",
                                                 "water use (km3)",
                                                 "eutrophication pot. (kt PO4eq)"
                                               )),
                                               selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC", "WLD"), multiple = TRUE, selected = c("WLD", "HIC", "UMC", "LMC", "LIC"))
                                        ),
                                        column(4,
                                               selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c(
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
                                               downloadButton("download_csv_region", "Download table"),
                                               downloadButton("download_plot_region", "Download plot")
                                        )
                                      )
                                    ),
                                    box(
                                      width = 12, title = "Output", collapsible = T, solidHeader = TRUE, status = "primary",
                                      tabsetPanel(
                                        tabPanel("Plot", plotOutput("plot_region"
                                                                    #, height = 400
                                        )),
                                        tabPanel("Table",tableOutput("region_table"))
                                      )
                                    )
                          )
                        ),
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
                        ),
                        tabItem(
                          ###Third item ----
                          tabName = "about_categories",
                          box(width = 12,
                              title = "About this data", HTML(
                                "These tabs allow you to compare absolute and per capita environmental footprints associated with the estimated demand of fifteen food groups, across regions.<br><br>
              We also grouped the fifteen food groups into three macrocategories: Animal Source Foods (ASF), Staples, and Other. This is an arbitrary classification made for the
              purpose of this dashboard, with the goal of making it easier for the user to make comparisons at a glance. By combining information on both the food group
              and the macrocategory, the user can explore how different categories of food impact each environmental dimension, while also seeing how individual food groups contribute within each macrocategory.<br><br>
              Open the first tab if you want to focus on the fifteen separate food groups, and compare their impacts across regions. Select the second tab if instead you want to focus on the three macrocategories."
                              )
                          )),
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
                        ),
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
                        ),
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
                        ),
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
                                                           selected = "average environmental impact"),
                                               selectInput("region_11", "Select Region:", choices = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"), multiple = TRUE, selected = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"))
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
                        ),
                        tabItem(
                          ###Fifth item ----
                          tabName = "readme",
                          fluidRow(
                            box(width = 12,
                                title = "ReadMe",
                                div(
                                  HTML(
                                    "This dashboard allows you to explore and compare diet-related environmental footprints of global diets in 2020. 
                    It uses new estimates developed by Prof. Marco Springmann.<br><br> 
                    The data can be filtered or compared by region, country, sex, age group, urbanicity, and education level.
                    The dashboard is divided in three sections, accessible through the tabs in the menu on the left.<br><br>
                    The first section focuses on how footprints differ across sociodemographics (sex, age, urbanicty, education level).<br>
                    The second, on how they differ across regions, both income and geographic.<br> 
                    The third, on how they differ across food groups and macrocategories.<br><br> 
                    Within each section, you can build plots and data tables by choosing among several filters and inputs. 
                    All data can be visualised and downloaded as both a plot or a table. Plots and tables are reproducible, as long as their source is clearly and correctly cited.<br><br>
                    For feedback or questions please contact Sebastiano Caleffi at s.caleffi@ucl.ac.uk"
                                  )
                                  #style = "width:100%;"
                                  
                                )
                            )
                          )
                        )
                      )
                    )
)
