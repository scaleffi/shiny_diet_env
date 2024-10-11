library(shinydashboard) # needed here even if already loaded in app.R

myUI <- dashboardPage(
                    skin = "black",
                    dashboardHeader(
                      title = "The environmental footprints of global diets",
                      titleWidth = 450
                    ),
                    # titlePanel("The environmental footprints of global diets"),----
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("View by sociodemographic", 
                                 tabName = NULL,
                                 icon = icon("person-half-dress"),
                                 menuSubItem("About this data", tabName = "about_sociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Sex and age", tabName = "sexage", icon = icon("person-half-dress")),
                                 menuSubItem("Edu. and urb.", tabName = "eduurb", icon = icon("person-half-dress")),
                                 menuSubItem("Absolute impacts, by sociodem", tabName = "multisociodem", icon = icon("person-half-dress")),
                                 menuSubItem("Relative impacts, by sociodem", tabName = "multisociodem_rel", icon = icon("person-half-dress")),
                                 menuSubItem("All sociodem", tabName = "all_sociodem", icon = icon("person-half-dress"))
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
                        menuItem("Plots for paper", tabName = NULL,
                        menuSubItem("Radar by region", tabName = "radar_region"),
                        menuSubItem("Radar by region (geo)", tabName = "radar_regiongeo"),
                        menuSubItem("Radar by region (income)", tabName = "radar_regionincome"),
                        menuSubItem("Radar by env dimension", tabName = "radar_envitm")),
                        menuItem("Info", tabName = "readme", icon = icon("info-circle")
                                 , selected = TRUE
                        ) 
                      ), collapsed = FALSE
                    ),
                    # mainPanel(
                    dashboardBody(
                      tabItems(
                        source("ui_files/UI_aboutsociodem.R", local = TRUE)$value,
                        source("ui_files/UI_sexage.R", local = TRUE)$value,
                        source("ui_files/UI_eduurb.R", local = TRUE)$value,
                        source("ui_files/UI_multisociodem.R", local = TRUE)$value,
                        source("ui_files/UI_multisociodem_rel.R", local = TRUE)$value,
                        source("ui_files/UI_all_sociodem.R", local = TRUE)$value,
                        source("ui_files/UI_aboutregion.R", local = TRUE)$value,
                        source("ui_files/UI_region.R", local = TRUE)$value,
                        source("ui_files/UI_regiongeo.R", local = TRUE)$value,
                        source("ui_files/UI_aboutcategories.R", local = TRUE)$value,
                        source("ui_files/UI_foodgroups.R", local = TRUE)$value,
                        source("ui_files/UI_foodmacro.R", local = TRUE)$value,
                        source("ui_files/UI_radar_region.R", local = TRUE)$value,
                        source("ui_files/UI_radar_regiongeo.R", local = TRUE)$value,
                        source("ui_files/UI_radar_regionincome.R", local = TRUE)$value,
                        source("ui_files/UI_radar_envitm.R", local = TRUE)$value,
                        source("ui_files/UI_readme.R", local = TRUE)$value
                      )
                    )
)
