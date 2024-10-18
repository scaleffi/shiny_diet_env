tabItem(
  ###Fifth item ----
  tabName = "readme",
  fluidRow(
    box(width = 12,
        title = "Info",
        div(
          HTML(
            "This dashboard allows you to explore and compare global food-related environmental footprints in 2020. 
                    It uses new estimates developed by Prof. Marco Springmann, and has been designed and developed
                    by Sebastiano Caleffi.<br><br> 
                    The footprints can be filtered and compared by region, sex, age group, urban/rural residence, and education level.
                    The dashboard is divided in three sections, accessible through the tabs in the menu on the left.<br>
                    The first section focuses on how footprints differ across sociodemographic groups (sex, age, urban/rural residence, education level).<br>
                    The second, on how they differ across regions, both income and geographic.<br> 
                    The third, on how they differ across food groups and macrocategories.<br><br> 
                    Within each section, you can build plots and data tables by choosing among several filters and inputs. 
                    All data can be visualised and downloaded as both a plot or a table. Plots and tables are reproducible, as long as their source is clearly and correctly cited.<br><br>
                    Estimates of food demand are built from FAOSTAT's Food Balance Sheets. Estimates
                    of daily intakes by sociodemographic are taken from the GDD.
                    Region- and food-specific environmental footprints are taken from Poore and Nemecek (2018).
                    For feedback or questions please contact Sebastiano Caleffi at s.caleffi@ucl.ac.uk"
          )
          #style = "width:100%;"
          
        )
    )
  )
)