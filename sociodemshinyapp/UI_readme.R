tabItem(
  ###Fifth item ----
  tabName = "readme",
  fluidRow(
    box(width = 12,
        title = "ReadMe",
        div(
          HTML(
            "This dashboard allows you to explore and compare diet-related environmental footprints of global diets in 2020. 
                    It uses new estimates developed by Prof. Marco Springmann, and has been designed and developed
                    by Sebastiano Caleffi.<br>Food demand data is taken from FAOSTAT; daily intakes by sociodemographic are taken from the GDD; 
                    region- and food-specific environmental footprints are taken from Poore and Nemecek (2018).<br><br> 
                    The data can be filtered or compared by region, country, sex, age group, urban/rural residence, and education level.
                    The dashboard is divided in three sections, accessible through the tabs in the menu on the left.<br><br>
                    The first section focuses on how footprints differ across sociodemographics (sex, age, urban/rural residence, education level).<br>
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