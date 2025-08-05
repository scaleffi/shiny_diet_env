tabItem(
  ###Fifth item ----
  tabName = "readme",
  fluidRow(
    box(width = 12,
        title = "Info",
        div(
          HTML(
            "This dashboard allows users to explore data on food-related (food intake + food waste) environmental footprints in 2020, for 175 countries, across four socio-demographic characteristics (age; sex; education level; urban/rural residency),
            and five environmental dimensions (GHG; land use; freshwater use; eutrophication potential; average environmental impact, which is a weighted average of impacts across the other four dimensions). It uses new estimates developed by Prof. Marco Springmann, and has been designed and developed by Sebastiano Caleffi.<br><br>
            
            The dashboard accompanies a scientific paper that has been submitted for review, and which contains detailed information on the methodology and data used. The paper will be linked to the dashboard once it has been published.
            The dashboard has been made publically available online with the support of Oxford University. For any questions or comments, please contact Sebastiano Caleffi at s.caleffi@ucl.ac.uk<br><br> 

            You can filter and compare food-related environmental footprints by region, sex, age group, urban/rural residence, and education level, navigating through the main three sections through the tabs in the menu on the left.<br>
            The first section focuses on how footprints differ across sociodemographic groups (sex, age, urban/rural residence, education level).<br>
            The second, on how they differ across regions, both income and geographic (we are working on incorporating country-level data in the dashboard).<br> 
            The third, on how they differ across food groups and macrocategories.<br><br> 
                    
            Within each section, you can build plots and data tables by choosing among several filters and inputs. 
            All data can be visualised and downloaded as both a plot (.png) or a table (.csv). Plots and tables are reproducible, as long as their source is clearly cited.<br><br>
            Estimates of food demand (food intake + food waste) are built from FAOSTAT's Food Balance Sheets, available at: https://www.fao.org/faostat/en/#data/FBS. Estimates of daily intakes by sociodemographic are taken from the GDD, available at: https://globaldietarydatabase.org/.
            Region- and food-specific environmental footprints are taken from Poore and Nemecek (2018), available at: https://doi.org/10.1126/science.aaq0216.<br> 
            
            Detailed information on the data and methods behind the dashboard will be made available once the underlying paper (currently in review) has been published.
            For feedback or questions please contact Sebastiano Caleffi at s.caleffi@ucl.ac.uk"
          )
          #style = "width:100%;"
          
        )
    )
  )
)