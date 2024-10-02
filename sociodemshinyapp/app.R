# Header ------------------------------------------------------------------
# Author: Sebastiano Caleffi (github: scaleffi)

# Load libraries ----------------------------------------------------------
library(shinydashboard)
library(tidyverse)
library(ggrepel)# to plot non-overlapping labels
library(DT)
library(rsconnect) # to connect to the posit account on which the dashboard is deployed
library(shiny)
library(scales)
library(ggthemes)
library(ggsci)
library(geomtextpath)
library(bslib)


# Load and prepare data ---------------------------------------------------
source('loaddata.R', local = TRUE)

# Load in elements for visualisations -------------------------------------
source('visualisation.R', local = TRUE)

# Create UI object, defining what the user will see -----------------------
source('ui.R', local = TRUE)

# Create server function, which drives all reactive behaviours ------------
myserver <- function(input,output) {
  source('server_files/server_sexage.R', local = TRUE)
  source('server_files/server_eduurb.R', local = TRUE)
  source('server_files/server_multisociodem.R', local = TRUE)
  source('server_files/server_multisociodemrel.R', local = TRUE)
  source('server_files/server_all_sociodem.R', local = TRUE)
  source('server_files/server_region.R', local = TRUE)
  source('server_files/server_regiongeo.R', local = TRUE)
  source('server_files/server_category.R', local = TRUE)
  source('server_files/server_categorymacro.R', local = TRUE)
  source('server_files/server_radar_region.R', local = TRUE)
  source('server_files/server_radar_regiongeo.R', local = TRUE)
  source('server_files/server_radar_regionincome.R', local = TRUE)
}

# Execute and launch the app ----------------------------------------------
sociodemapp <- shinyApp(ui = myUI, 
         server = myserver)

# Execute and launch the app in showcase mode -----------------------------
# runApp(myapp
#        #the "showcase" mode displays the content of the dashboard alongside the code behind it
#        #,display.mode = "showcase"
#        )
#shiny::runApp(display.mode="showcase")