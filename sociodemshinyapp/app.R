#=============================
# app.R file, building and deploying the sociodem shiny app
# author: Sebastiano Caleffi (github: scaleffi)
#=============================

# Load libraries ----
library(shinydashboard)
library(tidyverse)
#ggrepel is used in the sexage and eduurb tabs to visualise a scatterplot (geom_point)
#without generating excessive overlap between the data labels.
library(ggrepel)
library(DT)
#rsconnect is used to establish the connection with my online shiny account, where the app is published.
library(rsconnect)
library(shiny)
library(scales)
# library(fmsb)
library(ggthemes)
library(ggsci)
library(geomtextpath)
# library(plotly)
library(bslib)

# Load in the data and apply all needed transformations ----
source('loaddata.R', local = TRUE)

# Load in visualisation elements, a theme, and labels for plotting ----
source('visualisation.R', local = TRUE)

# Generate the UI object; the ui.R file establishes the overall structure and appearance of the dashboard ----
source('ui.R', local = TRUE)

# Create the reactive server function that takes care of the dynamic behaviour of the dashboard ----
myserver <- function(input,output) {
  source('server_sexage.R', local = TRUE)
  source('server_eduurb.R', local = TRUE)
  source('server_multisociodem.R', local = TRUE)
  source('server_multisociodemrel.R', local = TRUE)
  source('server_all_sociodem.R', local = TRUE)
  source('server_region.R', local = TRUE)
  source('server_regiongeo.R', local = TRUE)
  source('server_category.R', local = TRUE)
  source('server_categorymacro.R', local = TRUE)
  source('server_radar_region.R', local = TRUE)
  source('server_radar_regiongeo.R', local = TRUE)
  source('server_radar_regionincome.R', local = TRUE)
}

# Run the app ----

sociodemapp <- shinyApp(ui = myUI, 
         server = myserver)

# runApp(myapp
#        #the "showcase" mode displays the content of the dashboard alongside the code behind it
#        #,display.mode = "showcase"
#        )

#shiny::runApp(display.mode="showcase")