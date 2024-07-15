#Load required libraries
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(DT)
library(rsconnect)
library(shiny)
library(scales)
# library(fmsb)
library(ggthemes)
library(ggsci)
library(geomtextpath)
# library(plotly)
library(bslib)

#Load in the data and apply all needed transformation
source('loaddata.R', local = TRUE)
#Load in visualisation elements, a theme, and labels for plotting
source('visualisation.R', local = TRUE)

#Generate the UI object
source('ui.R', local = TRUE)

#Apply the reactive server function that takes care of the dynamic behaviour of the dashboard
#source('server.R', local = TRUE)
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

#Run the app ----

myapp <- shinyApp(ui = myUI, 
         server = myserver)

# runApp(myapp
#        #the "showcase" mode displays the content of the dashboard alongside the code behind it
#        #,display.mode = "showcase"
#        )

#shiny::runApp(display.mode="showcase")