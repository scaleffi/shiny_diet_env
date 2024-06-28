# This code generates a Shiny dashboard to visualise data on diet-related environmental footprints in 2020. The data
# was created by Prof. Marco Springmann. The dashboard was designed and developed by Sebastiano Caleffi. This is my first
# dashboard, and by far the biggest and most complex project I've created using R. I'm sure there are several best
# practices that I'm not following correctly, I am working on it. 
#
# CONTEXT: Manually executing the code launches the dashboard with no issues; loading in the data by manually sourcing just the loaddata.R
# file, and then clicking on the Run App button in the menu, launches the dashboard with no issues; grouping all scripts into one
# single script and then clicking on the Run App button in the menu also launches the dashboard with no issues, without the need to load the data first.
#
# BUG: trying to launch the dashboard from here by clicking directly on the Run App button results in a Warning: Error in $: object of type 'closure' is not subsettable.
# This bug does not appear if executing the dashboard manually, or if the loaddata.R file is sourced in before
# using the Run App button. The bug makes it also impossible to deploy the dashboard online (an identical version
# is currently deployed online through my GitHub repository. The entire code behind that version is held in one single script, and that
# doesn't generate the bug, but can also be quite slow). 
# 
# WHAT I TRIED SO FAR: I tried several debugging strategies, including asking ChatGPT and more expert colleagues, to no avail.
# Every command I tried to make the error more verbose have failed, to the point that it seems R runs into the error
# before even beginning to evaluate most of the commands. I tried isolating different sections by commenting out one by one
# each block, but the error always presented itself. Sourcing the loaddata.R file manually and fixing the data in the 
# environment first solves the issue, so I tried moving the code from that script directly inside this one, instead of 
# sourcing it from outside, but that also did not work.
#
# That error message should imply that somewhere I am trying to call a function as if it was an object. The server.R file
# generates several reactive elements that are then called throughout the script, but I checked every single one of them
# several times and none of the calls seem wrong. The fact that if the script is executed manually, or the separate scripts are bundled in a single
# script, everything works fine, suggests that the issue is not caused by a function being wrongly called as an object, otherwise
# I assume I would encounter the problem each time, not only when the scripts are split for modularity.

# I tried immediately to implement these two commands, to clear the environment and get more info on what crashes,
# but they have no effect.
# clear the workspace:
# rm(list = ls())
# tell shiny to be verbose about errors
# options(shiny.error = browser)  

#Load required libraries
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(DT)
library(rsconnect)
library(shiny)
library(scales)
library(fmsb)
library(ggthemes)
library(ggsci)
library(geomtextpath)
library(plotly)

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
}

#Run the app ----

myapp <- shinyApp(ui = myUI, 
         server = myserver)

runApp(myapp
       #the "showcase" mode displays the content of the dashboard alongside the code behind it
       #,display.mode = "showcase"
       )

#shiny::runApp(display.mode="showcase")