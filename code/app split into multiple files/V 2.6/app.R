#This file replicates the code in the Dashboard 2.6.R file, but split into four separate scripts. This one includes all the instructions to launch the dashboard. It only requires loading in the correct data files, but 
#it does not run with the Run App button, you have to launch it manually. Or, you can load in the loaddata.R file, and then use the Run App button

#----
#Load the required libraries. 
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(DT)
library(rsconnect)
library(shiny)

#Prepare the environment for executing the dashboard ----
#rm(list = ls()) #clear the environment

#options(shiny.error = browser) # for debugging - remove in last version

##Source in the files that prepare the dashboard ----
  #This loads in the dataset used for the dashboard
  source('/Users/lshsc40/Documents/GitHub/shiny_diet_env/code/app split into multiple files/V 2.6/loaddata.R', local = FALSE)

  #Load in the User interface file. This creates an object, called a ui label. The file needs to be in the same folder as this app.R file, otherwise the right path needs to 
  #be set to load the file in correctly.
  source('/Users/lshsc40/Documents/GitHub/shiny_diet_env/code/app split into multiple files/V 2.6/ui.R', local = FALSE)
  
  #Load in the server file. This creates a function, called server The file needs to be in the same folder as this app.R file, otherwise the right path needs to 
  #be set to load the file in correctly.
  source('/Users/lshsc40/Documents/GitHub/shiny_diet_env/code/app split into multiple files/V 2.6/server.R', local = FALSE)

#######-------------------#########-------------------------###########
#Start the dashboard. A dashboard in Shiny is composed of a UI, a Server Function, and a command to run the app.


#Notes on Shiny and dashboards. Shiny allows the user to create an app, or dashboard, to quickly visualise datasets. When using the app, and changing the data selections,
#the app cycles through both the UI and the Server function in order to display the data. The UI tells R how the dashboard should look like and what
#options the user should have. The Server function starts from the original data sources to build and visualise the data selection that results
#from the user's choices. Both UI and Server function are called on when running the app, so errors must be looked for in both. 


###----
#Run the Shiny app.
###

#shinyApp(ui, server)