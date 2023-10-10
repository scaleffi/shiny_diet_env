library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(tidyverse)
library(ggrepel)
library(gghighlight)
library(RColorBrewer)
library(ggthemes)
library(patchwork)
library(thematic)
library(bbplot)
library(plotly)


#clear the environment
rm(list = ls())

#################
#Load the _trs report, whith all sociodem charactestics listed in the same variable/vector, to create plots that display do not require comparing across more than one sociodem
#characteristic at a time
#################
#Report_TRS Load the csv file from work computer, and make it into a dataframe
csv_file_trs <- "/Users/lshsc40/Documents/R files/report_env_trs_053123.csv"
#personal laptop
#csv_file_trs <- "/Users/sebastianoc/Documents/R scripts/report_env_trs_053123.csv"

data_trs <- read.csv(csv_file_trs)
data_trs$value <- round(data_trs$value, 2)
df_trs <- data_trs
# Create a new dataset by adding a column labelled 'category' to the _trs dataset, to group different labels in the variable age/sociodem to subgroups (if useful)
data_trs_category <- data_trs %>%
  mutate(category = case_when(
    age %in% c("FML", "MLE", "BTH") ~ "Sex",
    age %in% c("low", "medium", "high", "all-e") ~ "Education level",
    age %in% c("rural", "urban", "all-u") ~ "Urbanisation level",
    age %in% c("0-10", "11-19", "20-39", "40-64", "65+", "all-a") ~ "Age group"
  ))
#Create another dataset by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
# namely ASF, Staples, Other, Total
data_trs_macrofoods <- data_trs_category %>%
  mutate(macrofoods = case_when(
    food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
    food_group %in% c("rice", "grains") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

df_trs_macrof <- df_trs %>%
  mutate(macrofoods = case_when(
    food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
    food_group %in% c("rice", "grains") ~ "Staples",
    food_group %in% c("fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds") ~ "Other",
    food_group %in% c("total") ~ "Total"
  ))

df_trs_category <- data_trs_category
#################
#Load the _box report, which has the 'box' variable, to create plots that display age-sex and edu-urb combinations.
#################
#Report_BOX Load the csv file on work laptop
csv_file_box <- "/Users/lshsc40/Documents/R files/report_env_box_060123.csv"
#Report_BOX Load the csv file on personal laptop
#csv_file_box <- "/Users/sebastianoc/Documents/R scripts/report_env_box_060123.csv"
#generate a dataset from the _box file as source for the dashboard
data_box <- read.csv(csv_file_box)
data_box$value <- round(data_box$value, 2)
df <- data_box


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Explore the environmental footprint of global diets", titleWidth = 450),
  
  # titlePanel("The environmental footprints of global diets"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Compare by sociodemographic", tabName = "sociodem", icon = icon("person-half-dress")),
      menuItem("Compare across regions", tabName = "food_groups", icon = icon("earth-africa")),
      menuItem("Compare by food group/category", tabName = "categories", icon = icon("wheat-awn")),
      menuItem("ReadMe", tabName = "readme", icon = icon("info-circle"), selected = TRUE)
    )
  ),
  # mainPanel(
  dashboardBody(
    tabItems(
      ###First item
      tabItem(
        tabName = "sociodem",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data",
            "These tabs show how the environmental footprints of average diets compare across different sociodemographic characteristics.\n
            The sex-age tab can help you focus on the relative differences of diet-related environmental impacts for males and females across age cohorts. The edu-urb tab instead  They display impacts as a ratio to the global average\n
            (which we've set equal to 1 in our data), allowing you to see how much more impactful the diets of urban populations in HIC are as a % of the\n
            global mean."
          )),
          # tabPanel(
          #   "Sex-age",
          #   fluidRow(
          #     column(6,
          #            selectInput("cns_prsp_2", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
          #            selectInput("measure_2", "Select Measure:", choices = c("cap-avg_WLD","cap-avg_RGS"), selected = "cap-avg_WLD"),
          #            selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "avg")
          #     ),
          #     column(6,
          #            selectInput("region_2", "Select Region:", choices = unique(df$region), selected = "WLD"),
          #            selectInput("age.education_2", "Select age group:", choices = c("0-10", "11-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-10", "11-19", "20-39", "40-64", "65+")),
          #            selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML"))
          #     )
          #   ),
          #   plotOutput("plot_sexage")
          # ),
          tabPanel(
            "Sex-age",
            fluidRow(
              box(
                width = 3, collapsible = T, title = "Select parameters", solidHeader = TRUE, status = "info",
                     selectInput("cns_prsp_2", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_2", "Select Measure:", choices = c("cap-avg_WLD","cap-avg_RGS"), selected = "cap-avg_WLD"),
                     selectInput("env_dimensions_2", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "avg"),
                     selectInput("region_2", "Select Region:", choices = unique(df$region), selected = "WLD"),
                     selectInput("age.education_2", "Select age group:", choices = c("0-10", "11-19", "20-39", "40-64", "65+", "all-a"), multiple = TRUE, selected = c("0-10", "11-19", "20-39", "40-64", "65+")),
                     selectInput("sex.urbanisation_2", "Select sex:", choices = c("MLE", "FML", "BTH"), multiple = TRUE, selected = c("MLE", "FML")),
                     #downloadButton("sexage_download_plot", "Download plot", class = "butt1")
              )
            ,
            box(
              width = 9, collapsible = T, solidHeader = TRUE, status = "primary",
            plotOutput("plot_sexage", height = 400)
          ))),
          tabPanel(
            "Edu-urb",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_3", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_3", "Select Measure:", choices = c("cap-avg_WLD","cap-avg_RGS"), selected = "cap-avg_WLD"),
                     selectInput("env_dimensions_3", "Select Environmental Dimensions:", choices = unique(df$env_itm), selected = "avg")
              ),
              column(6,
                     selectInput("region_3", "Select Region:", choices = unique(df$region)),
                     selectInput("age.education_3", "Select education level:", choices = c("low", "medium", "high"), multiple = TRUE, selected = c("low", "medium", "high")),
                     selectInput("sex.urbanisation_3", "Select urbanisation level:", choices = c("urban", "rural"), multiple = TRUE, selected = c("urban", "rural"))
              )
            ),
            plotOutput("plot_eduurb")
          ),
          tabPanel(
            "Sociodem",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_7", "Select Consumption Perspective:", choices = unique(df_trs_category$cns_prsp), selected = "avb"),
                     selectInput("measure_7", "Select Measure:", choices = c("abs","cap"), selected = "cap"),
                     selectInput("env_dimensions_7", "Select Environmental Dimensions:", choices = setdiff(unique(df_trs_category$env_itm), "avg"), selected = "GHG")
              ),
              column(6,
                     selectInput("region_7", "Select Region:", choices = unique(df_trs_category$region), selected = "WLD"),
                     selectInput("food_group_7", "Select Food Group:", choices = unique(df_trs_category$food_group), multiple = TRUE, selected = "total"),
                     selectInput("age_7", "Select sociodemographic:", choices = setdiff(unique(df_trs_category$age), c("all-a", "all-e", "BTH", "all-u")), multiple = TRUE, selected = c("low", "medium", "high"))
              )
            ),
            plotOutput("plot_sociodem")
          )
        )
      ),
      tabItem(
        ###Second item
        tabName = "food_groups",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data",
            "These tabs display the environmental footprints of diets across the world, by food group, in absolute and per capita terms"
          )),
          tabPanel(
            "Impacts by income region",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_1", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_1", "Select Measure:", choices = c("abs", "cap"), selected = "cap"),
                     selectInput("env_dimensions_1", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"), selected = "GHG")
              ),
              column(6,
                     selectInput("food_group_1", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = "total"),
                     selectInput("region_1", "Select Region:", choices = c("LIC", "LMC", "UMC", "HIC"), multiple = TRUE, selected = c("LIC", "LMC", "UMC", "HIC"))
              )
            ),
            plotOutput("plot_region")
          ),
          tabPanel(
            "Impacts by geographical region",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_5", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_5", "Select Measure:", choices = c("abs", "cap"), selected = "cap"),
                     selectInput("env_dimensions_5", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"), selected = "GHG")
              ),
              column(6,
                     selectInput("food_group_5", "Select Food Group:", choices = unique(df$food_group), multiple = TRUE, selected = c("beef", "milk", "rice", "roots")),
                     selectInput("region_5", "Select Region:", choices = c("NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF"), multiple = TRUE, selected = c("NAC", "SAS", "SSF"))
              )
            ),
            plotOutput("plot_regiongeo")
          ),
          
        )
      ),
      tabItem(
        ###Third item
        tabName = "categories",
        tabsetPanel(
          tabPanel("About", box(
            title = "About this data",
            "These tabs display the environmental footprints of diets across the world, as a ratio of the global average."
          )),
          tabPanel(
            "Impacts by food group",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_4", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_4", "Select Measure:", choices = c("abs", "cap"), selected = "abs"),
                     selectInput("env_dimensions_4", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), "avg"), selected = "GHG")
              ),
              column(6,
                     selectInput("food_group_4", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef", "lamb", "rice", "grains", "fruit_veg", "legumes")),
                     selectInput("region_4", "Select Region:", choices = unique(df$region), selected = "WLD")
              )
            ),
            plotOutput("plot_category")
          ),
          tabPanel(
            "Impacts by food category",
            fluidRow(
              column(6,
                     selectInput("cns_prsp_6", "Select Consumption Perspective:", choices = unique(df$cns_prsp), selected = "avb"),
                     selectInput("measure_6", "Select Measure:", choices = c("abs", "cap"), selected = "abs"),
                     selectInput("env_dimensions_6", "Select Environmental Dimensions:", choices = setdiff(unique(df$env_itm), c("avg", "land_pstr", "land_crop")), selected = "GHG")
              ),
              column(6,
                     #selectInput("food_group_6", "Select Food Group:", choices = setdiff(unique(df$food_group), "total"), multiple = TRUE, selected = c("beef", "pork", "milk", "legumes", "roots", "rice", "grains")),
                     #selectInput("category_6", "Select Food Category:", choices = setdiff(unique(df_trs_macrof$macrofoods), "Total"), multiple = TRUE),
                     selectInput("region_6", "Select Region:", choices = unique(df$region), selected = "WLD")
              )
            ),
            plotOutput("plot_categorymacro")
          )
          )
        ),
      tabItem(
        ###Fourth item
        tabName = "readme",
        box(
          title = "ReadMe",
          "This dashboard offers users the chance to explore data on the environmental footprints generated by global diets. All data refers to the year 2018 (CHECK!)
              and is built on the most comprehensive estimates of daily intakes and average footprints by food group currently available. Etc Etc Etc"
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  ###########
  #Create filters for tabs in the first menu item, 'compare by sociodemographic'
  ###########
  filtered_data_sexage <- reactive({
    df %>%
      filter(measure == input$measure_2,
             env_itm %in% input$env_dimensions_2,
             food_group == "total",
             box == "age-sex",
             age.education %in% input$age.education_2,
             sex.urbanisation %in% input$sex.urbanisation_2,
             cns_prsp == input$cns_prsp_2,
             region == input$region_2)
  })
  
  filtered_data_eduurb <- reactive({
    df %>%
      filter(measure == input$measure_3,
             env_itm %in% input$env_dimensions_3,
             food_group == "total",
             box == "edu-urb",
             age.education %in% input$age.education_3,
             sex.urbanisation %in% input$sex.urbanisation_3,
             cns_prsp == input$cns_prsp_3,
             region == input$region_3)
  })
  
  filtered_data_sociodem <- reactive({
    df_trs_category %>%
      filter(measure == input$measure_7,
             env_itm %in% input$env_dimensions_7,
             food_group %in% input$food_group_7,
             age %in% input$age_7,
             cns_prsp == input$cns_prsp_7,
             region == input$region_7)
  })
  
  #############
  #Create filters for tabs in the second menu item, 'compare by food groups'
  #############
  filtered_data_region <- reactive({
    df %>%
      filter(measure == input$measure_1,
             env_itm %in% input$env_dimensions_1,
             food_group %in% input$food_group_1,
             box == "age-sex",
             age.education == "all-a",
             sex.urbanisation == "BTH",
             cns_prsp == input$cns_prsp_1,
             region %in% input$region_1)
  })
  
  filtered_data_regiongeo <- reactive({
    df %>%
      filter(measure == input$measure_5,
             env_itm %in% input$env_dimensions_5,
             food_group %in% input$food_group_5,
             box == "age-sex",
             age.education == "all-a",
             sex.urbanisation == "BTH",
             cns_prsp == input$cns_prsp_5,
             region %in% input$region_5)
  })
  
  #############
  #Create filters for tabs in the third menu item, 'compare by categories'
  #############
  filtered_data_category <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_4,
             env_itm %in% input$env_dimensions_4,
             food_group %in% input$food_group_4,
             age == "all-a",
             cns_prsp == input$cns_prsp_4,
             region %in% input$region_4,
             macrofoods %in% c("ASF", "Staples", "Other"))
  })
  
  filtered_data_categorymacro <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_6,
             env_itm %in% input$env_dimensions_6,
             food_group != "total",
             age == "all-a",
             cns_prsp == input$cns_prsp_6,
             region %in% input$region_6,
             macrofoods %in% c("ASF", "Staples", "Other"))
  })
  
  ##################
  #Draw the plots
  ##################
  
  #Prepare objects that will be used to create the plots below.
  
  # #Control shading by setting alpha values through a new vector named alpha_vals; changing the range of shading helps with displaying some images that have several colors.
  # alpha_max <- 1
  # alpha_min <- 0.7
  # alpha_vals <- c(
  #   seq(alpha_max, alpha_min, length.out = 8), 
  #   seq(alpha_min, alpha_max, length.out = 8)[-1]
  # )
  # alpha_vals
  # 
  
  
  # #create a vector named 'colors' made up of three colors from the Set1 ColorBrewer palette. This can be used to assign colors to the macrofoods variable
  colors <- c("#E41A1C", "#377EB8", "#FFFF33")
  
  ###########
  #Draw plots for tabs in the first item
  ###########
  output$plot_sexage <- renderPlot({
    data <- filtered_data_sexage()
    ggplot(data, aes(x = age.education, y = value, color = sex.urbanisation, shape = sex.urbanisation)) +
      geom_point(size=3) +
      scale_color_brewer(palette = "Set1", name = "Sex:", labels = c("Female", "Male")) +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      geom_hline(yintercept = 1, alpha = 0.3) +
      guides(shape = "none") +
      theme_linedraw() +
      labs(x = "Age group", y = "avg env impact as ratio \nto WLD average (1 = world average)", color = "", shape = NULL) + 
      theme(plot.title=element_text(hjust = 0.5, size = 20), axis.title.x = element_text(face = "bold"), strip.text = element_text(size=12), legend.position = "top", legend.text = element_text(size = 12), axis.text.x = element_text(face = "bold"))
  })
  
  output$plot_eduurb <- renderPlot({
    data <- filtered_data_eduurb()
    ggplot(data, aes(x = factor(age.education, level=c("low", "medium", "high")), y = value, color = sex.urbanisation, shape = sex.urbanisation)) +
      geom_point(size=3) +
      scale_color_brewer(palette = "Dark2", name="Urbanisation:", labels=c("Rural", "Urban")) +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      geom_hline(yintercept = 1, alpha = 0.3) +
      guides(shape = "none") +
      labs(x = "Education level", y = "ratio of avg env impact to WLD average (1 = world average)", color = "Urbanisation level") + 
      theme_linedraw() + 
      theme(plot.title=element_text(hjust = 0.5, size = 20), axis.title.x = element_text(face = "bold"), strip.text = element_text(size=12), legend.position = "top", legend.text = element_text(size = 15), axis.text.x = element_text(face = "bold"))
  })
  
  output$plot_sociodem <- renderPlot({
    data <- filtered_data_sociodem()
    ggplot(data, aes(x = factor(age, level=c("MLE", "FML", "0-10", "11-19", "20-39", "40-64", "65+", "low", "medium", "high", "urban", "rural")), y = value, label = value)) +
                     #fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
      geom_col(aes(fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total"))), color = "white") +
      theme_linedraw() +
      labs(x = NULL, y = "Impact ", fill = "Food group")
      
      
  })
  
  ###########
  #Draw plots for tabs in the second item
  ###########
  output$plot_region <- renderPlot({
    data <- filtered_data_region()
    ggplot(data, aes(x = factor(region, level=c("LIC", "LMC", "UMC", "HIC")), y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total")))) +
      geom_col(color = "white", width = 0.6) +
      labs(x = NULL ,y = "Impact",
           fill = "Food group") +
      theme_linedraw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$plot_regiongeo <- renderPlot({
    data <- filtered_data_regiongeo()
    ggplot(data, aes(x = region, y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds", "total")))) +
      geom_col(color = "white", width = 0.6) +
      labs(x = NULL ,y = "Impact",
           fill = "Food group") +
      theme_linedraw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  ###########
  #Draw plots for tabs in the third item
  ###########
  output$plot_category <- renderPlot({
    data <- filtered_data_category()
    ggplot(data, aes(x = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")), y = value, fill = macrofoods)) +
      geom_col(color = "white", width = 0.6) +
      #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
      scale_fill_manual(values = colors) +
      labs(x = NULL, y = "Impact", fill = "Category:") +
      theme_linedraw() +
      theme(axis.text.x = element_text(face="bold"), axis.title.y = element_text(size = 12), strip.text.x = element_text(size = 12), legend.position = "right")
    
})
  
  output$plot_categorymacro <- renderPlot({
    data <- filtered_data_categorymacro()
    ggplot(data, aes(x = factor(macrofoods, level=c("ASF", "Staples", "Other")), y = value, fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
      geom_col(color = "white", width = 0.6) +
      #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
      #scale_fill_manual(values = colors) +
      labs(x = NULL, y = "Impact", fill = "Food group:") +
      theme_linedraw() +
      theme(axis.text.x = element_text(face="bold"), axis.title.y = element_text(size = 12), strip.text.x = element_text(size = 12), legend.position = "right")
    
  })
  
  # output$sexage_download_plot <- downloadHandler(
  #   filename = function() { paste("plot_sexage", '.png', sep='') },
  #   content = function(file) { ggsave(file, plot = output$plot_sexage(), device = "png", width = 10) } #  this wont work because it is a ggsave but the plot is plotly 
  # )
  
  # output$sexage_download_plot <- downloadHandler(
  #   filename = function() { paste("plot_sexage", '.png', sep='') },
  #   content = function(file) {
  #     ggsave(file, plot = output$plot_sexage, device = "png", width = 10, height = 6)
  #   }
  # )
  # 
  
  
  
}
# Run the Shiny app
shinyApp(ui = ui, server = server)
