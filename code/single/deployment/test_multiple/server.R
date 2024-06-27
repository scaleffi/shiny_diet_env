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
# library(plotly)

server <- function(input, output, session) {
  
  #Create filters----
  #NOTE: The filters need to be coherent with the options for data selection given to users in the UI using the command 'selectInput'. If the user is
  #given the freedom to choose specific inputs in the UI, but then the corresponding filters are not set appropriately in the Server, 
  #the app may work but it won't display any plots, or the plots may be empty. So if plots are not showing or not showing correctly, first ensure that
  #the inputs available to the user in the UI correctly match the corresponding filters in the Server function. Users should be given
  #the option to only choose inputs that are included in the subset created for that tab through the filters.
  
  
  
  ##Create filters for tabs in the first menu item, 'compare by sociodemographic'.----

  #Note on filter notation:
  #The notation == means that for the variable x, the app will take as input the specific value selected for that variable by the user through the UI interface. It implies that the user can select only one value at a time for that variable.
  #The notation %in% means that for the variable x, the app will take as input any values selected for that variable by the user through the UI interface. It implies that the user can select multiple values within that variable.

  filtered_data_sexage <- reactive({
    # For debugging purposes: Print out the values of the reactive function to check that they are as expected
    # print(input$dmd_scn_2)
    # print(input$measure_2)
    # print(input$env_dimensions_2)
    # print(input$region_2)
    # print(input$age.education_2)
    # print(input$sex.urbanisation_2)

    df %>%
      filter(measure == input$measure_2,
             env_itm %in% input$env_dimensions_2,
             food_group == "total",
             box == "age-sex",
             age.education %in% input$age.education_2,
             sex.urbanisation %in% input$sex.urbanisation_2,
             dmd_scn == input$dmd_scn_2,
             region %in% input$region_2)
  })

  filtered_data_eduurb <- reactive({
    df %>%
      filter(measure == input$measure_3,
             env_itm %in% input$env_dimensions_3,
             food_group == "total",
             box == "edu-urb",
             age.education %in% input$age.education_3,
             sex.urbanisation %in% input$sex.urbanisation_3,
             dmd_scn == input$dmd_scn_3,
             region %in% input$region_3)
  })

  filtered_data_sociodem <- reactive({
    df_trs_category %>%
      filter(measure == input$measure_7,
             env_itm %in% input$env_dimensions_7,
             food_group %in% input$food_group_7,
             age %in% input$age_7,
             dmd_scn == input$dmd_scn_7,
             region %in% input$region_7)
  })

  filtered_data_sociodem_rel <- reactive({
    df_trs_category %>%
      filter(measure == input$measure_8,
             env_itm %in% input$env_dimensions_8,
             food_group %in% input$food_group_8,
             #food_group == "total",
             #category %in% input$category_8,
             dmd_scn == input$dmd_scn_8,
             region %in% input$region_8,
             age %in% input$age_8
      )
  })

  filtered_data_all_sociodem <- reactive({
    # print(input$measure_9)
    # print(input$env_dimensions_9)
    # print(input$dmd_scn_9)
    # print(input$region_9)
    # print(input$age_9)
    # print(input$urban_9)
    # print(input$education_9)
    # print(input$sex_9)

    df_sel %>%
      filter(measure == input$measure_9,
             env_itm %in% input$env_dimensions_9,
             #food_group == "total",
             #food_group == "total",
             #category %in% input$category_8,
             dmd_scn == input$dmd_scn_9,
             region %in% input$region_9,
             age %in% input$age_9,
             urban %in% input$urbanisation_9,
             edu %in% input$education_9,
             sex %in% input$sex_9
      )
  })

  ##Create filters for tabs in the second menu item, 'compare by region'----

  filtered_data_region <- reactive({
    df %>%
      filter(measure == input$measure_1,
             env_itm %in% input$env_dimensions_1,
             food_group %in% input$food_group_1,
             box == "age-sex",
             age.education == "all-a",
             sex.urbanisation == "BTH",
             dmd_scn == input$dmd_scn_1,
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
             dmd_scn == input$dmd_scn_5,
             region %in% input$region_5)
  })


  ##Create filters for tabs in the third menu item, 'compare by categories'----

  filtered_data_category <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_4,
             env_itm %in% input$env_dimensions_4,
             food_group %in% input$food_group_4,
             age == "all-a",
             dmd_scn == input$dmd_scn_4,
             region %in% input$region_4
             #macrofoods %in% c("ASF", "Staples", "Other")
      )
  })

  filtered_data_categorymacro <- reactive({
    df_trs_macrof %>%
      filter(measure == input$measure_6,
             env_itm %in% input$env_dimensions_6,
             food_group %in% input$food_group_6,
             age == "all-a",
             dmd_scn == input$dmd_scn_6,
             region %in% input$region_6
             #macrofoods %in% c("ASF", "Staples", "Other")
      )
  })

  ##Create filters for tabs in the fourth menu item, 'compare by region (radar)'----

  filtered_data_regionradar <- reactive({
    #df %>%

    df_trs_category %>%
      filter(measure == input$measure_10,
             env_itm %in% input$env_dimensions_10,
             food_group == "total",
             age %in% input$age_10,
             dmd_scn == input$dmd_scn_10,
             region %in% c("WLD", "HIC", "UMC", "LMC", "LIC")
             #macrofoods %in% c("ASF", "Staples", "Other")
      )

  })

  filtered_data_regionradargeo <- reactive({
    #df %>%

    df_trs_category %>%
      filter(measure == input$measure_11,
             env_itm %in% input$env_dimensions_11,
             food_group == "total",
             age == "all-a",
             dmd_scn == input$dmd_scn_11,
             region %in% input$region_11
             #macrofoods %in% c("ASF", "Staples", "Other")
      )

  })

  
  #Prepare graphic objects and labels that will be used to create the plots below ----
  
  #Create custom theme as a function, which can be applied to each plot without having to manually edit each of them to
  #obtain the required look
  
  lshtm_theme_few <- function(){
    theme_few() +
      #%+replace%
      theme(
        #plot.margin = margin(0,0,0,0),
        axis.title.x = element_text(
          vjust = -0.5,
          size = 16,
          #family = "serif",
          face = "bold"),
        axis.title.y = element_text(size = 16,
                                    face = "bold",
                                    #family = "serif",
                                    #angle = 90
                                    vjust = 1.5
        ),
        strip.text.x = element_text(size = 14 
                                    #,face = "bold"
        ),
        panel.spacing = unit(0,"lines"),
        strip.text.y = element_text(size = 14
                                    , face = "bold"
        ),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   #vjust = 0.5,
                                   hjust = 1
        ),
        legend.position = "right",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12
                                    #face = "bold"
        ),
        plot.title = element_text(#family = "serif",
          size = 22, 
          face = "bold",
          hjust = 0.5,
          vjust = 1),
        plot.subtitle = element_text(size = 14, face = "bold"),
        panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
        panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
        strip.placement = "inside"
      )
  }
  
  #Create a second version of the custom theme, for radar plots. This type of plots require some changes that would be 
  #too time consuming to manually add to each plot
  lshtm_theme_few_radar <- function(){
    theme_few() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"),
        panel.spacing = unit(0,"lines"),
        strip.text.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.placement = "outside"
      )
  }
  
  #create a vector named 'colors_macro' made up of three colors from the Set1 ColorBrewer palette. This can be used to assign specific colors to values in the macrofoods variable.
  colors_macro <- c(
    "ASF" = "#922b21",
    "Staples" = "#f1c40f",
    "Other" = "#85929e"
  )
  
  #create vectors to assign colors to the sociodem characteristics
  colors_sex <- c(
    "FML" = "#E41A1C",
    "MLE" = "#377EB8",
    "BTH" = "#4DAF4A"
  )
  
  colors_urban <- c(
    "urban" = "#D95F02",
    "rural" = "#66A61E",
    "all-u" = "#f4d03f"
  )
  
  colors_sociodem <- c(
    "urban" = "#D95F02",
    "rural" = "#66A61E",
    "FML" = "#E41A1C",
    "MLE" = "#377EB8",
    "low" = "#fcf3cf",
    "medium" = "#f39c12",
    "high" = "#873600",
    "0-9" = "#ebdef0",
    "10-19" = "#a9cce3",
    "20-39" = "#48c9b0",
    "40-64" = "#229954",
    "65+"= "#7d6608"
  )
  
  colors_sociodem_category <- c(
    "Urb. level" = "#f4d03f",
    "Sex" = "#4DAF4A",
    "Edu. level" = "#f39c12",
    "Age" = "#48c9b0"
  )
  
  #Create a vector with specific color assigned to each food group
  #Based on _trs_110423
  colors_food <- c(
    "total" = "#a6a79b",
    "rice" = "#f9e79f",
    "roots" = "#eb984e",
    "sugar" = "#fad7a0",
    "legumes" = "#6e2c00",
    "beef" = "#cb4335",
    "lamb" = "#ec7063",
    "othr_meat" = "#d98880",
    "pork" = "#f5a5b5",
    "othr_ani" = "#fae5d3",
    "other" = "#fdedec",
    "dairy" = "#f0ebe2",
    "fish" = "#8fbad3",
    "grains" = "#ecdb54",
    "fruit_veg" = "#229954",
    "nuts_seeds" = "#7d6608",
    "oils" = "#abebc6")
  
  # Create a vector to rename facet plots with the full names of the environmental dimensions
  env_itm.labs <- c("GHG (Mt CO2eq)", "Freshwater use (km3)", "Eutrophication pot. (Mt PO4eq)", "land use (thousands of km2)", "Land use_pasture (thousands of km2)", "Land use_crops (thousands of km2)")
  names(env_itm.labs) <- c("GHG (Mt CO2eq)", "water use (km3)", "eutrophication pot. (kt PO4eq)", "land use (thousands of km2)", "land use, pasture (thousands of km2)", "land use, crops (thousands of km2)")
  
  # Create a vector to rename facet plots with the full names of the regions
  region.labs <-
    c(
      "World",
      "Low Income",
      "Lower Middle Income",
      "Upper Middle Income",
      "High Income",
      "East Asia and Pacific",
      "Europe & C. Asia",
      "Latin America & Caribbean",
      "Middle East and North Africa",
      "North America",
      "South Asia",
      "Sub-Saharan Africa"
    )
  names(region.labs) <-
    c("WLD",
      "LIC",
      "LMC",
      "UMC",
      "HIC",
      "EAS",
      "ECS",
      "LCN",
      "MEA",
      "NAC",
      "SAS",
      "SSF"
    )
  
  custom_order_region <-
    c("WLD",
      "HIC",
      "UMC",
      "LMC",
      "LIC",
      "ECS",
      "MEA",
      "EAS",
      "SAS",
      "NAC",
      "LCN",
      "SSF")
  
  custom_labels_region <-
    c(
      "World",
      "High Income",
      "Upper Middle Income",
      "Lower Middle Income",
      "Low Income",
      "Europe & C. Asia",
      "Middle East & N. Africa",
      "E. Asia & Pacific",
      "South Asia",
      "North America",
      "Latin Am. & Caribbean",
      "Sub-Saharan Africa"
    )
  
  ##Draw plots for tabs in the first item (sociodem) ----

  #The dashboard works if the plots are directly called using the renderPlot call, which means that R generates a new plot dynamically
  #each time the inputs change. HOWEVER, this means the plot object is never stored anywhere in R's working memory/environment. This makes
  #it impossible to download the plot, because R doesn't accept the result of a renderPlot call as the input to create a file for download.
  #I suspect this creates other issues as well, so in keeping with best practice I've seen online, since 16/11/2023 I've changed the code
  #so that the plots are first created as a reactive object and temporarily stored in R's working environment, AND ONLY THEN they are
  #rendered using the renderPlot call. This makes it possible to download the plots (an issue I have been trying to fix for weeks, if not
  #months), and likely streamlines the execution of the code as well.
  reactive_plot_sexage <- reactive({
    data <- filtered_data_sexage()

    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    selected_measure <- input$measure_2
    selected_env_itm_s <- input$env_dimensions_2
    selected_dmd_scn <- input$dmd_scn_2

    p_sexage <- ggplot(
      data,
      aes(
        x = age.education,
        y = value,
        color = sex.urbanisation
      )
    ) +
      geom_point(size = 4) +
      #coord_flip() +
      scale_color_manual(values = colors_sex,
                         breaks = c("MLE",
                                    "FML",
                                    "BTH"),
                         labels = c("Male",
                                    "Female",
                                    "Both"),
                         name = "(100 = global mean)") +
      scale_y_continuous(breaks = c(0, 50, 75, 100, 125, 150, 200)) +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom, ncol = 5,
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
      ) +
      geom_hline(yintercept = 100, alpha = 0.2, linewidth = 2) +
      #geom_texthline(mapping = NULL, data = NULL, yintercept = 100, label = "Global average") +
      labs(
        title = paste("Diet-related",
                      selected_env_itm_s,
                      "in 2020,",
                      "as\n",
                      selected_measure,
                      #"(100 = world or regional average)",
                      sep = " "),
        # # subtitle = paste("Note: all data is based on ",
        # #                   selected_dmd_scn,
        # #                  ".",
        #                  #".\nIn the plot, average is set equal to 100.",
        #                  sep = "") ,
        #caption = "LSHTM - Centre for Climate Change and Planetary Health",
        x = "Age",
        y = paste(selected_env_itm_s,
                  " as\n",
                  selected_measure,
                  #", with average set to 100",
                  sep = "")
      ) +
      lshtm_theme_few()+
      theme(legend.position = "top")
  })

  output$plot_sexage <- renderPlot({
    print(reactive_plot_sexage())
  })

  reactive_plot_eduurb <- reactive({
    data <- filtered_data_eduurb()

    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    selected_measure <- input$measure_3
    selected_env_itm_u <- input$env_dimensions_3
    selected_dmd_scn <- input$dmd_scn_3

    p_eduurb <- ggplot(data,
                       aes(
                         x = factor(age.education, level = c("all-e" ,"low", "medium", "high")),
                         y = value,
                         color = sex.urbanisation
                       )) +
      geom_point(size = 4) +
      scale_color_manual(values = colors_urban,
                         breaks = c("urban",
                                    "rural",
                                    "all-u"
                         ),
                         labels = c("Urban",
                                    "Rural",
                                    "all-u"
                         ),
                         name = "(100 = global mean)") +
      scale_y_continuous(breaks = c(0, 50, 75, 100, 125, 150, 175, 200)) +
      geom_text_repel(aes(label = value), show.legend = FALSE) +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom, ncol = 5,
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
      ) +
      geom_hline(yintercept = 100, alpha = 0.2, linewidth = 2) +
      labs(
        title = paste("Diet-related",
                      selected_env_itm_u,
                      "in 2020,",
                      "as\n",
                      selected_measure,
                      #"(100 = world or regional average)",
                      sep = " ") ,
        # subtitle = paste("Note: all data is based on ",
        #                  selected_dmd_scn,
        #                  ".",
        #                  #".\nIn the plot, average is set equal to 100.",
        #                  sep = "") ,
        #caption = "LSHTM - Centre for Climate Change and Planetary Health",
        x = "Education level",
        y = paste(selected_env_itm_u,
                  " as\n",
                  selected_measure,
                  #", with average set to 100",
                  sep = "")
      ) +
      lshtm_theme_few()+
      theme(legend.position = "top")
  })

  output$plot_eduurb <- renderPlot({
    print(reactive_plot_eduurb())
  })


  reactive_plot_sociodem <- reactive({
    data <- filtered_data_sociodem()

    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    selected_env_itm <- input$env_dimensions_7
    selected_dmd_scn <- input$dmd_scn_7
    selected_measure <- input$measure_7

    p_sociodem <- ggplot(data,
                         aes(
                           x = factor(
                             age,
                             level = c(
                               "MLE",
                               "FML",
                               "BTH",
                               "0-9",
                               "10-19",
                               "20-39",
                               "40-64",
                               "65+",
                               "all-a",
                               "low",
                               "medium",
                               "high",
                               "all-e",
                               "urban",
                               "rural",
                               "all-u"
                             )
                           ),
                           y = value,
                           label = value
                         )) +
      #fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds")))) +
      geom_col(aes(fill = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )), color = "white") +
      #coord_flip() is an easy way to swtich the x and y axis. Depending on what we want the user to focus on, each vis has its advantages. To see
      #the graph with the impacts on the y axis and the sociodem/age variables on the x axis, comment the coord_flip() call, AND switch the arguments in facet_grid to scales = "free_x", space = "free", switch = "x".
      coord_flip() +
      facet_grid(category ~ region_custom, scales = "free_y", space = "free_y", switch = "y", shrink = FALSE,
                 labeller = labeller(region_custom = label_wrap_gen(width = 15),
                                     category = label_wrap_gen(width = 5))
      ) +
      scale_fill_manual(values = colors_food) +
      labs(title = paste(selected_measure,
                         " diet-related ",
                         selected_env_itm,
                         " in 2020,\n",
                         "based on ",
                         selected_dmd_scn,
                         sep = "") ,
           #subtitle ="Note: value ranges along the x-axis differ across plots" ,
           #caption = "LSHTM - Centre for Climate Change and Planetary Health",
           x = NULL,
           y = paste(selected_env_itm,
                     "in 2020",
                     sep = " "),
           fill = NULL
      ) +
      lshtm_theme_few()
  })

  output$plot_sociodem <- renderPlot({
    print(reactive_plot_sociodem())
  })


  reactive_plot_sociodem_rel <- reactive({
    data <- filtered_data_sociodem_rel()

    data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    selected_env_itm <- input$env_dimensions_8
    selected_dmd_scn <- input$dmd_scn_8
    selected_measure <- input$measure_8

    p_sociodem_rel <- ggplot(data,
                             aes(
                               x = factor(
                                 age,
                                 level = c(
                                   "MLE",
                                   "FML",
                                   "BTH",
                                   "0-9",
                                   "10-19",
                                   "20-39",
                                   "40-64",
                                   "65+",
                                   "all-a",
                                   "low",
                                   "medium",
                                   "high",
                                   "all-e",
                                   "urban",
                                   "rural",
                                   "all-u"
                                 )
                               ),
                               y = value,
                               label = value
                             )) +
      geom_col(aes(fill = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )), color = "white") +
      coord_flip() +
      facet_grid(category ~ region_custom,
                 scales = "free_y",
                 space = "free_y", switch = "y", shrink = FALSE,
                 labeller = labeller(region_custom = label_wrap_gen(width = 15),
                                     category = label_wrap_gen(width = 5))
      ) +
      scale_fill_manual(values = colors_food) +
      labs(
        title = paste("diet-related ",
                      selected_env_itm,
                      " in 2020,\n",
                      "based on ",
                      selected_dmd_scn,
                      sep = "") ,
        #caption = "LSHTM - Centre for Climate Change and Planetary Health",
        x = NULL,
        y = paste("% contribution to diet-related ",selected_env_itm, "\nas ", selected_measure, sep = ""),
        fill = NULL
      ) +
      lshtm_theme_few()
  })

  output$plot_sociodem_rel <- renderPlot({
    print(reactive_plot_sociodem_rel())
  })

  reactive_plot_all_sociodem <- reactive({
    data <- filtered_data_all_sociodem()

    p_all_sociodem <- ggplot(data,
                             aes(x = age,
                                 y = value,
                                 label = value)) +
      geom_col(
        aes(
          #fill = sex
        ),
        position = "stack"
      ) +
      coord_flip() +
      facet_grid(region ~ edu + urban + sex) +
      lshtm_theme_few()
  })

  output$plot_all_sociodem <- renderPlot({
    print(reactive_plot_all_sociodem())
  })

  ##Draw plots for tabs in the second item (region) ----

  reactive_plot_region <- reactive({

    data <- filtered_data_region()

    selected_dmd_scn <- input$dmd_scn_1
    selected_measure <- input$measure_1

    p_region <- ggplot(data, aes(
      x = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      coord_flip() +
      scale_fill_manual(values = colors_food) +
      scale_x_discrete(breaks = c("WLD", "HIC", "UMC", "LMC", "LIC"),
                       labels = c(
                         "World",
                         "High Income",
                         "Upper Middle Income",
                         "Lower Middle Income",
                         "Low Income"
                       )) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      facet_wrap(
        ~ env_itm,
        scales = "free_x",
        ncol = 4,
        labeller = labeller(env_itm = label_wrap_gen(width = 15)),
        strip.position = "top"
      ) +
      labs(#caption = "LSHTM - Centre for Climate Change and Planetary Health",
        title = paste("Diet-related environmental impact from\n",
                      selected_dmd_scn,", in 2020 (", selected_measure, ")\n", sep = ""),
        x = NULL ,
        #y = "Diet-related environmental impact in 2020",
        y = NULL,
        fill = NULL) +
      lshtm_theme_few()
  })

  output$plot_region <- renderPlot({
    print(reactive_plot_region())
  })

  reactive_plot_regiongeo <- reactive({
    data <- filtered_data_regiongeo()

    selected_env_itm <- input$env_dimensions_5
    selected_dmd_scn <- input$dmd_scn_5
    selected_measure <- input$measure_5

    p_regiongeo <- ggplot(data, aes(
      x = factor(
        region,
        level = c(
          "WLD",
          "NAC",
          "ECS",
          "LCN",
          "MEA",
          "EAS",
          "SAS",
          "SSF"
        )
      ),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "fruit_veg",
          "oils",
          "sugar",
          "roots",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      coord_flip() +
      scale_fill_manual(values = colors_food) +
      scale_x_discrete(
        breaks = c("WLD", "NAC", "ECS", "LCN", "MEA", "EAS", "SAS", "SSF"),
        labels = c(
          "World",
          "North America",
          "Europe & C. Asia",
          "Latin Am. & Caribbean",
          "Middle East & N. Africa",
          "E. Asia & Pacific",
          "South Asia",
          "Sub-Saharan Africa"
        )
      ) +
      facet_wrap(
        ~ env_itm,
        scales = "free_x",
        ncol = 4,
        labeller = labeller(env_itm = label_wrap_gen(width = 15)),
        strip.position = "top"
      ) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      labs(title = paste("Diet-related environmental impact from\n",
                         selected_dmd_scn,", in 2020 (", selected_measure, ")\n", sep = ""),
           #caption = "LSHTM - Centre for Climate Change and Planetary Health",
           x = NULL,
           #y = "Impact",
           y = NULL,
           fill = NULL) +
      lshtm_theme_few()
  })

  output$plot_regiongeo <- renderPlot({
    print(reactive_plot_regiongeo())
  })

  ##Draw plots for tabs in the third item (category) ----

  reactive_plot_category <- reactive({

    selected_env_itm <- input$env_dimensions_4
    selected_dmd_scn <- input$dmd_scn_4
    selected_measure <- input$measure_4

    data <- filtered_data_category()

    data$region_custom <-
      factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    p_category <- ggplot(data, aes(
      x = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "roots",
          "fruit_veg",
          "oils",
          "sugar",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      ),
      y = value,
      fill = macrofoods
    )) +
      geom_col(color = "white", width = 0.6) +
      coord_flip() +
      #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
      facet_wrap( ~ region_custom,
                  ncol = 5,
                  scales = "free_x",
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
      ) +
      scale_fill_manual(values = colors_macro, breaks = c("ASF", "Staples", "Other")) +
      labs(title = paste("Diet-related ",
                         selected_env_itm,
                         " from\n",
                         selected_dmd_scn,", in 2020 (", selected_measure, ")\n", sep = ""),
           subtitle = "Note: value ranges along the x-axis differ across plots",
           #caption = "LSHTM - Centre for Climate Change and Planetary Health",
           x = "Food Group",
           y = paste(selected_env_itm),
           fill = NULL) +
      lshtm_theme_few()

  })

  output$plot_category <- renderPlot({
    print(reactive_plot_category())
  })


  reactive_plot_categorymacro <- reactive({

    selected_env_itm <- input$env_dimensions_6
    selected_dmd_scn <- input$dmd_scn_6
    selected_measure <- input$measure_6

    data <- filtered_data_categorymacro()

    data$region_custom <-
      factor(data$region, levels = custom_order_region, labels = custom_labels_region)

    p_categorymacro <- ggplot(data, aes(
      x = factor(macrofoods, level = c("ASF", "Staples", "Other")),
      y = value,
      fill = factor(
        food_group,
        level = c(
          "beef",
          "lamb",
          "dairy",
          "pork",
          "othr_meat",
          "fish",
          "othr_ani",
          "rice",
          "grains",
          "roots",
          "fruit_veg",
          "oils",
          "sugar",
          "legumes",
          "nuts_seeds",
          "other",
          "total"
        )
      )
    )) +
      geom_col(color = "white", width = 0.6) +
      #scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      facet_wrap( ~ region_custom , ncol = 5, scales = "free_x",
                  labeller = labeller(region_custom = label_wrap_gen(width = 15))
      ) +
      scale_fill_manual(values = colors_food) +
      #geom_text_repel(aes(label = value), show.legend = FALSE) +
      labs(title = paste("Diet-related ",
                         selected_env_itm,
                         " from\n",
                         selected_dmd_scn,", in 2020 (", selected_measure, ")\n", sep = ""),
           #subtitle = "Note: value ranges along the x-axis differ across plots",
           #caption = "LSHTM - Centre for Climate Change and Planetary Health",
           x = "Food Category",
           y = paste(selected_env_itm),
           fill = NULL) +
      lshtm_theme_few()
  })

  output$plot_categorymacro <- renderPlot({
    print(reactive_plot_categorymacro())
  })


  ##Draw plots for fourth item (radar) ----

  reactive_plot_regionradar <- reactive({

    data <- filtered_data_regionradar()

    #I need to play around with these input data frames to rearrange the plot so that the scale is at the top
    segments_1 <- data.frame(
      x1=rep(0.5,4),
      x2=rep(5,4),
      y1=c(0,50,100,150),
      y2=c(0,50,100,150)
    )

    labels_1 <- data.frame(
      y=c(0,50,100,150),
      x=rep(0.25,4)
    )

    p_regionradar <- ggplot(data, aes(
      x = #region,
        factor(region, level = c("HIC", "UMC", "LMC", "LIC", "WLD")),
      y = value,
      fill = category
      #fill = macrofoods
    )) +
      coord_polar() +
      theme_void() +
      geom_textpath(inherit.aes = FALSE,
                    # mapping = aes(x= factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                    # label = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                    mapping = aes(x = region,
                                  label = region,
                                  y =190),
                    text_only = TRUE, upright = TRUE
      ) +
      geom_segment(inherit.aes = FALSE,
                   data = segments_1,
                   mapping = aes(x=x1, xend=x2,y=y1,yend=y2), linewidth = 0.35, linetype = "dotted", color = "grey") +
      geom_col(
        #color = "white",
        alpha = 0.9,
        width = 0.6,
        #fill = "#a6a79b"
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(-60,190)) +
      geom_textsegment(inherit.aes = FALSE,
                       data = labels_1,
                       mapping = aes(x=4.5,xend=5.5,y=y,yend=y, label=
                                       #c("0%","25%","50%","75%","100%","125%","150%")
                                       y
                       ),
                       linewidth=0.35,
                       size=2.5,
                       linetype = "solid",
                       fontface = "bold"
      ) +
      facet_wrap(~ factor(age, level = c("urban",
                                         "rural",
                                         "FML",
                                         "MLE",
                                         "0-9",
                                         "10-19",
                                         "20-39",
                                         "40-64",
                                         "65+",
                                         "low",
                                         "medium",
                                         "high"
      )),
      ncol = 4
      ) +
      # scale_fill_manual(values = colors_macro
      #                   #, breaks = c("ASF", "Staples", "Other")
      #                   ) +
      scale_fill_manual(values = colors_sociodem_category) +
      lshtm_theme_few_radar()
  })

  output$plot_regionradar <-
    renderPlot({
      print(reactive_plot_regionradar())
    })


  reactive_plot_regionradargeo <- reactive({

    data <- filtered_data_regionradargeo()

    segments_1 <- data.frame(
      x1=rep(0.5,6),
      x2=rep(8,6),
      y1=c(0,50,100,150,200,250),
      y2=c(0,50,100,150,200,250)
    )

    labels_1 <- data.frame(
      y=c(0,50,100,150,200,250),
      x=rep(0.25,6)
    )

    p_regionradargeo <- ggplot(data, aes(
      x = region,
      #factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
      y = value
      #fill = macrofoods
    )) +
      coord_polar() +
      theme_void() +
      geom_textpath(inherit.aes = FALSE,
                    # mapping = aes(x= factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                    # label = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                    mapping = aes(x = region,
                                  label = region,
                                  y =285),
                    text_only = TRUE, upright = TRUE
      ) +
      geom_segment(inherit.aes = FALSE,
                   data = segments_1,
                   mapping = aes(x=x1, xend=x2,y=y1,yend=y2), linewidth = 0.35, linetype = "dotted", color = "grey") +
      geom_col(
        #color = "white",
        alpha = 0.9,
        width = 0.6,
        #fill = "#a6a79b"
        show.legend = FALSE
      ) +
      scale_y_continuous(limits = c(-60,285)) +
      geom_textsegment(inherit.aes = FALSE,
                       data = labels_1,
                       mapping = aes(x=7.5,xend=8.5,y=y,yend=y, label=
                                       #c("0%","25%","50%","75%","100%","125%","150%")
                                       y
                       ),
                       linewidth=0.35,
                       size=2.5,
                       linetype = "dotted"
      ) +
      facet_wrap(~ env_itm,
                 ncol = 4) +
      lshtm_theme_few_radar()
  })

  output$plot_regionradargeo <-
    renderPlot({
      print(reactive_plot_regionradargeo())
    })

  #Generate data tables ----

  #Generating a table from the user's data selection is not straightforward in cases where the plot is faceting using the
  #commands #face_wrap or #facet_grid. For example, in the sexage tab the code offers the user the chance to facet multiple plots
  #in a single image by selecting to display data for several regions at once. The age group is on the x axis, the impact value is on the
  #y axis, and the sex category drives the coloring of the data points. This adds another layer of data that a static table cannot handle.
  #The code below works around this issue by going through a few steps of data manipulation. The bulk of it was suggested by Chat GPT, but I had to make
  #adjustments to make it work in this context. There may be an easier way to code this, but I have not been able to find it yet.

  ##Generate data table for sexage ----
  #Create table for the sexage tab, starting from the subsection of the main dataset identified by the filtered_data_sexage element, that here is called as a function.
  sexage_table <- reactive({
    data <- filtered_data_sexage()

    #Add a unique identifier column based on all relevant variables. This is needed because otherwise the datatable command used further down will automatically
    #group into a single row instances where different combinations of variables in the dataset correspond to the same impact value on the y axis.
    #If for example FML aged 10-19 have an impact value of 1.19 in both HIC and LIC, the datatable command would display just one row for the value 1.19, and list
    #both HIC and LIC in the region column. Creating a unique identifier that is formed by the values taken by each variable in each occurrence ensures
    #that we can generate a table in which duplicate values are presented in distinct rows.
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))

    #Pivot the data to long format including the facet variable 'region'. This basically means expanding the number of
    #rows in the data selection. The table is a reactive element that follows the inputs
    #chosen by the user, but datatable needs to have access to the full resolution of the underlying dataset or it won't work.
    #By using pivot_longer, we are creating a new data frame that includes all occurrences of each variable for all values of
    #age.education, the variable on the x axis, and the region, the variable that drives the plot faceting.
    data_long <- data %>%
      pivot_longer(cols = c(age.education, region),
                   names_to = "variable",
                   values_to = "values")

    #Pivot the long data to a wide format, including the facet variable 'region'. This builds on the previous step, expanding on the
    #columns rather than the rows, ensuring that the resulting dataframe includes all the possible occurrences of the variables
    #specified in the pivot_longer call.
    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    #Convert the list column to character format, this is to avoid errors when creating the HTML table below.
    #Converting everything in the dataframe to characters ensures that R won't get confused by variables that are expressed in
    #different formats
    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    #generate the dataframe that results from the previous permutations. This concludes the reactive call that results in sexage_table
    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$sexage_table <- renderUI({
    sexage_data <- sexage_table()


    #Remove the 'unique_id' column from the data. We created this in the previous section to ensure that all occurrences of the value
    #on the y axis are assigned a distinct row, but we don't need to show that column to the user.
    sexage_data <- sexage_data[, !colnames(sexage_data) %in% c("unique_id", "box")]

    #Convert the list column 'age.education' to a character vector, ensuring there are no errors when R has to read it
    sexage_data$age.education <- sapply(sexage_data$age.education, paste, collapse = ", ")
    #This is the command that creates the actual table. The pageLength argument tells R to display a table that is as long
    #as there are rows in the originating dataset - which is defined dynamically by the user's choice of inputs.
    #The scrollX and scrollY arguments simply make the table scrollable across both axes. I set rownames to TRUE so that
    #R automatically assigns a number to each rowm to make it easier to count records.
    table_html <- datatable(sexage_data,
                            options = list(dom = 't', pageLength = nrow(sexage_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_sexage <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("sexage_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      sexage_data_export <- sexage_table()

      # Remove the 'unique_id' column
      sexage_data_export <- sexage_data_export[, !colnames(sexage_data_export) %in% c("unique_id", "box")]

      # Convert all columns to character
      sexage_data_export[] <- lapply(sexage_data_export, as.character)

      # Write the data to a CSV file
      write.csv(sexage_data_export, file, row.names = FALSE)
    }
  )

  ##Generate data table for eduurb ----
  #Create table for the eduurb tab, starting from the subsection of the main dataset identified by the filtered_data_eduurb element, that here is called as a function.
  eduurb_table <- reactive({
    data <- filtered_data_eduurb()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(sex.urbanisation, region),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$eduurb_table <- renderUI({

    eduurb_data <- eduurb_table()
    eduurb_data <- eduurb_data[, !colnames(eduurb_data) %in% c("unique_id", "box")]
    eduurb_data$sex.urbanisation <- sapply(eduurb_data$sex.urbanisation, paste, collapse = ", ")
    table_html <- datatable(eduurb_data,
                            options = list(dom = 't', pageLength = nrow(eduurb_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_eduurb <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("eduurb_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      eduurb_data_export <- eduurb_table()

      # Remove the 'unique_id' column
      eduurb_data_export <- eduurb_data_export[, !colnames(eduurb_data_export) %in% c("unique_id", "box")]

      # Convert all columns to character
      eduurb_data_export[] <- lapply(eduurb_data_export, as.character)

      # Write the data to a CSV file
      write.csv(eduurb_data_export, file, row.names = FALSE)
    }
  )

  ##Generate data table for sociodem ----
  #Create table for the sociodem tab, starting from the subsection of the main dataset identified by the filtered_data_sociodem element, that here is called as a function.
  sociodem_table <- reactive({
    data <- filtered_data_sociodem()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, category, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(food_group, region),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$sociodem_table <- renderUI({

    sociodem_data <- sociodem_table()
    sociodem_data <- sociodem_data[, !colnames(sociodem_data) %in% "unique_id"]
    sociodem_data$food_group <- sapply(sociodem_data$food_group, paste, collapse = ", ")
    table_html <- datatable(sociodem_data,
                            options = list(dom = 't', pageLength = nrow(sociodem_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_sociodem <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("sociodem_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      sociodem_data_export <- sociodem_table()

      # Remove the 'unique_id' column
      sociodem_data_export <- sociodem_data_export[, !colnames(sociodem_data_export) %in% "unique_id"]

      # Convert all columns to character
      sociodem_data_export[] <- lapply(sociodem_data_export, as.character)

      # Write the data to a CSV file
      write.csv(sociodem_data_export, file, row.names = FALSE)
    }
  )

  sociodem_rel_table <- reactive({
    data <- filtered_data_sociodem_rel()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, category, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(age, region),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$sociodem_rel_table <- renderUI({

    sociodem_rel_data <- sociodem_rel_table()
    sociodem_rel_data <- sociodem_rel_data[, !colnames(sociodem_rel_data) %in% "unique_id"]
    sociodem_rel_data$age <- sapply(sociodem_rel_data$age, paste, collapse = ", ")
    table_html <- datatable(sociodem_rel_data,
                            options = list(dom = 't', pageLength = nrow(sociodem_rel_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_sociodem_rel <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("sociodem_rel_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      sociodem_rel_data_export <- sociodem_rel_table()

      # Remove the 'unique_id' column
      sociodem_rel_data_export <- sociodem_rel_data_export[, !colnames(sociodem_rel_data_export) %in% "unique_id"]

      # Convert all columns to character
      sociodem_rel_data_export[] <- lapply(sociodem_rel_data_export, as.character)

      # Write the data to a CSV file
      write.csv(sociodem_rel_data_export, file, row.names = FALSE)
    }
  )

  ##Generate data table for region ----
  #Create table for the region tab, starting from the subsection of the main dataset identified by the filtered_data_eduurb element, that here is called as a function.
  region_table <- reactive({
    data <- filtered_data_region()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(env_itm, food_group),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$region_table <- renderUI({

    region_data <- region_table()
    region_data <- region_data[, !colnames(region_data) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
    region_data$env_itm <- sapply(region_data$env_itm, paste, collapse = ", ")
    table_html <- datatable(region_data,
                            options = list(dom = 't', pageLength = nrow(region_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_region <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("region_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      region_data_export <- region_table()

      # Remove the 'unique_id' column
      region_data_export <- region_data_export[, !colnames(region_data_export) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]

      # Convert all columns to character
      region_data_export[] <- lapply(region_data_export, as.character)

      # Write the data to a CSV file
      write.csv(region_data_export, file, row.names = FALSE)
    }
  )


  ##Generate data table for regiongeo ----
  #Create table for the regiongeo tab, starting from the subsection of the main dataset identified by the filtered_data_regiongeo element, that here is called as a function.
  regiongeo_table <- reactive({
    data <- filtered_data_regiongeo()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, box, sex.urbanisation, age.education, region, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(env_itm, food_group),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$regiongeo_table <- renderUI({

    regiongeo_data <- regiongeo_table()
    regiongeo_data <- regiongeo_data[, !colnames(regiongeo_data) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]
    regiongeo_data$env_itm <- sapply(regiongeo_data$env_itm, paste, collapse = ", ")
    table_html <- datatable(regiongeo_data,
                            options = list(dom = 't', pageLength = nrow(regiongeo_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_regiongeo <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("regiongeo_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      regiongeo_data_export <- regiongeo_table()

      # Remove the 'unique_id' column
      regiongeo_data_export <- regiongeo_data_export[, !colnames(regiongeo_data_export) %in% c("unique_id", "box", "age.education", "sex.urbanisation")]

      # Convert all columns to character
      regiongeo_data_export[] <- lapply(regiongeo_data_export, as.character)

      # Write the data to a CSV file
      write.csv(regiongeo_data_export, file, row.names = FALSE)
    }
  )


  ##Generate data table for category ----
  #Create table for the category tab, starting from the subsection of the main dataset identified by the filtered_data_category element, that here is called as a function.
  category_table <- reactive({
    data <- filtered_data_category()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, macrofoods, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(region, macrofoods),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$category_table <- renderUI({

    category_data <- category_table()
    category_data <- category_data[, !colnames(category_data) %in% "unique_id"]
    category_data$region <- sapply(category_data$region, paste, collapse = ", ")
    table_html <- datatable(category_data,
                            options = list(dom = 't', pageLength = nrow(category_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_category <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("category_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      category_data_export <- category_table()

      # Remove the 'unique_id' column
      category_data_export <- category_data_export[, !colnames(category_data_export) %in% "unique_id"]

      # Convert all columns to character
      category_data_export[] <- lapply(category_data_export, as.character)

      # Write the data to a CSV file
      write.csv(category_data_export, file, row.names = FALSE)
    }
  )

  ##Generate data table for category macro ----
  #Create table for the category macro tab, starting from the subsection of the main dataset identified by the filtered_data_categorymacro element, that here is called as a function.
  categorymacro_table <- reactive({
    data <- filtered_data_categorymacro()
    data <- data %>%
      mutate(unique_id = paste(measure, env_itm, dmd_scn, food_group, region, age, macrofoods, sep = "_"))

    data_long <- data %>%
      pivot_longer(cols = c(region, food_group),
                   names_to = "variable",
                   values_to = "values")

    data_wide <- data_long %>%
      pivot_wider(names_from = variable, values_from = values,
                  values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries

    data_wide <- data_wide %>%
      mutate(across(everything(), ~lapply(., as.character)))  # Convert to character

    return(data_wide)
  })

  #Now create the actual table based on the dataframe that results from the previous section
  output$categorymacro_table <- renderUI({

    categorymacro_data <- categorymacro_table()
    categorymacro_data <- categorymacro_data[, !colnames(categorymacro_data) %in% "unique_id"]
    categorymacro_data$region <- sapply(categorymacro_data$region, paste, collapse = ", ")
    table_html <- datatable(categorymacro_data,
                            options = list(dom = 't', pageLength = nrow(categorymacro_data),
                                           scrollX = TRUE, scrollY = TRUE),
                            rownames = TRUE)  # Include the default row numbers

    return(table_html)
  })

  #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
  output$download_csv_categorymacro <- downloadHandler(
    filename = function() {
      # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
      paste("categorymacro_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Create a copy of the data to avoid modifying the original data
      categorymacro_data_export <- categorymacro_table()

      # Remove the 'unique_id' column
      categorymacro_data_export <- categorymacro_data_export[, !colnames(categorymacro_data_export) %in% "unique_id"]

      # Convert all columns to character
      categorymacro_data_export[] <- lapply(categorymacro_data_export, as.character)

      # Write the data to a CSV file
      write.csv(categorymacro_data_export, file, row.names = FALSE)
    }
  )

  #Generate code to download the plots ----

  output$download_plot_sexage <- downloadHandler(
    filename = function() { paste("plot_sexage", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_sexage(),
                                      device = "png", width = 12) }
  )

  output$download_plot_eduurb <- downloadHandler(
    filename = function() { paste("plot_eduurb", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_eduurb(),
                                      device = "png", width = 12) }
  )

  output$download_plot_sociodem <- downloadHandler(
    filename = function() { paste("plot_sociodem", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_sociodem(),
                                      device = "png", width = 12) }
  )

  output$download_plot_sociodem_rel <- downloadHandler(
    filename = function() { paste("plot_sociodem_rel", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_sociodem_rel(),
                                      device = "png", width = 12) }
  )

  output$download_plot_region <- downloadHandler(
    filename = function() { paste("plot_region", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_region(),
                                      device = "png", width = 15) }
  )

  output$download_plot_regiongeo <- downloadHandler(
    filename = function() { paste("plot_regiongeo", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_regiongeo(),
                                      device = "png", width = 15) }
  )

  output$download_plot_category <- downloadHandler(
    filename = function() { paste("plot_category", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_category(),
                                      device = "png", width = 12) }
  )

  output$download_plot_categorymacro <- downloadHandler(
    filename = function() { paste("plot_categorymacro", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_categorymacro(),
                                      device = "png", width = 12) }
  )

  output$download_plot_regionradar <- downloadHandler(
    filename = function() { paste("plot_regionradar", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_regionradar(),
                                      device = "png", width = 12) }
  )

  output$download_plot_regionradargeo <- downloadHandler(
    filename = function() { paste("plot_regionradargeo", '.png', sep='') },
    content = function(file) { ggsave(file, plot = reactive_plot_regionradargeo(),
                                      device = "png", width = 12) }
  )
  
}