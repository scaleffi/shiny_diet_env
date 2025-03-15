filtered_data_sociodem <- reactive({
  df_trs_category %>%
    filter(measure == input$measure_7,
           env_itm %in% input$env_dimensions_7,
           food_group %in% input$food_group_7,
           age %in% input$age_7,
           dmd_scn == input$dmd_scn_7,
           region %in% input$region_7)
})

# ensure the user sees the appropriate choice of env dimensions depending on the measure selected
observe({ # R observes an event based on conditions set below
  if (input$measure_7 == "absolute")
    
    # this function is a smart way to impact the UI based on a user selection,
    # without having to change any code in the UI part of the application.
    updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_7",
                      choices = c(
                        "GHG (Mt CO\u2082eq)",  # Subscript 2
                        "water use (km\u00B3)",
                        "land use (thousands of km\u00B2)",
                        "land use, crops (thousands of km\u00B2)",
                        "land use, pasture (thousands of km\u00B2)",
                        "eutrophication pot. (kt PO\u2084eq)"
                      ),
                      selected = "GHG (Mt CO\u2082eq)"
    ) else { if (input$measure_7 == "per capita")
        updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_7",
                             choices = c(
                               "GHG (kg CO\u2082eq)",  # Subscript 2
                               "water use (m\u00B3)",
                               "land use (m\u00B2)",
                               "land use, crops (m\u00B2)",
                               "land use, pasture (m\u00B2)",
                               "eutrophication pot. (g PO\u2084eq)"
                             ),
                             selected = "GHG (kg CO\u2082eq)"
                             )
    }
})

reactive_plot_sociodem <- reactive({
  data <- filtered_data_sociodem()
  
  data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
  data$category_custom <- factor(data$category, levels = custom_order_category)
  
  selected_env_itm <- input$env_dimensions_7
  selected_dmd_scn <- input$dmd_scn_7
  selected_measure <- input$measure_7
  
  p_sociodem <- ggplot(data,
                       aes(
                         x = factor(
                           age,
                           level = custom_order_sociodem 
                             
                         ),
                         y = value,
                         label = value
                       )) +
    #fill = factor(food_group, level=c("beef","milk", "lamb", "pork", "poultry", "eggs", "fish", "rice", "grains", "fruit&veg", "oils", "sugar", "roots", "legumes", "nuts&seeds")))) +
    geom_col(aes(fill = factor(
      food_group,
      level = custom_order_foodgroup
    )), color = "black") +
    #coord_flip() is an easy way to swtich the x and y axis. Depending on what we want the user to focus on, each vis has its advantages. To see
    #the graph with the impacts on the y axis and the sociodem/age variables on the x axis, comment the coord_flip() call, AND switch the arguments in facet_grid to scales = "free_x", space = "free", switch = "x".
    coord_flip() +
    facet_grid(category_custom ~ region_custom, scales = "free_y", space = "free_y", switch = "y", shrink = FALSE,
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
    lshtm_theme_few() +
    theme(strip.text.y = element_text(face = "bold",
                                      size = 9
                                      #angle = -90
    ),
    strip.placement = "outside"
    )
})

output$plot_sociodem <- renderPlot({
  validate(
    need(nrow(filtered_data_sociodem()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_sociodem())
})

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

output$download_plot_sociodem <- downloadHandler(
  filename = function() { paste("plot_sociodem", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_sociodem(),
                                    device = "png", width = 12) }
)