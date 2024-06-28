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

output$download_plot_sociodem_rel <- downloadHandler(
  filename = function() { paste("plot_sociodem_rel", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_sociodem_rel(),
                                    device = "png", width = 12) }
)
