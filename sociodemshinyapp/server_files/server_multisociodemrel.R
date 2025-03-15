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

observe({
  if (input$measure_8 %in% c("ratio to global avg (absolute)", 
                             "ratio to regional avg (absolute)",
                             "ratio to regional mean (capita)",
                             "ratio to global mean (capita)"))
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "env_dimensions_8",
                      choices = c(
                        "GHG",  # Subscript 2
                        "water use",
                        "land use",
                        "land use, crops",
                        "land use, pasture",
                        "eutrophication pot.",
                        "average environmental impact",
                        "average environmental impact (pb weighted)"
                      ))
})

reactive_plot_sociodem_rel <- reactive({
  data <- filtered_data_sociodem_rel()
  
  data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
  data$category_custom <- factor(data$category, levels = custom_order_category)
  
  selected_env_itm <- input$env_dimensions_8
  selected_dmd_scn <- input$dmd_scn_8
  selected_measure <- input$measure_8
  
  p_sociodem_rel <- ggplot(data,
                           aes(
                             x = value,
                             y = factor(
                               age,
                               level = 
                                 custom_order_sociodem
                             )
                             ,label = value
                           )
                           ) +
    geom_vline(xintercept = 100, alpha = 0.4, linewidth = 0.8) +
    geom_col(aes(fill = factor(
      food_group,
      level = custom_order_foodgroup
    )), color = "black") +
    #coord_flip() +
    facet_grid(category_custom ~ region_custom,
               scales = "free_y",
               space = "free_y", switch = "y", shrink = FALSE,
               labeller = labeller(region_custom = label_wrap_gen(width = 15),
                                   category = label_wrap_gen(width = 5))
    ) +
    scale_fill_manual(values = colors_food) +
    #scale_x_continuous(position = "top") +
    labs(
      title = 
        paste("Share of diet-related ",
                    selected_env_itm,
                    " in 2020,\n",
                    "based on ",
                    selected_dmd_scn,
                    sep = ""),
      #caption = "LSHTM - Centre for Climate Change and Planetary Health",
        #"Diet-related environmental impacts per person\nnormalised to 2000 kcal/d (ratio of regional average, %)",
      x = paste(
        "Impact per person as ",
        selected_measure,
        ", in %",
        sep = ""
      ),
        # paste("% contribution to diet-related ",selected_env_itm, "\nas ", selected_measure, sep = ""),
      y = NULL,
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

output$plot_sociodem_rel <- renderPlot({
  validate(
    need(nrow(filtered_data_sociodem_rel()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
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
