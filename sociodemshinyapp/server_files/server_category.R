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

observe({ # R observes an event based on conditions set below
  if (input$measure_4 == "absolute")
    
    # this function is a smart way to impact the UI based on a user selection,
    # without having to change any code in the UI part of the application.
    updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_4",
                      choices = c(
                        "GHG (Mt CO\u2082eq)",  # Subscript 2
                        "water use (km\u00B3)",
                        "land use (thousands of km\u00B2)",
                        "land use, crops (thousands of km\u00B2)",
                        "land use, pasture (thousands of km\u00B2)",
                        "eutrophication pot. (kt PO\u2084eq)"
                      ),
                      selected = "GHG (Mt CO\u2082eq)"
    ) else { if (input$measure_4 == "per capita")
      updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_4",
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
      level = custom_order_foodgroup
    ),
    y = value,
    fill = macrofoods
  )) +
    geom_col(color = "black", width = 0.6) +
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
                       selected_dmd_scn,", in 2020 (", selected_measure, ")", sep = ""),
         #subtitle = "Note: value ranges along the x-axis differ across plots",
         #caption = "LSHTM - Centre for Climate Change and Planetary Health",
         x = "Food Group",
         y = paste(selected_env_itm),
         fill = NULL) +
    lshtm_theme_few()
  
})

output$plot_category <- renderPlot({
  validate(
    need(nrow(filtered_data_category()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_category())
})

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

output$download_plot_category <- downloadHandler(
  filename = function() { paste("plot_category", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_category(),
                                    device = "png", width = 12) }
)