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
      level = custom_order_foodgroup
        
    )
  )) +
    geom_col(color = "black", width = 0.6) +
    #scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    facet_wrap( ~ region_custom , ncol = 5, scales = "free_x",
                labeller = labeller(region_custom = label_wrap_gen(width = 15))
    ) +
    scale_fill_manual(values = colors_food) +
    #geom_text_repel(aes(label = value), show.legend = FALSE) +
    labs(title = paste("Diet-related ",
                       selected_env_itm,
                       " from\n",
                       selected_dmd_scn,", in 2020 (", selected_measure, ")", sep = ""),
         #subtitle = "Note: value ranges along the x-axis differ across plots",
         #caption = "LSHTM - Centre for Climate Change and Planetary Health",
         x = "Food Category",
         y = paste(selected_env_itm),
         fill = NULL) +
    lshtm_theme_few()
})

output$plot_categorymacro <- renderPlot({
  validate(
    need(nrow(filtered_data_categorymacro()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  
  print(reactive_plot_categorymacro())
})

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

output$download_plot_categorymacro <- downloadHandler(
  filename = function() { paste("plot_categorymacro", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_categorymacro(),
                                    device = "png", width = 12) }
)