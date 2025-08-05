filtered_data_regiongeo <- reactive({
  df %>%
    filter(measure == input$measure_5,
           env_itm %in% input$env_dimensions_5,
           food_group %in% input$food_group_5,
           box == "age-sex",
           age.education == "all-a",
           sex.urbanisation == "BTH",
           dmd_scn == input$dmd_scn_5,
           region %in% input$region_5
             #c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF")
           )
})

observe({ # R observes an event based on conditions set below
  if (input$measure_5 == "absolute")
    
    # this function is a smart way to impact the UI based on a user selection,
    # without having to change any code in the UI part of the application.
    updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_5",
                      choices = c(
                        "GHG (Mt CO\u2082eq)",  # Subscript 2
                        "water use (km\u00B3)",
                        "land use (thousands of km\u00B2)",
                        "land use, crops (thousands of km\u00B2)",
                        "land use, pasture (thousands of km\u00B2)",
                        "eutrophication pot. (kt PO\u2084eq)"
                      ),
                      selected = c("GHG (Mt CO\u2082eq)",
                                   "water use (km\u00B3)",
                                   "land use (thousands of km\u00B2)",
                                   "eutrophication pot. (kt PO\u2084eq)")
    ) else { if (input$measure_5 == "per capita")
      updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_5",
                        choices = c(
                          "GHG (kg CO\u2082eq)",  # Subscript 2
                          "water use (m\u00B3)",
                          "land use (m\u00B2)",
                          "land use, crops (m\u00B2)",
                          "land use, pasture (m\u00B2)",
                          "eutrophication pot. (g PO\u2084eq)"
                        ),
                        selected = c("GHG (kg CO\u2082eq)",
                                     "water use (m\u00B3)",
                                     "land use (thousands of m\u00B2)",
                                     "eutrophication pot. (g PO\u2084eq)")
      ) 
    }
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
      level = custom_order_foodgroup
        
    )
  )) +
    geom_col(color = "black", width = 0.6) +
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
    labs(title = paste("Food-related environmental impact from\n",
                       selected_dmd_scn,", in 2020 (", selected_measure, ")", sep = ""),
         #caption = "LSHTM - Centre for Climate Change and Planetary Health",
         x = NULL,
         #y = "Impact",
         y = NULL,
         fill = NULL) +
    lshtm_theme_few()
})

output$plot_regiongeo <- renderPlot({
  validate(
    need(nrow(filtered_data_regiongeo()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_regiongeo())
})

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

output$download_plot_regiongeo <- downloadHandler(
  filename = function() { paste("plot_regiongeo", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_regiongeo(),
                                    device = "png", width = 15) }
)