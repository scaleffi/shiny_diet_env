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

observe({ # R observes an event based on conditions set below
  if (input$measure_1 == "absolute")
    
    # this function is a smart way to impact the UI based on a user selection,
    # without having to change any code in the UI part of the application.
    updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_1",
                      choices = c(
                        "GHG (Mt CO\u2082eq)",  # Subscript 2
                        "water use (km\u00B3)",
                        "land use (thousands of km\u00B2)",
                        "land use, crops (thousands of km\u00B2)",
                        "land use, pasture (thousands of km\u00B2)",
                        "eutrophication pot. (kt PO\u2084eq)"
                      ),
                      selected = "GHG (Mt CO\u2082eq)"
    ) else { if (input$measure_1 == "per capita")
      updateSelectInput(session = getDefaultReactiveDomain(), "env_dimensions_1",
                        choices = c(
                          "GHG (kg CO\u2082eq)",  # Subscript 2
                          "water use (m\u00B3)",
                          "land use (m\u00B2)",
                          "land use, crops (m\u00B2)",
                          "land use, pasture (m\u00B2)",
                          "eutrophication pot. (g PO\u2084eq)"
                        ),
                        selected = "GHG (kg CO\u2082eq)"
      ) else {if (input$measure_1 %in% c("ratio to global avg (absolute)", 
                                         #"ratio to regional avg (absolute)",
                                         #"ratio to regional mean (capita)",
                                         "ratio to global mean (capita)"))
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "env_dimensions_1",
                          choices = c(
                            "GHG",
                            "water use",
                            "land use",
                            "land use, crops",
                            "land use, pasture",
                            "eutrophication pot.",
                            "average environmental impact",
                            "average environmental impact (pb weighted)"
                          ), selected = "GHG")
        
      }
    }
})


reactive_plot_region <- reactive({
  
  data <- filtered_data_region()
  
  selected_dmd_scn <- input$dmd_scn_1
  selected_measure <- input$measure_1
  
  p_region <- ggplot(data, aes(
    x = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
    y = value,
    fill = factor(
      food_group,
      level = custom_order_foodgroup
    )
  )) +
    geom_col(color = "black", width = 0.6) +
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
                    selected_dmd_scn,", in 2020 (", selected_measure, ")", sep = ""),
      x = NULL ,
      #y = "Diet-related environmental impact in 2020",
      y = NULL,
      fill = NULL) +
    lshtm_theme_few()
})

output$plot_region <- renderPlot({
  validate(
    need(nrow(filtered_data_region()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_region())
})

#Create table for the region tab, starting from the subsection of the main dataset identified by the filtered_data_region element, that here is called as a function.
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

output$download_plot_region <- downloadHandler(
  filename = function() { paste("plot_region", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_region(),
                                    device = "png", width = 15) }
)