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

observe({
  if (input$measure_3 %in% c("ratio to global mean (capita)","ratio to regional mean (capita)"))
    
    updateSelectInput(session = getDefaultReactiveDomain(),
                      "env_dimensions_3",
                      choices = c(
                        "GHG",  # Subscript 2
                        "water use",
                        "land use",
                        "land use, crops",
                        "land use, pasture",
                        "eutrophication pot.",
                        "average environmental impact",
                        "average environmental impact (pb weighted)"
                      ),
                      selected = "average environmental impact")
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
    geom_hline(yintercept = 100, alpha = 0.2, linewidth = 2) +
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
    scale_y_continuous(breaks = waiver() 
                         #c(0, 50, 75, 100, 125, 150, 175, 200)
                       ) +
    geom_text_repel(aes(label = value), show.legend = FALSE) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facet_wrap( ~ region_custom, ncol = 5,
                labeller = labeller(region_custom = label_wrap_gen(width = 15))
    ) +
    labs(
      title = paste("Diet-related",
                    selected_env_itm_u,
                    "per person\nin 2020,",
                    "based on",
                    selected_dmd_scn,
                    #"(100 = world or regional average)",
                    sep = " "),
      # subtitle = paste("Note: all data is based on ",
      #                  selected_dmd_scn,
      #                  ".",
      #                  #".\nIn the plot, average is set equal to 100.",
      #                  sep = "") ,
      #caption = "LSHTM - Centre for Climate Change and Planetary Health",
      x = "Education level",
      y = paste("Impact as ",
                    selected_measure,
                    #",\nset equal to 100.",
                    sep = "")
    ) +
    lshtm_theme_few()+
    theme(legend.position = "top")
})

output$plot_eduurb <- renderPlot({
  validate(
    need(nrow(filtered_data_eduurb()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_eduurb())
})

output$download_plot_eduurb <- downloadHandler(
  filename = function() { paste("plot_eduurb", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_eduurb(),
                                    device = "png", width = 12) }
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