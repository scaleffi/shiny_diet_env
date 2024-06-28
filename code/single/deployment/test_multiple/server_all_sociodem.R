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

#I have not yet written the code to download the plot and generate+download the table for this element

output$download_plot_all_sociodem <- downloadHandler(
  filename = function() { paste("plot_all_sociodem", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_all_sociodem(),
                                    device = "png", width = 12) }
)

# all_sociodem_table <- reactive({
#   data <- filtered_data_all_sociodem()
#   data <- data %>%
#     mutate(unique_id = paste(measure, env_itm, dmd_scn, region, age, urban, edu, sex, sep = "_"))
#   
#   data_long <- data %>%
#     pivot_longer(cols = c(food_group, region),
#                  names_to = "variable",
#                  values_to = "values")
#   
#   data_wide <- data_long %>%
#     pivot_wider(names_from = variable, values_from = values,
#                 values_fn = list)  # Use values_fn to create a list, this is needed because what we are displaying is a list of distinct entries
#   
#   data_wide <- data_wide %>%
#     mutate(across(everything(), ~lapply(., as.character)))  # Convert to character
#   
#   return(data_wide)
# })
# 
# #Now create the actual table based on the dataframe that results from the previous section
# output$all_sociodem_table <- renderUI({
#   
#   all_sociodem_data <- all_sociodem_table()
#   all_sociodem_data <- all_sociodem_data[, !colnames(all_sociodem_data) %in% "unique_id"]
#   all_sociodem_data$food_group <- sapply(all_sociodem_data$food_group, paste, collapse = ", ")
#   table_html <- datatable(all_sociodem_data,
#                           options = list(dom = 't', pageLength = nrow(sociodem_data),
#                                          scrollX = TRUE, scrollY = TRUE),
#                           rownames = TRUE)  # Include the default row numbers
#   
#   return(table_html)
# })
# 
# #Generate code to download the table. This call must match a downloadButton setup in the UI section of the code
# output$download_csv_all_sociodem <- downloadHandler(
#   filename = function() {
#     # Specify the filename for the downloaded file; in this case it's a name provided by the code and the current date
#     paste("all_sociodem_data_", Sys.Date(), ".csv", sep = "")
#   },
#   content = function(file) {
#     # Create a copy of the data to avoid modifying the original data
#     all_sociodem_data_export <- all_sociodem_table()
#     
#     # Remove the 'unique_id' column
#     all_sociodem_data_export <- all_sociodem_data_export[, !colnames(all_sociodem_data_export) %in% "unique_id"]
#     
#     # Convert all columns to character
#     all_sociodem_data_export[] <- lapply(all_sociodem_data_export, as.character)
#     
#     # Write the data to a CSV file
#     write.csv(all_sociodem_data_export, file, row.names = FALSE)
#   }
# )
