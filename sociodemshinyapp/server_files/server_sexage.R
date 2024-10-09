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

reactive_plot_sexage <- reactive({
  data <- filtered_data_sexage()
  
  if (nrow(data) == 0) {
    return(NULL)  # Return NULL if there is no data to plot
  }
  
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
                       name =
                         "(100 = global mean)"
                       ) +
    scale_y_continuous(breaks = c(0, 50, 75, 100, 125, 150, 200)) +
    geom_text_repel(aes(label = value), show.legend = FALSE) +
    #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    facet_wrap( ~ region_custom, ncol = 5,
                labeller = labeller(region_custom = label_wrap_gen(width = 15))
    ) +
    geom_hline(yintercept = 100, alpha = 0.2, linewidth = 2) +
    #geom_texthline(mapping = NULL, data = NULL, yintercept = 100, label = "Global average") +
    labs(
      title = 
      #   paste(
      #   "Mean diet-related ",
      #   selected_env_itm_s,
      #   "\nper person, from ",
      #   selected_dmd_scn,
      #   ", in 2020",
      #   sep = ""
      # ),
        paste("Diet-related",
                    selected_env_itm_s,
                    "per person\nin 2020,",
                    "based on",
                    selected_dmd_scn,
                    #"(100 = world or regional average)",
                    sep = " "),
      # subtitle = paste("Values are expressed as",
      #                   selected_measure,
      #                  ", set equal to 100."),
      # caption = paste("Values are expressed as ",
      #                                selected_measure,
      #                               ",\nwhich has been set equal to 100.",
      #                 sep = ""),
      x = "Age",
      y = paste("Impact as ",
                selected_measure,
                #",\nset equal to 100.",
                sep = "")
        #"100 = global mean"
        # paste(selected_env_itm_s,
        #         " as\n",
        #         selected_measure,
        #         #", with average set to 100",
        #         sep = "")
    ) +
    lshtm_theme_few() +
    theme(
        legend.position = "top"
    )
    #theme(legend.position = "top")
})

output$plot_sexage <- renderPlot({
  validate(
    need(nrow(filtered_data_sexage()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
  )
  print(reactive_plot_sexage())
})

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
  #R automatically assigns a number to each row to make it easier to count records.
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

output$download_plot_sexage <- downloadHandler(
  filename = function() { paste("plot_sexage", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_sexage(),
                                    device = "png", width = 12) }
)