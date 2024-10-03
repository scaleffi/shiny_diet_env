filtered_data_regionradar <- reactive({
  #df %>%
  
  df_trs_category %>%
    filter(measure == input$measure_10,
           env_itm %in% input$env_dimensions_10,
           food_group == "total",
           age %in% input$age_10,
           dmd_scn == input$dmd_scn_10,
           region %in% c("WLD", "HIC", "UMC", "LMC", "LIC")
           #macrofoods %in% c("ASF", "Staples", "Other")
    )
  
})

reactive_plot_regionradar <- reactive({
  
  data <- filtered_data_regionradar()
  
  #I need to play around with these input data frames to rearrange the plot so that the scale is at the top
  segments_1 <- data.frame(
    x1=rep(0.5,4),
    x2=rep(5,4),
    y1=c(0,50,100,150),
    y2=c(0,50,100,150)
  )
  
  labels_1 <- data.frame(
    y=c(0,50,100,150),
    x=rep(0.25,4)
  )
  
  p_regionradar <- ggplot(data, aes(
    x = #region,
      factor(region, level = c("HIC", "UMC", "LMC", "LIC", "WLD")),
    y = value,
    fill = category
    #fill = macrofoods
  )) +
    coord_polar() +
    theme_void() +
    geom_textpath(inherit.aes = FALSE,
                  # mapping = aes(x= factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  # label = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  mapping = aes(x = region,
                                label = region,
                                y =190),
                  text_only = TRUE, upright = TRUE
    ) +
    geom_segment(inherit.aes = FALSE,
                 data = segments_1,
                 mapping = aes(x=x1, xend=x2,y=y1,yend=y2), linewidth = 0.35, linetype = "dotted", color = "grey") +
    geom_col(
      #color = "white",
      alpha = 0.9,
      width = 0.6,
      #fill = "#a6a79b"
      show.legend = FALSE
    ) +
    scale_y_continuous(limits = c(-60,190)) +
    geom_textsegment(inherit.aes = FALSE,
                     data = labels_1,
                     mapping = aes(x=4.5,xend=5.5,y=y,yend=y, label=
                                     #c("0%","25%","50%","75%","100%","125%","150%")
                                     y
                     ),
                     linewidth=0.35,
                     size=2.5,
                     linetype = "solid",
                     fontface = "bold"
    ) +
    facet_wrap(~ factor(age, level = c("urban",
                                       "rural",
                                       "FML",
                                       "MLE",
                                       "0-9",
                                       "10-19",
                                       "20-39",
                                       "40-64",
                                       "65+",
                                       "low",
                                       "medium",
                                       "high"
    )),
    ncol = 4
    ) +
    # scale_fill_manual(values = colors_macro
    #                   #, breaks = c("ASF", "Staples", "Other")
    #                   ) +
    scale_fill_manual(values = colors_sociodem_category) +
    lshtm_theme_few_radar()
})

output$plot_regionradar <-
  renderPlot({
    validate(
      need(nrow(filtered_data_regionradar()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
    )
    print(reactive_plot_regionradar())
  })

output$download_plot_regionradar <- downloadHandler(
  filename = function() { paste("plot_regionradar", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_regionradar(),
                                    device = "png", width = 12) }
)

