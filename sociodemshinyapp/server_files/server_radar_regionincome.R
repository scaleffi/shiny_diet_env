filtered_data_regionradarincome <- reactive({
  #df %>%
  
  df_trs_category %>%
    filter(measure == input$measure_12,
           env_itm %in% input$env_dimensions_12,
           food_group == "total",
           age == "all-a",
           dmd_scn == input$dmd_scn_12,
           region %in% c("WLD", "HIC", "UMC", "LMC", "LIC")
           #macrofoods %in% c("ASF", "Staples", "Other")
    )
  
})

reactive_plot_regionradarincome <- reactive({
  
  data <- filtered_data_regionradarincome()
  
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
  
  p_regionradarincome <- ggplot(data, aes(
    x = #region,
    factor(region, level = c("HIC", "UMC", "LMC", "LIC", "WLD")),
    y = value
    #fill = macrofoods
  )) +
    coord_polar() +
    theme_void() +
    geom_textpath(inherit.aes = FALSE,
                  # mapping = aes(x= factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  # label = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  mapping = aes(x = region,
                                label = region,
                                y =285),
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
    scale_y_continuous(limits = c(-60,285)) +
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
    facet_wrap(~ env_itm,
               ncol = 4,
               labeller = labeller(env_itm = label_wrap_gen(width = 15))
               ) +
    lshtm_theme_few_radar()
})

output$plot_regionradarincome <-
  renderPlot({
    validate(
      need(nrow(filtered_data_regionradarincome()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
    )
    print(reactive_plot_regionradarincome())
  })

output$download_plot_regionradarincome <- downloadHandler(
  filename = function() { paste("plot_regionradarincome", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_regionradarincome(),
                                    device = "png", width = 12) }
)

