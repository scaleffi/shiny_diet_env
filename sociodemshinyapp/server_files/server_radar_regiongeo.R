filtered_data_regionradargeo <- reactive({
  #df %>%
  
  df_trs_category %>%
    filter(measure == input$measure_11,
           env_itm %in% input$env_dimensions_11,
           food_group == "total",
           age == "all-a",
           dmd_scn == input$dmd_scn_11,
           region %in% c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF")
           #macrofoods %in% c("ASF", "Staples", "Other")
    )
  
})

reactive_plot_regionradargeo <- reactive({
  
  data <- filtered_data_regionradargeo()
  
  data$region_custom <- factor(data$region, levels = custom_order_region, labels = custom_labels_region)
  
  segments_1 <- data.frame(
    x1=rep(0.5,6),
    x2=rep(8,6),
    y1=c(0,50,100,150,200,250),
    y2=c(0,50,100,150,200,250)
  )
  
  labels_1 <- data.frame(
    y=c(0,50,100,150,200,250),
    x=rep(0.25,6)
  )
  
  p_regionradargeo <- ggplot(data, aes(
    x = #region,
    factor(region, level = c("WLD", "NAC", "LCN", "ECS", "MEA", "SAS", "EAS", "SSF")),
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
                     mapping = aes(x=7.5,xend=8.5,y=y,yend=y, label=
                                     #c("0%","25%","50%","75%","100%","125%","150%")
                                     y
                     ),
                     linewidth=0.35,
                     size=2.5,
                     linetype = "dotted"
    ) +
    facet_wrap(~ env_itm,
               ncol = 4
               ) +
    lshtm_theme_few_radar()
})

output$plot_regionradargeo <-
  renderPlot({
    print(reactive_plot_regionradargeo())
  })

output$download_plot_regionradargeo <- downloadHandler(
  filename = function() { paste("plot_regionradargeo", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_regionradargeo(),
                                    device = "png", width = 12) }
)

