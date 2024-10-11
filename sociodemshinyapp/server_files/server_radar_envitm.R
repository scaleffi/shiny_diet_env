filtered_data_envitmradar <- reactive({
  #df %>%
  
  df_trs_category %>%
    mutate(
      env_itm = case_when(
        env_itm == "GHG (Mt CO2eq)" ~ "GHG",
        env_itm == "water use (km3)" ~ "water",
        env_itm == "land use (thousands of km2)" ~ "land",
        env_itm == "land use, crops (thousands of km2)" ~ "land_crop",
        env_itm == "land use, pasture (thousands of km2)" ~ "land_pstr",
        env_itm == "eutrophication pot. (kt PO4eq)" ~ "eutr",
        env_itm == "average environmental impact" ~ "avg",
        env_itm == "avg_pb" ~ "average environmental impact (pb weighted)",
        TRUE ~ env_itm
      )) %>%
    filter(measure == input$measure_13,
           env_itm %in% c("avg", "GHG", "water", "land", "eutr"),
           food_group == "total",
           age == "all-a",
           dmd_scn == input$dmd_scn_13,
           region %in% input$region_13
           #macrofoods %in% c("ASF", "Staples", "Other")
    )
  
})

reactive_plot_env_itmradar <- reactive({
  
  data <- filtered_data_envitmradar()
  
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
  
  p_envitmradar <- ggplot(data, aes(
    x = #env_itm,
    factor(env_itm, level = c("GHG", "land", "water", "eutr", "avg")),
    y = value
    #fill = env_itm
  )) +
    coord_polar() +
    theme_void() +
    geom_textpath(inherit.aes = FALSE,
                  # mapping = aes(x= factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  # label = factor(region, level = c("WLD", "HIC", "UMC", "LMC", "LIC")),
                  mapping = aes(x = env_itm,
                                label = env_itm,
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
    facet_wrap(~ region,
               ncol = 4,
               labeller = labeller(region = label_wrap_gen(width = 15))
               ) +
    lshtm_theme_few_radar()
})

output$plot_envitmradar <-
  renderPlot({
    validate(
      need(nrow(filtered_data_envitmradar()) >0, "The current input selection returns an empty plot.\nPlease change the input selection to display a valid plot.")
    )
    print(reactive_plot_env_itmradar())
  })

output$download_plot_envitmradar <- downloadHandler(
  filename = function() { paste("plot_envitmradar", '.png', sep='') },
  content = function(file) { ggsave(file, plot = reactive_plot_env_itmradar(),
                                    device = "png", width = 12) }
)

