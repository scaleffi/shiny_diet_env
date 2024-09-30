# Header ------------------------------------------------------------------
# Author: Sebastiano Caleffi


# Create custom themes ----------------------------------------------------
lshtm_theme_few <- function(){
  theme_few() +
    #%+replace%
    theme(
      #plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(
        vjust = -1,
        size = 14,
        #family = "serif",
        face = "plain"),
      axis.title.x.top = element_text(
        size = 14,
        vjust = 1,
        hjust = 0
      ),
      axis.title.y = element_text(size = 14,
                                  face = "plain",
                                  #family = "serif",
                                  #angle = 90
                                  vjust = 1.5
      ),
      strip.text.x = element_text(size = 12 
                                  #,face = "bold"
      ),
      panel.spacing = unit(0,"lines"),
      strip.text.y = element_text(size = 12
                                  , face = "bold"
      ),
      axis.text.y = element_text(size = 10.5),
      axis.text.x = element_text(size = 10.5,
                                 angle = 45,
                                 #vjust = 0.5,
                                 hjust = 1
      ),
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12
                                  #face = "bold"
      ),
      plot.title = element_text(#family = "serif",
        size = 22, 
        face = "bold",
        hjust = 0.5,
        vjust = 0.2
        ),
      plot.subtitle = element_text(
        size = 10, 
        face = "plain",
        vjust = 1,
        ),
      plot.caption = element_text(
        size = 12,
        face = "bold",
        hjust = 0
      ),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
      strip.placement = "outside"
    )
}

# Create a second version of the custom theme, for radar plots
lshtm_theme_few_radar <- function(){
  theme_few() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      strip.text.x = element_text(size = 12, face = "bold"),
      panel.spacing = unit(0,"lines"),
      strip.text.y = element_text(size = 12, face = "bold"),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.placement = "outside"
    )
}

# Assign colours to variables ---------------------------------------------
colors_macro <- c(
  "ASF" = "#922b21",
  "Staples" = "#f1c40f",
  "Other" = "#85929e"
)

colors_sex <- c(
  "FML" = "#E41A1C",
  "MLE" = "#377EB8",
  "BTH" = "#4DAF4A"
)

colors_urban <- c(
  "urban" = "#D95F02",
  "rural" = "#66A61E",
  "all-u" = "#f4d03f"
)

colors_sociodem <- c(
  "urban" = "#D95F02",
  "rural" = "#66A61E",
  "FML" = "#E41A1C",
  "MLE" = "#377EB8",
  "low" = "#fcf3cf",
  "medium" = "#f39c12",
  "high" = "#873600",
  "0-9" = "#ebdef0",
  "10-19" = "#a9cce3",
  "20-39" = "#48c9b0",
  "40-64" = "#229954",
  "65+"= "#7d6608"
)

colors_age <-
  c("0-9" = "#ebdef0",
    "10-19" = "#a9cce3",
    "20-39" = "#48c9b0",
    "40-64" = "#229954",
    "65+"= "#7d6608"
      )

colors_sociodem_category <- c(
  "Urb. level" = "#f4d03f",
  "Sex" = "#4DAF4A",
  "Edu. level" = "#f39c12",
  "Age" = "#48c9b0"
)

colors_food <- c(
  "beef" = "#cb4335",
  "lamb" = "#ec7063",
  "pork" = "#f5a5b5",
  "other meats" = "#d98880",
  "fish" = "#003366",
  "dairy" = "#66CCFF",
  "eggs&fats" = "#fae5d3",
  "other" = "#fdedec",
  "sugar" = "#bdc3c7",
  "oils" = "#7b7d7d",
  "nuts&seeds" = "#999933",
  "fruit&veg" = "#66CC00",
  "legumes" = "#66CC99",
  "roots" = "#eb984e",
  "rice" = "#f9e79f",
  "grains" = "#ecdb54",
  "total" = "#2c3e50"
  )


# Rename regions for clearer facet titles ---------------------------------
region.labs <-
  c(
    "World",
    "Low Income",
    "Lower Middle Income",
    "Upper Middle Income",
    "High Income",
    "East Asia and Pacific",
    "Europe & C. Asia",
    "Latin America & Caribbean",
    "Middle East and North Africa",
    "North America",
    "South Asia",
    "Sub-Saharan Africa"
  )
names(region.labs) <-
  c("WLD",
    "LIC",
    "LMC",
    "UMC",
    "HIC",
    "EAS",
    "ECS",
    "LCN",
    "MEA",
    "NAC",
    "SAS",
    "SSF"
  )


# Define ordering of variables in plots -----------------------------------
custom_order_region <-
  c("WLD",
    "HIC",
    "UMC",
    "LMC",
    "LIC",
    "ECS",
    "MEA",
    "EAS",
    "SAS",
    "NAC",
    "LCN",
    "SSF")

custom_order_category <-
  c("Sex",
    "Age",
    "Education",
    "Residence")

custom_labels_region <-
  c(
    "World",
    "High Income",
    "Upper Middle Income",
    "Lower Middle Income",
    "Low Income",
    "Europe & C. Asia",
    "Middle East & N. Africa",
    "E. Asia & Pacific",
    "South Asia",
    "North America",
    "Latin Am. & Caribbean",
    "Sub-Saharan Africa"
  )

custom_order_sociodem <- 
  c(
    "BTH",
    "MLE",
    "FML",
    "all-a",
    "65+",
    "40-64",
    "20-39",
    "10-19",
    "0-9",
    "all-e",
    "high",
    "medium",
    "low",
    "all-u",
    "rural",
    "urban"
  )

custom_order_foodgroup <- 
  c(
    "beef",
    "lamb",
    "pork",
    "other meats",
    "fish",
    "dairy",  
    "eggs&fats",
    "other",
    "sugar",
    "oils",
    "nuts&seeds",
    "fruit&veg",
    "legumes",
    "roots",
    "rice",
    "grains",
    "total"
  )

custom_order_edu <- 
  c(
    "high",
    "medium",
    "low",
    "all-u"
  )

custom_order_urban <-
  c(
    "all-u",
    "rural",
    "urban"
  )

custom_order_sex <-
  c(
    "BTH",
    "MLE",
    "FML"
  )