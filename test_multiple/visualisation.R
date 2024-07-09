#Prepare graphic objects and labels that will be used to create the plots below ----

#Create custom theme as a function, which can be applied to each plot without having to manually edit each of them to
#obtain the required look

lshtm_theme_few <- function(){
  theme_few() +
    #%+replace%
    theme(
      #plot.margin = margin(0,0,0,0),
      axis.title.x = element_text(
        vjust = -0.5,
        size = 16,
        #family = "serif",
        face = "bold"),
      axis.title.y = element_text(size = 16,
                                  face = "bold",
                                  #family = "serif",
                                  #angle = 90
                                  vjust = 1.5
      ),
      strip.text.x = element_text(size = 14 
                                  #,face = "bold"
      ),
      panel.spacing = unit(0,"lines"),
      strip.text.y = element_text(size = 14
                                  , face = "bold"
      ),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12,
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
        vjust = 1),
      plot.subtitle = element_text(size = 14, face = "bold"),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted"),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
      strip.placement = "inside"
    )
}

#Create a second version of the custom theme, for radar plots. This type of plots require some changes that would be 
#too time consuming to manually add to each plot
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

#create a vector named 'colors_macro' made up of three colors from the Set1 ColorBrewer palette. This can be used to assign specific colors to values in the macrofoods variable.
colors_macro <- c(
  "ASF" = "#922b21",
  "Staples" = "#f1c40f",
  "Other" = "#85929e"
)

#create vectors to assign colors to the sociodem characteristics
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

colors_sociodem_category <- c(
  "Urb. level" = "#f4d03f",
  "Sex" = "#4DAF4A",
  "Edu. level" = "#f39c12",
  "Age" = "#48c9b0"
)

#Create a vector with specific color assigned to each food group
#Based on _trs_110423
colors_food <- c(
  "total" = "#a6a79b",
  "rice" = "#f9e79f",
  "roots" = "#eb984e",
  "sugar" = "#fad7a0",
  "legumes" = "#6e2c00",
  "beef" = "#cb4335",
  "lamb" = "#ec7063",
  "othr_meat" = "#d98880",
  "pork" = "#f5a5b5",
  "othr_ani" = "#fae5d3",
  "other" = "#fdedec",
  "dairy" = "#f0ebe2",
  "fish" = "#8fbad3",
  "grains" = "#ecdb54",
  "fruit_veg" = "#229954",
  "nuts_seeds" = "#7d6608",
  "oils" = "#abebc6")

# Create a vector to rename facet plots with the full names of the environmental dimensions
env_itm.labs <- c("GHG (Mt CO2eq)", "Freshwater use (km3)", "Eutrophication pot. (Mt PO4eq)", "land use (thousands of km2)", "Land use_pasture (thousands of km2)", "Land use_crops (thousands of km2)")
names(env_itm.labs) <- c("GHG (Mt CO2eq)", "water use (km3)", "eutrophication pot. (kt PO4eq)", "land use (thousands of km2)", "land use, pasture (thousands of km2)", "land use, crops (thousands of km2)")

# Create a vector to rename facet plots with the full names of the regions
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