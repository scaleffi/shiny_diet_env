#This file is used to collect code that is not part of the application at the moment, but could be useful in the future

#library(shinythemes)
#library(periscope)
#library(patchwork)
#library(thematic)
#library(gghighlight)

#logdebug("log", logger = "ss_userAction")


# #Control shading by setting alpha values through a new vector named alpha_vals; changing the range of shading helps with displaying some images that have several colors.
# alpha_max <- 1
# alpha_min <- 0.7
# alpha_vals <- c(
#   seq(alpha_max, alpha_min, length.out = 8), 
#   seq(alpha_min, alpha_max, length.out = 8)[-1]
# )
# alpha_vals
# 

# env_itm == "GHG" ~ "GHG (kt CO2eq)",
# env_itm == "water" ~ "water use (km3)",
# env_itm == "land" ~ "land use (thousands of km2)",
# env_itm == "land_crop" ~ "Land use,crops (thousands of km2)",
# env_itm == "land_pstr" ~ "Land use,pasture (thousands of km2)",
# env_itm == "eutr" ~ "eutrophication pot. (kt PO4eq)",


#%>% spread(key = region, value = value) %>%
# select(-c(measure, env_itm, dmd_scn, food_group,box,age.education,sex.urbanisation))



#THIS SECTION OF CODE works to generate individual spider plots using the fmsb package. Because it runs
#outside of ggplot2, it does not allow for faceting or saving using the existing code for the rest of the
#script

#     # Extracting the data from reshaped_data

#radar_data <- as.data.frame(data)
#print(radar_data)
# # # Add rows for max and min values

#max_values <- rep(150, ncol(radar_data))
#min_values <- rep(0, ncol(radar_data))

# # Bind the new max/min rows to the data - the radarchart() function needs this to work properly

#radar_data_maxmin <- rbind(min_values, max_values, radar_data)
# # Check the data is ok

#print(radar_data_maxmin)
#
# # Create radar plot - WARNING: because this is not going through ggplot2, the resulting plots can't
# # be faceted, or easily customised using the same settings as in ggplot2
#p_regionradar <- radarchart(radar_data_maxmin, maxmin = TRUE, title = "Avg env footprint per capita\n(compared to WLD average)")
#
#return(p_regionradar)



# p_regionradar <- radarchart(reshaped_data, maxmin = FALSE
# )
#  print(reshaped_data)
# Print the result
# print("Reshaped Data:")
# print(reshaped_data)