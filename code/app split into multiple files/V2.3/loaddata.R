library(tidyverse)

#######-------------------#########-------------------------###########
#Load data files
#######-------------------#########-------------------------###########


#Load files from Github repository
# csv_file_trs <- "data/report_env_trs_053123.csv"
# csv_file_box <- "data/report_env_box_060123.csv" 
# csv_file_cons <- "data/cons_compare_012823.csv"
# csv_file_FBSintake <- "data/FBS_intake_socio_all-a_051523.csv"

csv_file_trs <- "/Users/lshsc40/Documents/GitHub/shiny_diet_env/data/report_env_trs_053123.csv"
csv_file_box <- "/Users/lshsc40/Documents/GitHub/shiny_diet_env/data/report_env_box_060123.csv" 
csv_file_cons <- "/Users/lshsc40/Documents/GitHub/shiny_diet_env/data/cons_compare_012823.csv"
csv_file_FBSintake <- "/Users/lshsc40/Documents/GitHub/shiny_diet_env/data/FBS_intake_socio_all-a_051523.csv"

  data_box <- read.csv(csv_file_box)
  data_box$value <- round(data_box$value, 2)
  df <- data_box 

  data_trs <- read.csv(csv_file_trs)
  data_trs$value <- round(data_trs$value, 2)
  df_trs <- data_trs 
  
    # Create a new dataset, data_trs_category, by adding a column labelled 'category' to the _trs dataset, to group different labels in the variable age/sociodem to subgroups (if useful)
    data_trs_category <- data_trs %>%
      mutate(category = case_when(
        age %in% c("FML", "MLE", "BTH") ~ "Sex",
        age %in% c("low", "medium", "high", "all-e") ~ "Edu. level",
        age %in% c("rural", "urban", "all-u") ~ "Urb. level",
        age %in% c("0-10", "11-19", "20-39", "40-64", "65+", "all-a") ~ "Age group"
      ))
    
    #Create another dataset, data_trs_macrofoods by adding to data_trs_category a column labelled 'macrofoods', to group different labels in the food_group variable to subgroups (if useful)
    # namely ASF, Staples, Other, Total. This dataset includes a column for the category, and a column for the macrofoods.
    data_trs_macrofoods <- data_trs_category %>%
      mutate(macrofoods = case_when(
        food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
        food_group %in% c("rice", "grains") ~ "Staples",
        food_group %in% c("fruit_veg", "oils", "sugar", "roots", "legumes", "nuts_seeds") ~ "Other",
        food_group %in% c("total") ~ "Total"
      ))
    
    #Create a third dataset, df_trs_macrof, by adding a column labelled 'macrofoods' to the main df dataframe. This dataset only has one additional column, to group macrofoods, if the user is not
    #interested in grouping sociodem categories
    df_trs_macrof <- df_trs %>%
      mutate(macrofoods = case_when(
        food_group %in% c("beef", "milk", "lamb", "pork", "poultry", "eggs", "fish") ~ "ASF",
        food_group %in% c("rice", "grains", "roots") ~ "Staples",
        food_group %in% c("fruit_veg", "oils", "sugar", "legumes", "nuts_seeds") ~ "Other",
        food_group %in% c("total") ~ "Total"
      ))
    
    df_trs_category <- data_trs_category
  
    data_cons <- read.csv(csv_file_cons)
    data_cons$Intake <- round(data_cons$Intake, 2)
    df_cons <- data_cons[-7]
    
    data_FBSintake <- read.csv(csv_file_FBSintake)
    data_FBSintake$Value <- round(data_FBSintake$Value, 2)
    df_FBSintake <- data_FBSintake