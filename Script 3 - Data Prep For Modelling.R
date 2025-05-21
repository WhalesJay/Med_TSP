#This script first collates temperature data by beach, time matches this to the nichemapr
#data then creates data to input models to interpolate / extrapolate nest temperature. 

#Load data
library(tidyr)
rm(list=ls())
load("Section2_Output.RData")


#Load required packages
library(dplyr)
library(reshape2)
library(lubridate)
library(mgcv)
library(ggplot2)
library(zoo)

#There are some issues with data from Patara, I am unsure if this applies to other
#beaches, so I am going to do some filtering to ensure these errors aren't
#Carried through

#First I am going to filter data so that temperatures beneath 11 degrees are deleted 
#These data points in Patara are scattered throughout and are clear outliers

data_2018[data_2018 < 11] = NA
data_2019[data_2019 < 11] = NA

#While this removed most values I am going to run a rolling average to make sure 
#any other outliers are removed. I will set temperature changes of more than 5 
#degrees celcius within a day to be removed (this is probably overly coarse, but
#I don't want to erroneously remove data with potentially large day-night temperature
#deviations)
data_2018 <- data_2018 %>%
  mutate(across(where(is.numeric), ~{
    rolling_median <- rollapply(.x, width = 25, FUN = median, fill = NA, align = "center", na.rm = TRUE)
    .x[abs(.x - rolling_median) > 5] <- NA  # Remove outliers
    .x
  }))

data_2019 <- data_2019 %>%
  mutate(across(where(is.numeric), ~{
    rolling_median <- rollapply(.x, width = 25, FUN = median, fill = NA, align = "center", na.rm = TRUE)
    .x[abs(.x - rolling_median) > 5] <- NA  # Remove outliers
    .x
  }))

# Function to compute row averages by grouping columns based on meta$Beach
average_by_beach <- function(data, meta) {
  # Extract the datetime column
  datetime_column <- data[, 1]
  
  # Initialize data frames to store results for Sand and Nest
  result_sand <- data.frame(datetime = datetime_column)
  result_nest <- data.frame(datetime = datetime_column)
  
  # Update the 'Code..as.received.' column names in meta to have valid names
  meta$Code..as.received. <- gsub("[ ()-]", ".", meta$Code..as.received.)
  
  # Loop through each unique Beach value
  for (beach in unique(meta$Beach)) {
    # Find columns in data that correspond to this beach
    matching_columns <- meta$Code..as.received.[meta$Beach == beach]
    
    # Separate columns into "Sand" and "Nest" categories based on meta$Sand.or.Nest
    sand_columns <- matching_columns[meta$Sand.or.Nest[meta$Beach == beach] == "Sand"]
    nest_columns <- matching_columns[meta$Sand.or.Nest[meta$Beach == beach] == "Nest"]
    
    # Print the columns being processed for the current beach
    cat("Processing Beach:", beach, "\n")
    cat("Sand columns:", sand_columns, "\n")
    cat("Nest columns:", nest_columns, "\n")
    
    # Ensure the matching columns exist in the data
    existing_sand_columns <- intersect(sand_columns, colnames(data))
    existing_nest_columns <- intersect(nest_columns, colnames(data))
    
    if (length(existing_sand_columns) > 0) {
      # Convert to numeric and calculate row-wise averages for the Sand group
      group_values_sand <- as.data.frame(lapply(data[, existing_sand_columns, drop = FALSE], as.numeric))
      result_sand[[beach]] <- rowMeans(group_values_sand, na.rm = TRUE)
    } else {
      cat("No Sand columns found for Beach:", beach, "\n")
    }
    
    if (length(existing_nest_columns) > 0) {
      # Convert to numeric and calculate row-wise averages for the Nest group
      group_values_nest <- as.data.frame(lapply(data[, existing_nest_columns, drop = FALSE], as.numeric))
      result_nest[[beach]] <- rowMeans(group_values_nest, na.rm = TRUE)
    } else {
      cat("No Nest columns found for Beach:", beach, "\n")
    }
  }
  
  return(list(Sand = result_sand, Nest = result_nest))
}

averaged_2018 = average_by_beach(data_2018, meta_2018)
averaged_2018_sand = averaged_2018$Sand
averaged_2018_nest = averaged_2018$Nest


averaged_2019 = average_by_beach(data_2019, meta_2019)
averaged_2019_sand = averaged_2019$Sand
averaged_2019_nest = averaged_2019$Nest


# Combine and melt the data

# Convert 'datetime' columns to POSIXct
allsoil$datetime <- as.POSIXct(allsoil$datetime, format = "%Y-%m-%d %H:%M:%S")
# Convert 'datetime' to POSIXct with the appropriate format
averaged_2018_sand$datetime <- as.POSIXct(averaged_2018_sand$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
averaged_2018_nest$datetime <- as.POSIXct(averaged_2018_nest$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
averaged_2019_sand$datetime <- as.POSIXct(averaged_2019_sand$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")
averaged_2019_nest$datetime <- as.POSIXct(averaged_2019_nest$datetime, format = "%Y-%m-%d %H:%M", tz = "UTC")



# Round 'datetime' to the nearest hour in 'averaged_2018' and 'averaged_2019'
averaged_2018_nest$datetime = floor_date(averaged_2018_nest$datetime, "hour")
averaged_2018_sand$datetime = floor_date(averaged_2018_sand$datetime, "hour")
averaged_2019_sand$datetime = floor_date(averaged_2019_sand$datetime, "hour")
averaged_2019_nest$datetime = floor_date(averaged_2019_nest$datetime, "hour")


# Melt 'allsoil' dataframe
melted_allsoil = melt(allsoil, id.vars = "datetime", variable.name = "Beach", value.name = "Soil")


# Melt 'averaged_2018' dataframe and combine with the corresponding soil data
melted_averaged_2018_sand = melt(averaged_2018_sand, id.vars = "datetime", variable.name = "Beach", value.name = "Nest")
melted_averaged_2018_sand$Beach <- paste0(melted_averaged_2018_sand$Beach, "_2018")
melted_averaged_2018_sand$Loc = "Sand"
model_data_2018_sand = merge(melted_averaged_2018_sand, melted_allsoil, by = c("datetime", "Beach"))

melted_averaged_2018_nest = melt(averaged_2018_nest, id.vars = "datetime", variable.name = "Beach", value.name = "Nest")
melted_averaged_2018_nest$Beach <- paste0(melted_averaged_2018_nest$Beach, "_2018")
melted_averaged_2018_nest$Loc = "Nest"
model_data_2018_nest = merge(melted_averaged_2018_nest, melted_allsoil, by = c("datetime", "Beach"))



# Melt 'averaged_2019' dataframe and combine with the corresponding soil data
melted_averaged_2019_sand = melt(averaged_2019_sand, id.vars = "datetime", variable.name = "Beach", value.name = "Nest")
melted_averaged_2019_sand$Beach <- paste0(melted_averaged_2019_sand$Beach, "_2019")
melted_averaged_2019_sand$Loc = "Sand"
model_data_2019_sand = merge(melted_averaged_2019_sand, melted_allsoil, by = c("datetime", "Beach"))

melted_averaged_2019_nest = melt(averaged_2019_nest, id.vars = "datetime", variable.name = "Beach", value.name = "Nest")
melted_averaged_2019_nest$Beach <- paste0(melted_averaged_2019_nest$Beach, "_2019")
melted_averaged_2019_nest$Loc = "Nest"
model_data_2019_nest = merge(melted_averaged_2019_nest, melted_allsoil, by = c("datetime", "Beach"))

#This is a cheap workaround so I could use allsoil as a single dataframe but the 2019 
#beaches are listed as xx/xx/2018 so I need to subtract a year for the matching
melted_averaged_2019_sand$datetime = melted_averaged_2019_sand$datetime - years(1)
model_data_2019_sand = merge(melted_averaged_2019_sand, melted_allsoil, by = c("datetime", "Beach"))

#Now add the year back on 
melted_averaged_2019_sand$datetime = melted_averaged_2019_sand$datetime + years(1)

#Nest
melted_averaged_2019_nest$datetime = melted_averaged_2019_nest$datetime - years(1)
model_data_2019_nest = merge(melted_averaged_2019_nest, melted_allsoil, by = c("datetime", "Beach"))

#Now add the year back on 
melted_averaged_2019_nest$datetime = melted_averaged_2019_nest$datetime + years(1)

##########
#Now combine the two, I'm just manually appending the 2019 data to the bottom of the 
#2018 data because the fancy ways of doing this require more memory than my PC has available
model_data_sand = model_data_2018_sand
x = nrow(model_data_2018_sand) +1
y = x + nrow(model_data_2019_sand) -1 
model_data_sand[x:y, ] = model_data_2019_sand


model_data_nest = model_data_2018_nest
x = nrow(model_data_2018_nest) +1
y = x + nrow(model_data_2019_nest) -1 
model_data_nest[x:y, ] = model_data_2019_nest

#Clear the junk
objects_to_keep <- c("model_data_nest", "model_data_sand","durationmodel")

# Remove all objects except the ones to keep
rm(list = setdiff(ls(), objects_to_keep))


save.image(file = "Section3_Output.RData")
