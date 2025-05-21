#Section 2 - Metabolic Heating and Duration Modelling 

rm(list=ls())
load("Section1_Output.RData")



#This script builds a model to calcluate nesting duration based on temperature 
#As well as metabolic heating based on nesting and sand temperature data
#Load the required packages
library(mgcv)
library(rsq)
library(ggplot2)

# Function to add time series data and assign 'timein' based on the first non-NA value in the time series
nest_duration = read.csv("nest_durations.csv")
data_2018[data_2018 == ""] <- NA
data_2019[data_2019 == ""] <- NA
#I don't know why, but three of the names are changing at some point so I am restting 
#Them to correct
nest_duration$Code.of.loggers[3] = "Rethymno.B06L..Greece.05."
nest_duration$Code.of.loggers[12] = "Kyparıssıa.Bay.An009TL..Greece.01."


add_time_series <- function(nest_duration, data_2018, data_2019) {
  # Replace spaces, hyphens, and parentheses with dots to match column names
  logger_codes <- gsub("[ ()-]", ".", nest_duration$Code.of.loggers)
  
  # Initialize lists to track successful and unsuccessful matches
  found_loggers <- c()
  not_found_loggers <- c()
  
  # Initialize lists to store the cleaned time series vectors and 'timein' values
  time_series_list <- vector("list", nrow(nest_duration))
  timein_list <- vector("list", nrow(nest_duration))
  
  # Loop through each row in nest_duration
  for (i in seq_len(nrow(nest_duration))) {
    # Get the logger code with dots
    logger_code <- logger_codes[i]
    
    # Initialize placeholders
    time_series <- NULL
    timein <- NA
    
    # Check if the logger code exists as a column in data_2018
    if (logger_code %in% colnames(data_2018)) {
      time_series <- data_2018[[logger_code]]
      
      # Find the first row with a non-NA value in the time series column
      first_non_na_row <- which(!is.na(time_series))[1]
      if (!is.na(first_non_na_row)) {
        # Get the value from column 1 (time column) for that row
        timein <- data_2018[[1]][first_non_na_row]
      }
      
      found_loggers <- c(found_loggers, logger_code)
    } 
    # Otherwise, check in data_2019
    else if (logger_code %in% colnames(data_2019)) {
      time_series <- data_2019[[logger_code]]
      
      # Find the first row with a non-NA value in the time series column
      first_non_na_row <- which(!is.na(time_series))[1]
      if (!is.na(first_non_na_row)) {
        # Get the value from column 1 (time column) for that row
        timein <- data_2019[[1]][first_non_na_row]
      }
      
      found_loggers <- c(found_loggers, logger_code)
    } 
    # If not found, add to the not_found_loggers list
    else {
      not_found_loggers <- c(not_found_loggers, logger_code)
    }
    
    # Remove NA values from the time series if not NULL
    if (!is.null(time_series)) {
      time_series <- na.omit(time_series)
    }
    
    # Add the cleaned time series and timein values to their respective lists
    time_series_list[[i]] <- time_series
    timein_list[[i]] <- timein
  }
  
  # Add the time series and timein lists as new columns to nest_duration
  nest_duration$TimeSeries <- time_series_list
  nest_duration$timein <- unlist(timein_list) # Flatten the list to a vector
  
  # Print diagnostics
  cat("Successfully found logger codes:\n")
  print(found_loggers)
  
  cat("\nLogger codes not found:\n")
  print(not_found_loggers)
  
  return(nest_duration)
}


# Application of function
nest_duration <- add_time_series(nest_duration, data_2018, data_2019)


# Add the length of each time series vector to a new column 'duration'
nest_duration$duration <- sapply(nest_duration$TimeSeries, length)

#Create nesting as a function of percent complete 
# Create a percentage vector for each row based on 'duration', skipping NA or 0 values
nest_duration$percent <- sapply(nest_duration$duration, function(d) {
  if (is.na(d) || d == 0) {
    return(NA)  # or return an empty vector, e.g., return(c())
  } else {
    return(seq(from = 100 / d, to = 100, by = 100 / d))
  }
})

# Scale each TimeSeries vector to percentages (ignoring 0 and NA values)
nest_duration$scaled_TimeSeries <- lapply(nest_duration$TimeSeries, function(ts) {
  # Ensure ts is numeric
  if (!is.numeric(ts)) {
    ts <- as.numeric(ts)
  }
  
  # Remove NA and 0 values before scaling
  ts_clean <- ts[!is.na(ts) & ts != 0]  # Remove NA and 0 values
  
  if (length(ts_clean) > 0) {
    # Scale by the max of cleaned series
    return(100 * ts_clean / max(ts_clean, na.rm = TRUE))
  } else {
    # If the vector is empty (all values were NA or 0), return the original vector
    return(ts)
  }
})


# Determine the maximum length of the TimeSeries
max_length <- max(sapply(nest_duration$scaled_TimeSeries, length), na.rm = TRUE)

# Now pad all TimeSeries vectors to match the maximum length
TimeSeries_matrix <- do.call(rbind, lapply(nest_duration$scaled_TimeSeries, function(x) {
  # If the length of x is less than the maximum, pad it with NAs
  if (length(x) < max_length) {
    return(c(x, rep(NA, max_length - length(x))))  # Fill with NAs
  } else {
    return(x)  # Return the vector as it is if it already has max length
  }
}))

# Cap the maximum length of the TimeSeries to 720 hours, or 30 days, which is 
#on the short side of incubation period, so we can use the average temperature of 
#this period to determine exactly how long the incubation will be 
max_length <- 720

nest_duration$durationmod <- lapply(nest_duration$TimeSeries, function(ts) {
  if (length(ts) > max_length) {
    return(ts[1:max_length])  # Trim if longer than 960
  } else if (length(ts) < max_length) {
    return(c(ts, rep(NA, max_length - length(ts))))  # Pad with NA if shorter
  } else {
    return(ts)  # No change if already 960 elements
  }
})

#Take the average temperature 
# Calculate the average of each vector in the list and assign to 'avgtemp'
nest_duration$avgtemp <- sapply(nest_duration$durationmod, function(x) mean(x, na.rm = TRUE))

#Create a model
durationmodel <- glm(incubation.period ~ avgtemp, data = nest_duration)

#Make a nice plot to visualise
ggplot(nest_duration, aes(x = avgtemp, y = incubation.period)) +
  geom_point(alpha = 0.5) +  # Add scatter plot of observed data
  geom_smooth(method = "glm", formula = y ~ x, color = "blue", size = 1, se = TRUE) +  # Fit the GAM model
  labs(x = "Average Temperature (°C) in first 40 days", y = "Incubation Period (Days)")+
  theme_classic()

#Summary stats for model
summary(durationmodel)
rsq(durationmodel)
#R sq 0.2283287, p>0.001
#Clear the junk
objects_to_keep <- c("allsoil", "data_2018", "data_2019", "meta_2018", "meta_2019", "nest_duration", "durationmodel")

# Remove all objects except the ones to keep
rm(list = setdiff(ls(), objects_to_keep))

save.image(file = "Section2_Output.RData")

