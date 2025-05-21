#This script and those which follow are for the various prediction windows. 

#mid Predictions
#First add the required temperature increase for the relevant period
#Nest
extrap_data_mid = extrap_data
extrap_data_mid$Soil = extrap_data_mid$Soil + 2.1

svm_extrap_mid = extrap_data_mid[, c("Soil", "yday", "locn")]
extrap_data_mid$fit_new = predict(svm_model_4, newdata = svm_extrap_mid)

#Sand
extrap_sand_mid = extrap_sand
extrap_sand_mid$Soil = extrap_sand_mid$Soil + 2.1
svm_sand_mid = extrap_sand_mid[, c("Soil", "yday", "locn")]
extrap_sand_mid$fit_new = predict(svm_model_s, newdata = svm_sand_mid)

#Save each of our models into a wide dataframe so that the predicting loop can draw from it logically
#Nest loggers
wide_predict_mid = extrap_data_mid[,c(1, 2, 9)]
colnames(wide_predict_mid) = c("datetime", "beach", "predict")
wide_predict_mid = pivot_wider(wide_predict_mid, 
                                names_from = beach, 
                                values_from = predict)

#Sand
wide_predict_mid_s = extrap_sand_mid[,c(1,2,8)]
colnames(wide_predict_mid_s) = c("datetime", "beach", "predict")
wide_predict_mid_s = pivot_wider(wide_predict_mid_s, 
                                  names_from = beach, 
                                  values_from = predict)

#Split by days, note that all the dataloggers record in local time which means there 
#is an hour +/- but given that nesting is generally only at night it seems relevant to maintain
#relative (local) time 
daybreaks = seq(by = 24, from = 1, to = nrow(wide_predict))
daybreaks_s = seq(by = 24, from = 1, to = nrow(wide_predict_s))

#Lets assume all our hypothetical nests were laid at midnight
#How many days we got?

#Number of days is the same for sand and nest so I only need to do this once

ndays = yday(wide_predict$datetime[4391])- yday(wide_predict$datetime[1]) - 65 #This will give us enough wiggle room to ensure all nests are ended by september 30th
ndays = seq(from = 1, to = ndays, by = 1)


#set up incubation durations for Reproducability
set.seed(4971)

#Save wide predict as a matrix to speed up processing 
wide_predict_matrix_mid = as.matrix(wide_predict_mid[,-1])
wide_predict_matrix_s_mid = as.matrix(wide_predict_mid_s[,-1])

#Prediction loop for nest temperature 
names = colnames(wide_predict_matrix_mid)
#Middle third proportions
u = 33.333
v = 66.666

tosave_mid = as.data.frame(matrix(nrow = 100, ncol = (128*length(beach_names))))

for (i in 1:128){ #Sets number of days
  f = length(beach_names)
  columnsaver = ((i-1)*f) #length(names) not working
  
  for(l in 1:100){ #sets number of replicates
    
    for( j in 1:ncol(wide_predict_matrix_mid)){ #Setting up all beaches
      #dur = (i * 131) + (l * 100) + (j * 22)
      repeat {
        dayzero = ((i-1)*24)+1 #sets day zero to be midnight of first day
        day30 = dayzero + (24*30)
        first30days = as.data.frame(mean(wide_predict_matrix_mid[(dayzero:day30), j]))
        colnames(first30days) = "avgtemp"
        incubation_duration = predict(durationmodel, first30days) + rnorm(nrow(first30days), mean = 0, sd = sd(residuals(durationmodel)))
        #Ensures (some degree of) randomness when estimating incubation duration 
        dayend = ceiling(dayzero + (incubation_duration * 24))
        
        # Check if dayend is larger than 4392
        if (dayend <= 4392) {
          break  # Exit loop if condition is met
        }
      }
      heated = as.data.frame(wide_predict_matrix_mid[dayzero:dayend,j])
      colnames(heated) = "inctemp"
      for(k in 1:nrow(heated)){ #Running per beach
        #  heated$temp_dev[k] = NA
        heated$percent[k] = (k/nrow(heated))*100
        heated$ID[k] = (paste(names[j], "_day_", i, sep = ""))
      }
      
      #temp_predict = predict(metabolic_heat_gamm$gam, heated, type = "response", se.fit = TRUE)
      #heated$inctemp = (heated[,1] + temp_predict$fit)
      heated$percent = as.numeric(heated$percent)
      mean_col2 = heated[heated$percent >= u & heated$percent <= v, ]
      mean_col2 = as.data.frame(mean_col2)
      beach_avg = sum(mean_col2$inctemp)/nrow(mean_col2)
      
      tosave_mid[l,columnsaver+(j)] = beach_avg
      colnames(tosave_mid)[columnsaver+(j)] = paste(heated$ID[k])
    }
  }
  
  print(paste("Day", i, "of 128 days done at",Sys.time(), sep = " "))
}

#Sand
names = colnames(wide_predict_matrix_s_mid)
tosave_s_mid = as.data.frame(matrix(nrow = 100, ncol = (128*length(beach_names_s))))
set.seed(442)
for (i in 1:128){ #Sets number of days
  f = length(beach_names_s)
  columnsaver = ((i-1)*f) #length(names) not working
  
  for(l in 1:100){ #sets number of replicates
    
    for( j in 1:ncol(wide_predict_matrix_s_mid)){ #Setting up all beaches
      #dur = (i * 131) + (l * 100) + (j * 22)
      repeat {
        dayzero = ((i-1)*24)+1
        day30 = dayzero + (24*30)
        first30days = as.data.frame(mean(wide_predict_matrix_s_mid[(dayzero:day30), j]))
        colnames(first30days) = "avgtemp"
        incubation_duration = predict(durationmodel, first30days) + rnorm(nrow(first30days), mean = 0, sd = sd(residuals(durationmodel)))
        
        dayend = ceiling(dayzero + (incubation_duration * 24))
        
        # Check if dayend is larger than 4392
        if (dayend <= 4392) {
          break  # Exit loop if condition is met
        }
      }
      heated = as.data.frame(wide_predict_matrix_s_mid[dayzero:dayend,j])
      colnames(heated) = "inctemp"
      for(k in 1:nrow(heated)){ #Running per beach
        #  heated$temp_dev[k] = NA
        heated$percent[k] = (k/nrow(heated))*100
        heated$ID[k] = (paste(names[j], "_day_", i, sep = ""))
      }
      
      #temp_predict = predict(metabolic_heat_gamm$gam, heated, type = "response", se.fit = TRUE)
      #heated$inctemp = (heated[,1] + temp_predict$fit)
      heated$percent = as.numeric(heated$percent)
      mean_col2 = heated[heated$percent >= u & heated$percent <= v, ]
      mean_col2 = as.data.frame(mean_col2)
      beach_avg = sum(mean_col2$inctemp)/nrow(mean_col2)
      
      tosave_s_mid[l,columnsaver+(j)] = beach_avg
      colnames(tosave_s_mid)[columnsaver+(j)] = paste(heated$ID[k])
    }
  }
  
  print(paste("Day", i, "of 128 days done at",Sys.time(), sep = " "))
}


# Convert the `tosave` matrix into a data frame for plotting
tosave_df_mid <- as.data.frame(tosave_mid)
colnames(tosave_df_mid) <- colnames(tosave_mid) # Ensure column names are correctly set

tosave_df_s_mid <- as.data.frame(tosave_s_mid)
colnames(tosave_df_s_mid) <- colnames(tosave_s_mid) # Ensure column names are correctly set


# Extract unique beach names from column names
colnames(tosave_df_mid) <- gsub(" ", "_", colnames(tosave_df_mid))
beach_names <- unique(substr(colnames(tosave_df_mid[1:19]), 1, nchar(colnames(tosave_df_mid)) - 6))

colnames(tosave_df_s_mid) <- gsub(" ", "_", colnames(tosave_df_s_mid))
beach_names_s_mid <- unique(substr(colnames(tosave_df_s_mid[1:19]), 1, nchar(colnames(tosave_df_s_mid)) - 6))

#Make plots and save the data in a format to use later
#Nest
# Initialize a list to store plots for each beach
plots <- list()

for (beach in beach_names) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tosave_df_mid))
  beach_data <- tosave_df_mid[, beach_columns, drop = FALSE]
  
  # Melt data into long format for ggplot2
  melted_data <- data.frame(
    Day = (1:128) # Repeat days for each replicate
  )
  # Extract the temperature data for the current beach
  beach_columns <- grep(paste0("^", beach), colnames(beach_data))
  beach_temp <- as.vector(beach_data[, beach_columns])
  for (day in 1:128) {
    # Add the temperature data to melted_data
    melted_data$Temperature[day] <- mean(unlist(beach_temp[day]))
    melted_data$se[day] <- sd(unlist(beach_temp[day]))
  }
  
  
  
  #  Plot the line graph with error margins
  p = ggplot(melted_data, aes(x = Day, y = Temperature)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_ribbon(aes(ymin = Temperature - se, ymax = Temperature + se), 
                alpha = 0.2, fill = "blue") +  # Error margins as shaded area
    labs(title = paste("mid Term Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  
  
  # Store the plot in the list
  plots[[beach]] <- p
}

#Save and display plots
for (beach in names(plots)) {
  print(plots[[beach]])  # Display plot
  ggsave(paste0(beach, "_nest_mid_projection_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}

#Sand
# Initialize a list to store plots for each beach
plots_s <- list()

for (beach in beach_names_s) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tosave_df_s_mid))
  beach_data <- tosave_df_s_mid[, beach_columns, drop = FALSE]
  
  # Melt data into long format for ggplot2
  # Melt data into long format for ggplot2
  melted_data <- data.frame(
    Day = (1:128) # Repeat days for each replicate
  )
  # Extract the temperature data for the current beach
  beach_columns <- grep(paste0("^", beach), colnames(beach_data))
  beach_temp <- as.vector(beach_data[, beach_columns])
  for (day in 1:128) {
    # Add the temperature data to melted_data
    melted_data$Temperature[day] <- mean(unlist(beach_temp[day]))
    melted_data$se[day] <- sd(unlist(beach_temp[day]))
  }
  
  
  
  #  Plot the line graph with error margins
  p = ggplot(melted_data, aes(x = Day, y = Temperature)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_ribbon(aes(ymin = Temperature - se, ymax = Temperature + se), 
                alpha = 0.2, fill = "blue") +  # Error margins as shaded area
    labs(title = paste("mid Term Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  # Store the plot in the list
  plots_s[[beach]] <- p
}

#Save and display plots
for (beach in names(plots_s)) {
  print(plots_s[[beach]])  # Display plot
  ggsave(paste0(beach, "sand_mid_projection_plot.jpeg"), plot = plots_s[[beach]])  # Save plot as PNG
}
#Calculate the per day sex ratio using the Hill Equation

p = 29
s = -0.0336281
Hill_K = 0.1  # Renamed K to Hill_K
# Iterate over the rows and columns of tosave_df
#nest
tocalc_mid = tosave_df_mid
for (i in 1:nrow(tocalc_mid)) {
  for (j in 1:ncol(tocalc_mid)) {
    input_val <- tocalc_mid[i, j]
    
    q <- exp((1 / s) * (log(p + Hill_K) - log(input_val + Hill_K)))
    sr <- 1 / (1 + q)
    
    # Overwrite the value in tosave_df
    tocalc_mid[i, j] <- sr
  }
  print(paste0(i, "% done!"))
}

#sand
tocalc_s_mid = tosave_df_s_mid
for (i in 1:nrow(tocalc_s_mid)) {
  for (j in 1:ncol(tocalc_s_mid)) {
    input_val <- tocalc_s_mid[i, j]
    
    q <- exp((1 / s) * (log(p + Hill_K) - log(input_val + Hill_K)))
    sr <- 1 / (1 + q)
    
    # Overwrite the value in tosave_df
    tocalc_s_mid[i, j] <- sr
  }
  print(paste0(i, "% done!"))
}


#Make plots and save the data in a format to use later
#Nest
# Initialize a list to store plots for each beach
plots <- list()

for (beach in beach_names) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tocalc_mid))
  beach_data <- tocalc_mid[, beach_columns, drop = FALSE]
  
  # Melt data into long format for ggplot2
  melted_data <- data.frame(
    Day = (1:128) # Repeat days for each replicate
  )
  # Extract the temperature data for the current beach
  beach_columns <- grep(paste0("^", beach), colnames(beach_data))
  beach_temp <- as.vector(beach_data[, beach_columns])
  for (day in 1:128) {
    # Add the temperature data to melted_data
    melted_data$sex[day] <- mean(unlist(beach_temp[day]))
    melted_data$se[day] <- sd(unlist(beach_temp[day]))
  }
  
  
  
  #  Plot the line graph with error margins
  p = ggplot(melted_data, aes(x = Day, y = sex)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_ribbon(aes(ymin = sex - se, ymax = sex + se), 
                alpha = 0.2, fill = "blue") +  # Error margins as shaded area
    labs(title = paste("mid Term Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  
  
  # Store the plot in the list
  plots[[beach]] <- p
}

#Save and display plots
for (beach in names(plots)) {
  print(plots[[beach]])  # Display plot
  ggsave(paste0(beach, "_nest_sex_projection_mid_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}

#Sand
# Initialize a list to store plots for each beach
plots_s <- list()

for (beach in beach_names_s) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tocalc_s_mid))
  beach_data <- tocalc_s_mid[, beach_columns, drop = FALSE]
  
  # Melt data into long format for ggplot2
  # Melt data into long format for ggplot2
  melted_data <- data.frame(
    Day = (1:128) # Repeat days for each replicate
  )
  # Extract the temperature data for the current beach
  beach_columns <- grep(paste0("^", beach), colnames(beach_data))
  beach_temp <- as.vector(beach_data[, beach_columns])
  for (day in 1:128) {
    # Add the temperature data to melted_data
    melted_data$sex[day] <- mean(unlist(beach_temp[day]))
    melted_data$se[day] <- sd(unlist(beach_temp[day]))
  }
  
  
  
  #  Plot the line graph with error margins
  p = ggplot(melted_data, aes(x = Day, y = sex)) +
    geom_line(color = "blue", size = 1) +  # Line plot
    geom_ribbon(aes(ymin = sex - se, ymax = sex + se), 
                alpha = 0.2, fill = "blue") +  # Error margins as shaded area
    labs(title = paste("mid Term Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  # Store the plot in the list
  plots_s[[beach]] <- p
}

#Save and display plots
for (beach in names(plots_s)) {
  print(plots_s[[beach]])  # Display plot
  ggsave(paste0(beach, "_sand_sex_mid_projection_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}




# Initialize results dataframes
modelled_results_mid <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names)))
colnames(modelled_results_mid) <- colnames(wide_predict_mid[, 2:20])  # Adjust to include all 20 beaches
modelled_sd_mid <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names)))
colnames(modelled_sd_mid) <- colnames(wide_predict_mid[, 2:20])

#nest
# Loop through beaches (columns) and days (rows)
for (i in 1:length(beach_names)) { # Number of beaches
  for (j in 1:128) { # Number of days
    # Select the appropriate column for beach and row subset for the day
    colselect <- i + ((j-1)*length(beach_names))  # Calculate the correct index for tocalc
    modelled_results_mid[j, i] <- mean(tocalc_mid[, colselect], na.rm = TRUE)
    modelled_sd_mid[j, i] <- sd(tocalc_mid[, colselect], na.rm = TRUE)
  }
  print(paste0("beach_", i))
}
modelled_results_long_mid = modelled_results_mid
modelled_results_long_mid$date = seq.Date(from =  as.Date("2023-04-01") , by = "day", length.out = nrow(modelled_results_long_mid))
12
modelled_results_long_mid = modelled_results_long_mid %>%
  pivot_longer(cols = -date, names_to = "Beach", values_to = "Males")

#sand
modelled_results_s_mid <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names_s)))
colnames(modelled_results_s_mid) <- colnames(wide_predict_mid_s[, 2:24])  # Adjust to include all 20 beaches
modelled_sd_s_mid <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names_s)))
colnames(modelled_sd_s_mid) <- colnames(wide_predict_mid_s[, 2:24])

# Loop through beaches (columns) and days (rows)
for (i in 1:length(beach_names_s)) { # Number of beaches
  for (j in 1:128) { # Number of days
    # Select the appropriate column for beach and row subset for the day
    colselect <- i + ((j-1)*length(beach_names_s))  # Calculate the correct index for tocalc
    modelled_results_s_mid[j, i] <- mean(tocalc_s_mid[, colselect], na.rm = TRUE)
    modelled_sd_s_mid[j, i] <- sd(tocalc_s_mid[, colselect], na.rm = TRUE)
  }
  print(paste0("beach_", i))
}
modelled_results_long_s_mid = modelled_results_s_mid
modelled_results_long_s_mid$date = seq.Date(from =  as.Date("2023-04-01") , by = "day", length.out = nrow(modelled_results_long_s_mid))

modelled_results_long_s_mid = modelled_results_long_s_mid %>%
  pivot_longer(cols = -date, names_to = "Beach", values_to = "Males")

# Create the scatter plot
beach_male_plot_mid = ggplot(modelled_results_long_mid, aes(x = date, y = Males, color = Beach)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Date", y = "Mean Proportion of Male Hatchlings") +
  xlim(as.Date("2023-04-01"), as.Date("2023-08-05"))+
  #scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")+
  theme_classic()+
  geom_vline(xintercept = as.Date("2023-06-15"), color = "red")+
  geom_hline(yintercept = 0.20)

beach_male_plot_s_mid = ggplot(modelled_results_long_s_mid, aes(x = date, y = Males, color = Beach)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Date", y = "Mean Proportion of Male Hatchlings") +
  xlim(as.Date("2023-04-01"), as.Date("2023-08-05"))+
  #scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")+
  theme_classic()+
  geom_vline(xintercept = as.Date("2023-06-15"), color = "red")+
  geom_hline(yintercept = 0.20)

rowMeans(modelled_results_mid)
row_number_mid = as.data.frame(matrix(nrow = 38, ncol = 6))
colnames(row_number_mid) = c("Logger", "Date", "Beach", "June-15", "June-05", "May-26")

for(i in 1:length(beach_names)){
  row_number_mid[i,1] = "Nest"
  row_number_mid[i,2] =  min(which(modelled_results_mid[,i] < 0.2))
  row_number_mid[i,3] = colnames(modelled_results_mid[i])
  row_number_mid[i,4] = modelled_results_mid[75,i]
  row_number_mid[i,5] = modelled_results_mid[65,i]
  row_number_mid[i,6] = modelled_results_mid[55,i]
}

for(i in 1:length(beach_names_s)){
  e = length(beach_names_s) + i
  row_number_mid[e,1] = "Sand"
  row_number_mid[e,2] =  min(which(modelled_results_s_mid[,i] < 0.2))
  row_number_mid[e,3] = colnames(modelled_results_s_mid[i])
  row_number_mid[e,4] = modelled_results_s_mid[75,i]
  row_number_mid[e,5] = modelled_results_s_mid[65,i]
  row_number_mid[e,6] = modelled_results_s_mid[55,i]
}


row_number[,1] = row_number[,1] + 91

