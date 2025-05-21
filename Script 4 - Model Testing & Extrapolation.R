#This script creates and assesses the effectiveness of models to interpolate and
#extrapolate temperature using nichemapr data to predict logger temp
#Load data

rm(list=ls())
load("Section3_Output.RData")


# Install required packages if not already installed
required_packages <- c("mgcv", "lme4", "randomForest", "gbm", "brms", "spdep", "earth", "e1071", "fda", "dlm", "spatialreg")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(mgcv)
library(lme4)
library(randomForest)
library(gbm)
library(brms)
library(spdep)
library(earth)
library(e1071)
library(fda)
library(dlm)
library(reshape)
library(lubridate)
library(tidyr)

#Now to create the model we need to remove all of gaps in nest temperature, but 
#we will keep a copy of this for our extrapolation/interpolation later
extrap_data = model_data_nest
model_data = model_data_nest[!is.na(model_data_nest$Nest), ]
model_data$Beach = as.factor(model_data$Beach)
model_data2 = model_data

extrap_data$yday = yday(extrap_data$datetime)
extrap_data$locn = substr(extrap_data$Beach, 1, 4)
model_data2$yday = yday(model_data2$datetime)
model_data2$locn = substr(model_data2$Beach, 1, 4)

# Initialize results table
model_results <- data.frame(
  Model = character(),
  AIC = numeric(),
  R_squared = numeric(),
  RMSE = numeric(), 
  MAE = numeric(), 
  MAPE = numeric(),
  stringsAsFactors = FALSE
)

# Create folder for saving plots
results_dir <- "Model Results"
dir.create(results_dir, showWarnings = FALSE)

#Format the extrapolation data for fitting, same as predictor data
extrap_data$locn <- substr(extrap_data$Beach, 1, 4)
extrap_data$yday = yday(extrap_data$datetime)

#Loop through the rows of model_data2 to match and assign lat-long
for (i in seq_len(nrow(extrap_data))) {
  # Find the matching Beach in meta
  matching_beach <- meta$Beach[meta$Beach == extrap_data$locn[i]]
  
  if (length(matching_beach) > 0) {
    # Assign latitude and longitude
    extrap_data$Latitude[i] <- meta$Lattitude[meta$Beach == matching_beach]
    extrap_data$Longitude[i] <- meta$Longitude[meta$Beach == matching_beach]
  } else {
    # Optional: Warning for unmatched rows
    warning(paste("No matching Beach found for row", i, "in extrap_data"))
  }
}

#Function for model metrics

calculate_errors <- function(actual, predicted) {
  RMSE <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  MAE <- mean(abs(actual - predicted), na.rm = TRUE)
  MAPE <- mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
  return(data.frame(RMSE = RMSE, MAE = MAE, MAPE = MAPE))
}

#ADd yday to the extrapolation data
extrap_data$yday = yday(extrap_data$datetime)
extrap_data$Beach = as.factor(extrap_data$Beach)

# 1. GAMM
extrap_gamm <- gamm(Nest ~ s(Soil) + s(Soil, by = yday, bs = "tp", k = 5),
                    random = list(Beach = ~1),
                    data = model_data2)
gamm_summary <- summary(extrap_gamm$gam)
GAMM_errors = calculate_errors(model_data2$Nest, extrap_gamm$gam$fitted.values)
model_results <- rbind(model_results, data.frame(
  Model = "GAMM",
  AIC = AIC(extrap_gamm$lme),
  RMSE = GAMM_errors$RMSE, MAE = GAMM_errors$MAE, MAPE = GAMM_errors$MAPE,
  R_squared = gamm_summary$r.sq
))

extrap_data$GAMM = predict(extrap_gamm$gam, extrap_data, type = 'response')



# 2. GLMM
extrap_glmm <- lmer(Nest ~ Soil + (1 | Beach), data = model_data2)
glmm_summary <- summary(extrap_glmm)
GLMM_errors = calculate_errors(model_data2$Nest, predict(extrap_glmm, newdata = model_data2))
model_results <- rbind(model_results, data.frame(
  Model = "GLMM",
  RMSE = GLMM_errors$RMSE, MAE = GLMM_errors$MAE, MAPE = GLMM_errors$MAPE,
  AIC = AIC(extrap_glmm),
  R_squared = NA  # lmer does not compute R-squared directly
))
extrap_data$GLMM = predict(extrap_glmm, newdata = extrap_data, re.form = NA, type = "response")


# 3. Random Forest
extrap_rf <- randomForest(Nest ~ Soil + yday + Beach, data = model_data2)
RF_error = calculate_errors(model_data2$Nest, extrap_rf$predicted)
model_results <- rbind(model_results, data.frame(
  Model = "Random Forest",
  AIC = NA,  # AIC is not applicable for Random Forest
  RMSE = RF_error$RMSE, MAE = RF_error$MAE,MAPE = RF_error$MAPE,
  R_squared = extrap_rf$rsq[length(extrap_rf$rsq)]
))
extrap_data$RandomForest = predict(extrap_rf, newdata = extrap_data)

# 4. Boosted Regression Trees
extrap_brt <- gbm(Nest ~ Soil + yday + Beach, data = model_data2,
                  distribution = "gaussian", n.trees = 1000, interaction.depth = 3)
BRT_error = calculate_errors(model_data2$Nest, extrap_brt$fit)
BRT_summary = summary(extrap_brt)
model_results <- rbind(model_results, data.frame(
  Model = "Boosted Regression Trees",
  AIC = NA,  # AIC is not applicable for BRT
  RMSE = BRT_error$RMSE, MAE = BRT_error$MAE, MAPE = BRT_error$MAPE,
  R_squared = cor(model_data2$Nest, pred_brt)^2
))
extrap_data$BRT = predict(extrap_brt, extrap_data)

# 5. Bayesian Model
extrap_bayes <- brm(Nest ~ s(Soil) + (1 | Beach), data = model_data2, family = gaussian(), chains = 2, cores = 2)
bayes <- predict(extrap_brt, n.trees = 1000, model_data2)
Bayes_error = calculate_errors(model_data2$Nest, bayes)

model_results <- rbind(model_results, data.frame(
  Model = "Bayesian",
  AIC = WAIC(extrap_bayes)$waic,  # Using WAIC instead of AIC
  RMSE = Bayes_error$RMSE, MAE = Bayes_error$MAE, MAPE = Bayes_error$MAPE,
  R_squared = bayes_R2(extrap_bayes)[1]
))
predictBayes <-predict(extrap_bayes, newdata = extrap_data)[, "Estimate"]
extrap_data$Bayes = predictBayes



# 7. MARS

mars_model <- earth(Nest ~ Soil + yday + locn, data = model_data2)
mars_summary <- summary(mars_model)
mars_p = predict(mars_model, model_data2)
mars_error = calculate_errors(model_data2$Nest, mars_p)
model_results <- rbind(model_results, data.frame(
  Model = "MARS",
  AIC = NA,  # AIC is not standard for MARS
  RMSE = mars_error$RMSE, MAE = mars_error$MAE, MAPE = mars_error$MAPE,
  R_squared = mars_summary$gcv
))
extrap_data$MARS = predict(mars_model, newdata = extrap_data)

# 8. SVM
svm_model <- svm(Nest ~ Soil + yday + locn, data = model_data2, kernel = "radial")
svm_error = calculate_errors(model_data2$Nest, svm_model$fitted)
model_results <- rbind(model_results, data.frame(
  Model = "SVM",
  AIC = NA,  # AIC is not applicable
  RMSE = svm_error$RMSE, MAE = svm_error$MAE, MAPE = svm_error$MAPE,
  R_squared = NA  # R-squared is not standard
))
svm_extrap = extrap_data[, c("Soil", "yday", "locn")]
extrap_data$SVM_Predictions <- predict(svm_model, newdata = svm_extrap)


# 9. Dynamic Linear Model
# Set up the dynamic linear model for extrap_data
dlm_model_extrap <- dlmModPoly(order = 2) + dlmModReg(extrap_data$Soil)

# Initial parameter estimates
initial_params_extrap <- c(0.1, 0.1)

# Fit the model
fit_extrap <- dlmMLE(extrap_data$Temperature, parm = initial_params_extrap, build = dlm_model_extrap)



model_results <- rbind(model_results, data.frame(
  Model = "Dynamic Linear Model",
  AIC = fit$value,  # Negative log-likelihood approximation
  R_squared = NA  # R-squared not directly computed
))
fitted_dlm <- dlmFilter(model_data2$Soil, dlm_model)
dlm <- fitted_dlm$m[-1]  # Extract one-step ahead predictions




for (i in 1:7) {
  # Calculate errors
  model = colnames(extrap_data[i+9])
  errors <- calculate_errors(extrap_data$Nest, extrap_data[[model]])
  
  # Residuals
  residuals <- extrap_data$Nest - extrap_data[[model]]
  
  # Create residual plot
  p <- ggplot(extrap_data, aes_string(x = model, y = "residuals")) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggtitle(paste("Residual Plot for", model)) +
    xlab(model) +
    ylab("Residuals")
  
  # Save plot
  ggsave(filename = file.path(results_dir, paste0(model, "_Residual_Plot.png")), plot = p, width = 8, height = 6)
  
  # Print errors
  print(paste("Model:", model))
  print(errors)
}

for (i in 1:7) {
  # Calculate errors
  model = colnames(extrap_data[i+9])
  subset = extrap_data
  
  # Create residual plot
  p <- ggplot(extrap_data, aes(x = datetime, y = extrap_data[,i+9], col = locn)) +
    geom_point() +
    ggtitle(paste("Temperature Predictions for", model)) +
    xlab(model) +
    ylab("Predicted Temperature")
  
  # Save plot
  ggsave(filename = file.path(results_dir, paste0(model, "_Predictions_Plot.png")), plot = p, width = 8, height = 6)
}

#Based on these metrics, the residuals and the predicted temperatures, I am selecting
#the SVM family 
#Now the model family has been chosen I will do some refining by tweaking the input 
#parameters and kernel type iteratively. 

svm_model <- svm(Nest ~ locn+ Soil + yday, data = extrap_data, kernel = "radial")
extrap_data$yday_smooth <- ns(extrap_data$yday, df = 4)  # Apply natural spline

svm_model_smooth <- svm(Nest ~ locn + Soil + yday_smooth, 
                        data = extrap_data, 
                        kernel = "radial")

tune_result <- tune.svm(Nest ~ locn + Soil + yday, 
                        data = extrap_data, 
                        kernel = "radial", 
                        cost = 10^(-1:2), 
                        gamma = 10^(-2:1))

svm_model_tuned <- tune_result$best.model  # Select best tuned model

svm_model_interaction <- svm(Nest ~ locn * yday + Soil, 
                             data = extrap_data, 
                             kernel = "radial")

svm_model_interaction2 <- svm(Nest ~ locn +  Soil, 
                              data = extrap_data, 
                              kernel = "radial")

#Tuning and smoothing are not helping the model performance so I will try changing the kernel method
#svm_model_s2 <- svm(Nest ~ locn+ Soil + yday, data = extrap_sand, kernel = "radial")
use = model_data2
use$locn = as.factor(use$locn)
svm_model_2 <- svm(Nest ~ Soil + yday+ locn, data = model_data2, kernel = "linear")
svm_model_3 <- svm(Nest ~ Soil + locn, data = use, kernel = "linear")
svm_model_4 <- svm(Nest ~ Soil + yday*locn + locn, data = use, kernel = "linear")

# List all objects in the workspace
all_objects <- ls()

# Filter for SVM models (assuming they are of class "svm")
svm_models <- all_objects[sapply(all_objects, function(obj) inherits(get(obj), "svm"))]

# Initialize an empty list to store errors
svm_errors <- list()

# Loop through each SVM model and calculate errors
for (model_name in svm_models) {
  model <- get(model_name)  # Retrieve the model object
  svm_errors[[model_name]] <- calculate_errors(model_data2$Nest, model$fitted)  # Store the error results
}

# View the errors
svm_errors

#Model 4 will be implemented
#Nest
extrap_data$SVM4 = predict(svm_model_4, newdata = svm_extrap)
#I will set up the same model to predict the sand only beaches by building an identical model


model_data_sand$Beach = as.factor(model_data_sand$Beach)
model_data_sand$yday = yday(model_data_sand$datetime)
model_data_sand$locn = substr(model_data_sand$Beach, 1, 4)
extrap_sand = model_data_sand
model_data_sand = model_data_sand[!is.na(model_data_sand$Nest), ]

svm_model_s <- svm(Nest ~ Soil + yday*locn + locn, data = model_data_sand, kernel = "linear")
svm_extrap_s = extrap_sand[, c("Soil", "yday", "locn")]
extrap_sand$fit_new = predict(svm_model_s, newdata = svm_extrap_s)

# Extract unique beach names from column names
#Nest
beach_names = unique(extrap_data$Beach)

#Sand
# Extract unique beach names from column names
beach_names_s = unique(extrap_sand$Beach)

#Now the data is to be reformatted for the rest of the modelling, this also gives
#a chance to remove all the extra variables created during model selection
wide_predict = extrap_data[, c("datetime", "Beach", "SVM4")]
colnames(wide_predict) = c("datetime", "beach", "predict")
wide_predict = pivot_wider(wide_predict, 
                           names_from = beach, 
                           values_from = predict)

#Sand
wide_predict_s = extrap_sand[,c("datetime", "Beach", "fit_new")]
colnames(wide_predict_s) = c("datetime", "beach", "predict")
wide_predict_s = pivot_wider(wide_predict_s, 
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
wide_predict_matrix = as.matrix(wide_predict[,-1])
wide_predict_matrix_s = as.matrix(wide_predict_s[,-1])

#Prediction loop for nest temperature 
names = colnames(wide_predict_matrix)
#Middle third proportions
u = 33.333
v = 66.666

tosave = as.data.frame(matrix(nrow = 100, ncol = (128*length(beach_names))))

for (i in 1:128){ #Sets number of days
  f = length(beach_names)
  columnsaver = ((i-1)*f) #length(names) not working
  
  for(l in 1:100){ #sets number of replicates
    
    for( j in 1:ncol(wide_predict_matrix)){ #Setting up all beaches
      #dur = (i * 131) + (l * 100) + (j * 22)
      repeat {
        dayzero = ((i-1)*24)+1 #sets day zero to be midnight of first day
        day30 = dayzero + (24*30)
        first30days = as.data.frame(mean(wide_predict_matrix[(dayzero:day30), j]))
        colnames(first30days) = "avgtemp"
        incubation_duration = predict(durationmodel, first30days) + rnorm(nrow(first30days), mean = 0, sd = sd(residuals(durationmodel)))
        #Ensures (some degree of) randomness when estimating incubation duration 
        dayend = ceiling(dayzero + (incubation_duration * 24))
        
        # Check if dayend is larger than 4392
        if (dayend <= 4392) {
          break  # Exit loop if condition is met
        }
      }
      heated = as.data.frame(wide_predict_matrix[dayzero:dayend,j])
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
      
      tosave[l,columnsaver+(j)] = beach_avg
      colnames(tosave)[columnsaver+(j)] = paste(heated$ID[k])
    }
  }
  
  print(paste("Day", i, "of 128 days done at",Sys.time(), sep = " "))
}

#Sand
names = colnames(wide_predict_matrix_s)
tosave_s = as.data.frame(matrix(nrow = 100, ncol = length(beach_names_s)))
set.seed(442)
for (i in 1:128){ #Sets number of days
  f = length(beach_names_s)
  columnsaver = ((i-1)*f) #length(names) not working
  
  for(l in 1:100){ #sets number of replicates
    
    for( j in 1:ncol(wide_predict_matrix_s)){ #Setting up all beaches
      #dur = (i * 131) + (l * 100) + (j * 22)
      repeat {
        dayzero = ((i-1)*24)+1
        day30 = dayzero + (24*30)
        first30days = as.data.frame(mean(wide_predict_matrix_s[(dayzero:day30), j]))
        colnames(first30days) = "avgtemp"
        incubation_duration = predict(durationmodel, first30days) + rnorm(nrow(first30days), mean = 0, sd = sd(residuals(durationmodel)))
        
        dayend = ceiling(dayzero + (incubation_duration * 24))
        
        # Check if dayend is larger than 4392
        if (dayend <= 4392) {
          break  # Exit loop if condition is met
        }
      }
      heated = as.data.frame(wide_predict_matrix_s[dayzero:dayend,j])
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
      
      tosave_s[l,columnsaver+(j)] = beach_avg
      colnames(tosave_s)[columnsaver+(j)] = paste(heated$ID[k])
    }
  }
  
  print(paste("Day", i, "of 128 days done at",Sys.time(), sep = " "))
}


# Convert the `tosave` matrix into a data frame for plotting
tosave_df <- as.data.frame(tosave)
colnames(tosave_df) <- colnames(tosave) # Ensure column names are correctly set

tosave_df_s <- as.data.frame(tosave_s)
colnames(tosave_df_s) <- colnames(tosave_s) # Ensure column names are correctly set


# Extract unique beach names from column names
colnames(tosave_df) <- gsub(" ", "_", colnames(tosave_df))
beach_names <- unique(substr(colnames(tosave_df[1:19]), 1, nchar(colnames(tosave_df)) - 6))

colnames(tosave_df_s) <- gsub(" ", "_", colnames(tosave_df_s))
beach_names_s <- unique(substr(colnames(tosave_df_s[1:23]), 1, nchar(colnames(tosave_df_s)) - 6))

#Make plots and save the data in a format to use later
#Nest
# Initialize a list to store plots for each beach
plots <- list()

for (beach in beach_names) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tosave_df))
  beach_data <- tosave_df[, beach_columns, drop = FALSE]
  
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
    labs(title = paste("Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  
  
  # Store the plot in the list
  plots[[beach]] <- p
}

#Save and display plots
for (beach in names(plots)) {
  print(plots[[beach]])  # Display plot
  ggsave(paste0(beach, "_nest_projection_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}

#Sand
# Initialize a list to store plots for each beach
plots_s <- list()

for (beach in beach_names_s) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tosave_df_s))
  beach_data <- tosave_df_s[, beach_columns, drop = FALSE]
  
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
    labs(title = paste("Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  # Store the plot in the list
  plots_s[[beach]] <- p
}

#Save and display plots
for (beach in names(plots_s)) {
  print(plots_s[[beach]])  # Display plot
  ggsave(paste0(beach, "sand_projection_plot.jpeg"), plot = plots_s[[beach]])  # Save plot as PNG
}
#Calculate the per day sex ratio using the Hill Equation

p = 29
s = -0.0336281
Hill_K = 0.1  # Renamed K to Hill_K
# Iterate over the rows and columns of tosave_df
#nest
tocalc = tosave_df
for (i in 1:nrow(tocalc)) {
  for (j in 1:ncol(tocalc)) {
    input_val <- tocalc[i, j]
    
    q <- exp((1 / s) * (log(p + Hill_K) - log(input_val + Hill_K)))
    sr <- 1 / (1 + q)
    
    # Overwrite the value in tosave_df
    tocalc[i, j] <- sr
  }
  print(paste0(i, "% done!"))
}

#sand
tocalc_s = tosave_df_s
for (i in 1:nrow(tocalc_s)) {
  for (j in 1:ncol(tocalc_s)) {
    input_val <- tocalc_s[i, j]
    
    q <- exp((1 / s) * (log(p + Hill_K) - log(input_val + Hill_K)))
    sr <- 1 / (1 + q)
    
    # Overwrite the value in tosave_df
    tocalc_s[i, j] <- sr
  }
  print(paste0(i, "% done!"))
}

# Convert the `tocalc` matrix into a data frame for plotting
#tosave_df <- as.data.frame(tosave)
#colnames(tosave_df) <- colnames(tosave) # Ensure column names are correctly set

#tosave_df_s <- as.data.frame(tosave_s)
#colnames(tosave_df_s) <- colnames(tosave_s) # Ensure column names are correctly set

#Make plots and save the data in a format to use later
#Nest
# Initialize a list to store plots for each beach
plots <- list()

for (beach in beach_names) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tocalc))
  beach_data <- tocalc[, beach_columns, drop = FALSE]
  
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
    labs(title = paste("Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  
  
  # Store the plot in the list
  plots[[beach]] <- p
}

#Save and display plots
for (beach in names(plots)) {
  print(plots[[beach]])  # Display plot
  ggsave(paste0(beach, "_nest_sex_projection_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}

#Sand
# Initialize a list to store plots for each beach
plots_s <- list()

for (beach in beach_names_s) {
  # Select columns corresponding to the current beach
  beach_columns <- grep(paste0("^", beach), colnames(tocalc_s))
  beach_data <- tocalc_s[, beach_columns, drop = FALSE]
  
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
    labs(title = paste("Projected Results for", beach), 
         x = "Day", 
         y = "Average Temperature") +
    theme_classic()
  
  # Store the plot in the list
  plots_s[[beach]] <- p
}

#Save and display plots
for (beach in names(plots_s)) {
  print(plots_s[[beach]])  # Display plot
  ggsave(paste0(beach, "_sand_sex_projection_plot.jpeg"), plot = plots[[beach]])  # Save plot as PNG
}




# Initialize results dataframes
modelled_results <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names)))
colnames(modelled_results) <- colnames(wide_predict[, 2:20])  # Adjust to include all 20 beaches
modelled_sd <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names)))
colnames(modelled_sd) <- colnames(wide_predict[, 2:20])

#nest
# Loop through beaches (columns) and days (rows)
for (i in 1:length(beach_names)) { # Number of beaches
  for (j in 1:128) { # Number of days
    # Select the appropriate column for beach and row subset for the day
    colselect <- i + ((j-1)*length(beach_names))  # Calculate the correct index for tocalc
    modelled_results[j, i] <- mean(tocalc[, colselect], na.rm = TRUE)
    modelled_sd[j, i] <- sd(tocalc[, colselect], na.rm = TRUE)
  }
  print(paste0("beach_", i))
}
modelled_results_long = modelled_results
modelled_results_long$date = seq.Date(from =  as.Date("2023-04-01") , by = "day", length.out = nrow(modelled_results_long))

modelled_results_long = modelled_results_long %>%
  pivot_longer(cols = -date, names_to = "Beach", values_to = "Males")

#sand
modelled_results_s <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names_s)))
colnames(modelled_results_s) <- colnames(wide_predict_s[, 2:24])  # Adjust to include all 20 beaches
modelled_sd_s <- as.data.frame(matrix(nrow = 128, ncol = length(beach_names_s)))
colnames(modelled_sd_s) <- colnames(wide_predict_s[, 2:24])

# Loop through beaches (columns) and days (rows)
for (i in 1:length(beach_names_s)) { # Number of beaches
  for (j in 1:128) { # Number of days
    # Select the appropriate column for beach and row subset for the day
    colselect <- i + ((j-1)*length(beach_names_s))  # Calculate the correct index for tocalc
    modelled_results_s[j, i] <- mean(tocalc_s[, colselect], na.rm = TRUE)
    modelled_sd_s[j, i] <- sd(tocalc_s[, colselect], na.rm = TRUE)
  }
  print(paste0("beach_", i))
}
modelled_results_long_s = modelled_results_s
modelled_results_long_s$date = seq.Date(from =  as.Date("2023-04-01") , by = "day", length.out = nrow(modelled_results_long_s))

modelled_results_long_s = modelled_results_long_s %>%
  pivot_longer(cols = -date, names_to = "Beach", values_to = "Males")

# Create the scatter plot
beach_male_plot = ggplot(modelled_results_long, aes(x = date, y = Males, color = Beach)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Date", y = "Mean Proportion of Male Hatchlings") +
  xlim(as.Date("2023-04-01"), as.Date("2023-08-05"))+
  #scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")+
  theme_classic()+
  geom_vline(xintercept = as.Date("2023-06-15"), color = "red")+
  geom_hline(yintercept = 0.20)

beach_male_plot_s = ggplot(modelled_results_long_s, aes(x = date, y = Males, color = Beach)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Date", y = "Mean Proportion of Male Hatchlings") +
  xlim(as.Date("2023-04-01"), as.Date("2023-08-05"))+
  #scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")+
  theme_classic()+
  geom_vline(xintercept = as.Date("2023-06-15"), color = "red")+
  geom_hline(yintercept = 0.20)

rowMeans(modelled_results)
row_number = as.data.frame(matrix(nrow = 38, ncol = 6))
colnames(row_number) = c("Logger", "Date", "Beach", "June-15", "June-05", "May-26")

for(i in 1:length(beach_names)){
  row_number[i,1] = "Nest"
  row_number[i,2] =  min(which(modelled_results[,i] < 0.2))
  row_number[i,3] = colnames(modelled_results[i])
  row_number[i,4] = modelled_results[75,i]
  row_number[i,5] = modelled_results[65,i]
  row_number[i,6] = modelled_results[55,i]
}

for(i in 1:length(beach_names_s)){
  e = length(beach_names_s) + i
  row_number[e,1] = "Sand"
  row_number[e,2] =  min(which(modelled_results_s[,i] < 0.2))
  row_number[e,3] = colnames(modelled_results_s[i])
  row_number[e,4] = modelled_results_s[75,i]
  row_number[e,5] = modelled_results_s[65,i]
  row_number[e,6] = modelled_results_s[55,i]
}


row_number[,1] = row_number[,1] + 91

#Clear the junk
objects_to_keep <- c("row_number", "to_save","to_calc", "modelled_results", "modelled_results_long", "beach_names",
                     "row_number_s", "to_save_s","to_calc_s", "modelled_results_s", "modelled_results_long_s", "beach_names_s")

# Remove all objects except the ones to keep
rm(list = setdiff(ls(), objects_to_keep))