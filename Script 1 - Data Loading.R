#The following code is broken into several scripts that will provide breakpoints
#and user input to modify

#This first section allows the user to load up their data, match locations and 
#fetch the necessary environmnetal variables using the nicemapr package.

#The outputs from this script are collated soil data for all beaches across all years 
#And plots to match

#Section 1 - Data loading 

#Required Packages
library(dplyr)
library(NicheMapR)
library(lubridate)
library(tidyr)
library(grid)
library(gridExtra)
library(ggplot2)
#First Load the temperature time series data

data_2018 = read.csv("data_2018.csv", header = T)
data_2019 = read.csv("data_2019.csv", header = T)
data_2018[] <- lapply(data_2018, function(column) ifelse(column == "", NA, column))
data_2019[] <- lapply(data_2019, function(column) ifelse(column == "", NA, column))
#Next Load the Meta Data 
meta_2018 = read.csv("meta_2018.csv", header = T)
meta_2019 = read.csv("meta_2019.csv", header = T)

#And lastly load the nest duration data
nest_duration = read.csv("nest_durations.csv", header = T)



# Remove duplicate rows based on the 'beach' column, so that we don't unnecessarily
#rerun the loop
meta_2018_unique <- meta_2018 %>%
  distinct(Beach, .keep_all = TRUE)

#2018 Dates
dstart = "01/04/2018"
dfinish = "30/09/2018"


#Set location as blank 
loc = vector()

#Loop for fetching soil data
for(i in 1:nrow(meta_2018_unique)){
  loc[1] = as.numeric(meta_2018_unique[i,6]) #If rerunning the code make sure these columns are correct
  loc[2] = as.numeric(meta_2018_unique[i,5])
  micro = micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
  soil = as.data.frame(micro[["soil"]])
  soil = soil[,c("DOY", "TIME", "D50cm")]
  assign( paste("soil", meta_2018_unique$Beach[i], "2018", sep = "_") , soil)   
}
  
# Remove duplicate rows based on the 'beach' column, so that we don't unnecessarily
#rerun the loop
meta_2019_unique <- meta_2019 %>%
  distinct(Beach, .keep_all = TRUE)

#2019 Dates
dstart = "01/04/2019"
dfinish = "30/09/2019"


#Set location as blank 
loc = vector()

#Loop for fetching soil data
for(i in 1:nrow(meta_2019_unique)){
  loc[1] = as.numeric(meta_2019_unique[i,6])
  loc[2] = as.numeric(meta_2019_unique[i,5])
  micro = micro_ncep(loc = loc, dstart = dstart, dfinish = dfinish)
  soil = as.data.frame(micro[["soil"]])
  soil = soil[,c("DOY", "TIME", "D50cm")]
  assign( paste("soil", meta_2019_unique$Beach[i], "2019", sep = "_") , soil)   
}

rm(i, micro, loc, dstart, dfinish, soil, meta_2019_unique, meta_2018_unique)


#Create a large dataframe for plotting 
soil_objects = ls(pattern = "^soil_")
# Start and end timestamps
start_timestamp <- ymd_hms("2018-04-01 00:00:00") # Start at midnight
end_timestamp <-  ymd_hms("2018-09-30 23:00:00")    # End at midnight of the next day
hourly_timestamps <- seq(start_timestamp, end_timestamp, by = "hour")

allsoil = as.data.frame(matrix(nrow = length(hourly_timestamps), ncol = (length(soil_objects))))
#Save the date-time as one column
allsoil[,1] = hourly_timestamps
colnames(allsoil)[1] <- "datetime"

# Assuming the first column in allsoil is already assigned, start from column 2
for (i in seq_along(soil_objects)) {
  # Fetch the object by its name
  soil_data <- get(soil_objects[i])
  
  # Extract the $D50cm column
  D50cm_column <- soil_data$D50cm
  
  # Assign it to the corresponding column in allsoil
  allsoil[[i + 1]] <- D50cm_column
  
  # Rename the column based on the object name (remove 'soil_' prefix)
  colnames(allsoil)[i + 1] <- sub("^soil_", "", soil_objects[i])
}

# View the resulting allsoil data frame
head(allsoil)

soil_plot = gather(allsoil, key = "variable", value = "value", -datetime)

#Colour blind palette
pal <- c("#000000","#004949","#009292","#ff6db6","#ffb6db",
         "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff",
         "#920000","#924900","#db6d00","#24ff24","#FAF7AA")

nichemaprplot1 = ggplot(soil_plot[1:52704,], aes(x = datetime, y = value, color = variable, group = 1))+
  geom_point(size = 0.5)+
  labs(x = "Date", y = "Temperature (oC)", color = "Series") +
  ggtitle("NicheMapr Data, 50cm Soil Depth") +
  theme_classic() +
  scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")

nichemaprplot2 = ggplot(soil_plot[52705:105408,], aes(x = datetime, y = value, color = variable, group = 1))+
  geom_point(size = 0.5)+
  labs(x = "Date", y = "Temperature (oC)", color = "Series") +
  ggtitle("NicheMapr Data, 50cm Soil Depth") +
  theme_classic() +
  scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")

nichemaprplot3 = ggplot(soil_plot[105409:162504,], aes(x = datetime, y = value, color = variable, group = 1))+
  geom_point(size = 0.5)+
  labs(x = "Date", y = "Temperature (oC)", color = "Series") +
  ggtitle("NicheMapr Data, 50cm Soil Depth") +
  theme_classic() +
  scale_color_manual(values=pal)+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  labs(color = "Beach")


ggsave("nichemap2r.jpg", width = 4500, height = 2100, units = "px")
print(grid.arrange(nichemaprplot1, nichemaprplot2, nichemaprplot3, ncol = 1))
dev.off()
#Clear the junk
objects_to_keep <- c("allsoil", "data_2018", "data_2019", "meta_2018", "meta_2019", "nest_duration")

# Remove all objects except the ones to keep
rm(list = setdiff(ls(), objects_to_keep))
save.image(file = "Section1_Output.RData")
