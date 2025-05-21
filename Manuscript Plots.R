#Interpolated Temperature plot 
#Nest
ggplot(extrap_data, aes(x = datetime, y = Soil, color= Beach))+
  geom_line()+
  theme_classic()+
  ylab("Interpolated/Extrapolated Nest Temperature (°C)")+
  xlab("Date")

#Sand 
ggplot(extrap_sand, aes(x = datetime, y = Soil, color= Beach))+
  geom_line()+
  theme_classic()+
  ylab("Interpolated/Extrapolated Sand Temperature (°C)")+
  xlab("Date")+
  guides(color = guide_legend(ncol = 1))  # Forces legend into a single column


#Facet present data boxplots
ggplot(data = modelled_results_long, aes( x = date, y = Males, color = Beach))+
  geom_boxplot()+
  facet_wrap(~Beach)+
  theme_classic()
 
#Primary results plots
# List of all the dataframes to join
dataframes_list <- list(
  modelled_results_long,
  modelled_results_long_far,
  modelled_results_long_mid,
  modelled_results_long_near,
  modelled_results_long_s,
  modelled_results_long_s_far,
  modelled_results_long_s_mid,
  modelled_results_long_s_near
)

# Full join all the dataframes by "Beach", marking variables as unique
#Nest
combined_modelled_results <- modelled_results_long

combined_modelled_results$near = modelled_results_long_near$Males
combined_modelled_results$mid = modelled_results_long_mid$Males
combined_modelled_results$far = modelled_results_long_far$Males

#Sand
combined_modelled_results_s <- modelled_results_long_s

combined_modelled_results_s$near = modelled_results_long_s_near$Males
combined_modelled_results_s$mid = modelled_results_long_s_mid$Males
combined_modelled_results_s$far = modelled_results_long_s_far$Males


combined_modelled_results= (pivot_longer(combined_modelled_results, cols = c(-date, -Beach), names_to = "state", values_to = "Males"))
combined_modelled_results_s= (pivot_longer(combined_modelled_results_s, cols = c(-date, -Beach), names_to = "state", values_to = "Males"))


combined_modelled_results <- combined_modelled_results %>%
  mutate(state = case_when(
    grepl("Males", state) ~ "Present (2018)",
    grepl("near", state) ~ "Near (2021-2040)",
    grepl("mid", state) ~ "Mid (2041-2060)",
    grepl("far", state) ~ "Far (2081-2100)"
  ))
combined_modelled_results_s <- combined_modelled_results_s %>%
  mutate(state = case_when(
    grepl("Males", state) ~ "Present (2018)",
    grepl("near", state) ~ "Near (2021-2040)",
    grepl("mid", state) ~ "Mid (2041-2060)",
    grepl("far", state) ~ "Far (2081-2100)"
  ))

#Remove the years to avergae on beaches with more than one year of insitu data
combined_modelled_results$Beach <- substr(combined_modelled_results$Beach, 1, nchar(combined_modelled_results$Beach) - 5)
combined_modelled_results_s$Beach <- substr(combined_modelled_results_s$Beach, 1, nchar(combined_modelled_results_s$Beach) - 5)

#Average between years 
combined_modelled_results <- combined_modelled_results %>%
  group_by(date, Beach, state) %>%
  summarise(Males = mean(Males), .groups = "drop")

combined_modelled_results_s <- combined_modelled_results_s %>%
  group_by(date, Beach, state) %>%
  summarise(Males = mean(Males), .groups = "drop")

combined_modelled_results$state = factor(combined_modelled_results$state , levels = c("Present (2018)", "Near (2021-2040)", "Mid (2041-2060)", 'Far (2081-2100)'))
combined_modelled_results_s$state = factor(combined_modelled_results_s$state , levels = c("Present (2018)", "Near (2021-2040)", "Mid (2041-2060)", 'Far (2081-2100)'))

colnames(combined_modelled_results) = c("Month", "Beach", "Projection", "Males" )
colnames(combined_modelled_results_s) = c("Month", "Beach", "Projection", "Males" )

ggplot(data = combined_modelled_results, aes(x = Month, y = Males, color = Projection))+
  xlim(c(ymd("2023-05-01"),ymd("2023-08-31")))+
  ylim(c(0,1))+
  geom_line()+
  geom_vline(xintercept = ymd("2023-06-15"), alpha = 0.3)+
  geom_hline(yintercept = 0.2, alpha = 0.3)+
  facet_wrap(~Beach, ncol = 3)+
  theme_classic()+
  theme(legend.position = "bottom",  # Position legend at the bottom
        legend.title = element_blank()) 


ggplot(data = combined_modelled_results_s, aes(x = Month, y = Males, color = Projection))+
  xlim(c(ymd("2023-05-01"),ymd("2023-08-31")))+
  ylim(c(0,1))+
  geom_line()+
  geom_vline(xintercept = ymd("2023-06-15"), alpha = 0.3)+
  geom_hline(yintercept = 0.2, alpha = 0.3)+
  facet_wrap(~Beach, ncol = 4)+
  theme_classic()+
  theme(legend.position = "bottom",  # Position legend at the bottom
        legend.title = element_blank()) 


row_cols = colnames(row_number)


combined_row = row_number

for( i in 1:3){
  inputdata = get(paste0("row_number_", states[i+1]))
  col1 = i*8
  col2 = (i*8)+7
  combined_row[,col1:col2] = inputdata
  gaga = paste(colnames(row_number), times[i+1], sep = "_")
  colnames(combined_row)[col1:col2] = gaga
}

combined_row = combined_row[,-c(1,2,7, 8,9,10,14,15,16,17,18,22,23,24,25,26,30, 31, 32)]
combined_row= (pivot_longer(combined_row, cols = c(-Beach), names_to = "state", values_to = "Males"))

#Because the formatting had got a bit messy I reformatted in excel so just loading that back in

plot_data = read.csv("row_number.csv")

plot_data$Phenology = factor(plot_data$Phenology, levels = c("No Shift", "10 Day Shift", "20 Day Shift"))
plot_data$Time = factor(plot_data$Time, levels = c("Present (2018)", "Near (2021-2040)", "Mid (2041-2060)", 'Far (2081-2100)'))
ggplot(data = plot_data, aes(x = Time, y = Males, color = Phenology))+
  geom_boxplot()+
  facet_wrap(~Beach, ncol = 3)+
  theme_classic()+
  theme(legend.position = "bottom",  # Position legend at the bottom
        legend.title = element_blank()) 


######################################
nesting_season = rand_nesting_season
nesting_season = pivot_longer(nesting_season, cols = everything(), names_to = "Beach", values_to = paste0("Present"))
seasons = ls(pattern = "^rand_nesting_season")

for( i in 1:length(seasons)){
  remem = get(seasons[i])
  remem = pivot_longer(remem, cols = everything(), names_to = "Beach", values_to = paste0("Males_", seasons[i]))
  nesting_season[,i+2] = remem[,2]            
}

nesting_season = pivot_longer(nesting_season, cols = c(-Beach), names_to = "State", values_to = "Males")

nesting_season <- nesting_season %>%
  mutate(Phenology = case_when(
    grepl("10_day_shift", State) ~ "10 Day Shift",
    grepl("20_day_shift", State) ~ "20 Day Shift",
    TRUE ~ "No Shift"
  ))

nesting_season <- nesting_season %>%
  mutate(Time = case_when(
    grepl("near", State) ~ "Near (2021-2040)",
    grepl("mid", State) ~ "Mid (2041-2060)",
    grepl("far", State) ~ "Far (2081-2100)",
    TRUE ~ "Present (2018)"
  ))

nesting_season = nesting_season[,-2]
nesting_season$Phenology = factor(nesting_season$Phenology, levels = c("No Shift", "10 Day Shift", "20 Day Shift"))
nesting_season$Time = factor(nesting_season$Time, levels = c("Present (2018)", "Near (2021-2040)", "Mid (2041-2060)", 'Far (2081-2100)'))
# Create a named vector for mapping
beach_mapping <- setNames(meta$beach, substr(meta$beach, 1, 3))

# Replace short names with long names
nesting_season <- nesting_season %>%
  mutate(Beach = recode(Beach, !!!beach_mapping))
ggplot(data = nesting_season, aes(x = Time, y = Males, color = Phenology))+
  geom_boxplot()+
  facet_wrap(~Beach, ncol = 3)+
  theme_classic()+
  theme(legend.position = "bottom",  # Position legend at the bottom
        legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
