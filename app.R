# Load required libraries
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(ggplot2)
library(dplyr)
library(terra)
library(reshape)
library(lubridate)
library(DT)

map <- readRDS("map.rds")
map_near <- readRDS("map_near.rds")
map_mid <- readRDS("map_mid.rds")
map_far <- readRDS("map_far.rds")


# Define UI
ui <- fluidPage(
  titlePanel("Mediterranean Loggerhead Sex Ratio Estimation"),
  # Ensure the map maintains a 4:2 aspect ratio
  tags$style("
    #map_container {
      width: 100%;
      padding-top: 66.66%; /* 1 / 3 = 66.66% */
      position: relative;
    }
    #map {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
    }
  "),
  fluidRow(
    column(3,  # First column
           selectInput("dataset", "Select Temporal Projection:", 
                       choices = c("Present Data (2018-2019)" = "map", 
                                   "Near Term (2021-2040)" = "map_near", 
                                   "Mid Term (2041-2060)" = "map_mid", 
                                   "Far Term (2080-2100)" = "map_far"),
                       selected = "map"),
           radioButtons("grouping", "Group Data By:",
                        choices = c("Beach", "Country"),
                        selected = "Beach", inline = TRUE),
           checkboxInput("filter_nest", "Show Nest Data Only", value = FALSE),
           wellPanel(  # Create a wellPanel for the information section
             h4("Information"),
             p("This tool visualizes the sex ratio of Mediterranean Loggerhead turtles based on modelled predicted soil (Nichemapr) and in-situ temperature logger data from 31 beaches across 8 countries in the Mediterranean temporal projections."),
             p("Select a temporal projection, grouping criteria (Beach or Country), and adjust nesting data using the sliders."),
             p("Click on a pie chart for details. The map will display either beach-specific or country-specific information 
               depending on your selection. Line graph shows individual beaches coloured by country and a large black line which
               is population cohort projection. The pie chart is cohort projected sex ratio given the user's filters  in the
               filtered dataset."),
             p("Country specific nest abundance numbers are based primarily on Casale et al., 2018 and will reset to these numbers; Climate projection is IPCC scenario 7.0"),
             p("Beaches are jittered randomly to avoid overlapping pie charts, don't worry I don't think we have open water nests"),
             p("At this point in time the group by country display is NOT functioning as I updated the nesting by country date, I also have a load of warnings that I have no answers for at the moment"),
             p("This project is a work in progress and visuals will be updated in time"),
             p("To report issues or for further information please email me at jkirkham@dal.ca"), 
             p("To cite: Kirkham et al., 2025... [Needs Update]")
           )
    ),
    
    column(3,  # Second column
           #This is the previous date selection mechanism and provided a single value to all rows, I know include selection based on unique(data$country
           #selectInput("data_column", "Select Mean Nesting Date:", 
            #           choices = names(map)[sapply(map, is.numeric)],  # Numeric columns only
            #           selected = "Jun.15"),  # Default selection
           uiOutput("country_sliders")  # Dynamically generated sliders
    ),
    column(3, #Third Column
           uiOutput("date_slider")
           ),
    
    column(5,  # Fourth column
          plotOutput("line_graph", height = "400px"),
          plotOutput("weighted_pie", height = "400px")
    ),
    
    column(7,  # FIfth column
           div(id = "map_container", leafletOutput("map", width = "150%", height = "90%")),
    )
  )
)

server <- function(input, output, session) {
  
  
  # Reactive dataset based on user selection
  selected_data <- reactive({
    df <- get(input$dataset)  # Get selected dataframe
    
    # Apply nest filter if enabled
    if (input$filter_nest) {
      df <- df %>% filter(nest == 1)
    }
    
    return(df)
  })
  

  
  # Dynamically generate sliders for each country
  output$country_sliders <- renderUI({
    map_filtered <- selected_data()
    unique_countries <- unique(map_filtered$country)
    
    slider_list <- lapply(unique_countries, function(country) {
      country_total_nest <- sum(map_filtered$n_nest[map_filtered$country == country], na.rm = TRUE)
      default_value <- country_total_nest  # Default value for the slider
      
      # Create the slider and reset button for each country
      tagList(
        sliderInput(
          inputId = paste0("slider_", country),
          label = paste("Adjust nests in", country),
          min = 1,  # Default min = current value
          max = 5000,  # Allow doubling the nests
          value = default_value,
          step = 10
        ),
        actionButton(
          inputId = paste0("reset_", country),
          label = paste("Reset", country)
        )
      )
    })
    
    do.call(tagList, slider_list)
  })


  # Adjust data based on sliders
  adjusted_data <- reactive({
    df <- selected_data()
    
    unique_countries <- unique(df$country)
    
    for (country in unique_countries) {
      slider_id <- paste0("slider_", country)
      if (!is.null(input[[slider_id]])) {
        new_total_nest <- input[[slider_id]]
        original_total_nest <- sum(df$n_nest[df$country == country], na.rm = TRUE)
        
        if (original_total_nest > 0) {
          increase_factor <- new_total_nest / original_total_nest
          df$n_nest[df$country == country] <- df$n_nest[df$country == country] * increase_factor
        }
      }
      
      # Handle date selection for each country
      date_col <- input[[paste0("select_", country)]]
      if (!is.null(date_col)) {
        df$selected_date <- df[[date_col]]
      }
    }
    
    return(df)
  })
  
  # Observe reset button events
  observe({
    unique_countries <- unique(selected_data()$country)
    
    lapply(unique_countries, function(country) {
      observeEvent(input[[paste0("reset_", country)]], {
        # Reset the slider to the default value when the reset button is clicked
        updateSliderInput(session, paste0("slider_", country),
                          value = sum(selected_data()$n_nest[selected_data()$country == country], na.rm = TRUE))
      })
    })
  })
  
  # Render UI for country-based date selection
  output$date_slider <- renderUI({
    req(selected_data())  # Ensure data is loaded
    
    # Get unique values from the 'country' column
    unique_countries <- unique(selected_data()$country)
    
    # Create a list of selectInput widgets for each unique country
    select_inputs <- lapply(unique_countries, function(country_label) {
      selectInput(
        inputId = paste0("select_", country_label), 
        label = paste("Select mean nesting date for", country_label), 
        choices = colnames(selected_data()[7:134]),  # Select from appropriate columns
        selected = colnames(selected_data())[82]  # Default selection
      )
    })
    
    # Return the list as UI
    do.call(tagList, select_inputs)
  })
  

  
  # Render Leaflet map
  output$map <- renderLeaflet({
    map_filtered <- adjusted_data()  # Use adjusted data here
    
    # Check grouping selection
    if (input$grouping == "Country") {
      # Aggregate data by country
      map_agg <- map_filtered %>%
        group_by(country) %>%
        summarize(
          longitude = mean(longitude, na.rm = TRUE),
          latitude = mean(latitude, na.rm = TRUE),
          males = mean(!!sym(input$data_column), na.rm = TRUE),
          n_nest = sum(as.numeric(n_nest), na.rm = TRUE)
        ) %>%
        ungroup()
      
      females <- 1 - map_agg$males
      labels <- map_agg$country
      latitudes <- map_agg$latitude
      longitudes <- map_agg$longitude
      chart_data <- cbind(map_agg$males, females)
      n_nest <- map_agg$n_nest
      
    } else {
      # Individual beach data
      males = vector()
      # Iterate
      for (i in 1:nrow(map_filtered)) {
        selected_column <- input[[paste0("select_", map_filtered[i, "country"])]]  # Get the selected column for the country in the current row
        m <- map_filtered[[selected_column]][i]  # Extract males data from the selected column for the current row
        males = c(males, m)
      }
      females <- 1 - males
      labels <- map_filtered$beach
      latitudes <- map_filtered$latitude
      longitudes <- map_filtered$longitude
      chart_data <- cbind(males, females)
      n_nest <- map_filtered$n_nest
    }
    
    dodge_points <- function(longitudes, latitudes, threshold = 0.2, amount = 0.5) {
      # Compute pairwise distances
      dist_matrix <- as.matrix(dist(cbind(longitudes, latitudes)))
      diag(dist_matrix) <- Inf  # Ignore self-distance
      
      # Identify points that are too close
      close_points <- which(dist_matrix < threshold, arr.ind = TRUE)
      
      # Apply jitter only to points that are too close
      if (length(close_points) > 0) {
        longitudes[unique(close_points[, 1])] <- jitter(longitudes[unique(close_points[, 1])], amount = amount)
        latitudes[unique(close_points[, 1])] <- jitter(latitudes[unique(close_points[, 1])], amount = amount)
      }
      
      return(list(longitudes = longitudes, latitudes = latitudes))
    }
    set.seed(4)
    adjusted_coords <- dodge_points(longitudes, latitudes)
    # Define bin sizes for pie charts
    pie_sizes <- case_when(
      n_nest == 0 ~ 15,   # No pie chart for 0 nests
      n_nest < 100 ~ 15,  # Small pie for 1-99 nests
      n_nest < 500 ~ 30,  # Medium pie for 100-499 nests
      TRUE ~ 30            # Large pie for >500 nests
    )
    popup_texts <- paste0(
      "Beach: ", as.character(labels), "<br>",
      "Male: ", round(as.numeric(males) * 100, 2), "%<br>",  # Convert to % and round
      "Female: ", round(as.numeric(females) * 100, 2), "%<br>",
      "Nest Count: ", as.integer(n_nest)  # Convert to integer
    )
  
    
    # Create Leaflet map
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%  # Minimalistic base map
      setView(lng = mean(longitudes, na.rm = TRUE), 
              lat = mean(latitudes, na.rm = TRUE), 
              zoom = 5.5) %>%  # Adjust zoom for wider view
      addMinicharts(
        lng = adjusted_coords$longitudes, 
        lat = adjusted_coords$latitudes, 
        chartdata = chart_data, 
        type = "pie",
        width = pie_sizes,  
        height = pie_sizes,
        legend = TRUE,
        colorPalette = c("black", "grey"),
        popup = popupArgs(noPopup = T)
      ) %>%
      addCircleMarkers(
        lng = adjusted_coords$longitudes, 
        lat = adjusted_coords$latitudes,
        radius = 15,  # Make the marker larger to cover more area
        color = "transparent",  # Transparent circles
        weight = 0,  # No border
        popup = popup_texts# Make them clickable
        #,zIndexOffset = 1000  # Ensure they are above other layers (minicharts)
      )%>%
      addLabelOnlyMarkers(
        lng = longitudes, lat = latitudes, 
        label = labels,  
        labelOptions = labelOptions(
          textsize = "15px",
          noHide = NULL, 
          direction = "auto", 
          textOnly = TRUE, 
          offset = c(0, 0)  # Offset to the right by 10 units
        )
      ) %>%
      addScaleBar(position = "bottomleft") 
  })
  
  # Render line graph
  output$line_graph <- renderPlot({
    map_filtered <- adjusted_data()
    map_fi = map_filtered[1:134, ]
    map_fi = map_fi[,-c(3,4,6)]
    meltmap = map_fi[,-3]
    meltmap = melt(meltmap, id= c("country", "beach"))
    colnames(meltmap) = c("Country", "Beach", "Date", "Males")
    meltmap$Date = paste(meltmap$Date, "2018", sep = " ")
    meltmap$Date = mdy(meltmap$Date)
    meltmap = na.omit(meltmap)
    meltmap$Males = as.numeric(meltmap$Males)
    # Compute weighted proportions
    males <- map_fi[1:31,4:131]
    n_nests <- map_filtered$n_nest
    

    # Element-wise multiplication for each column (location/time series)
    weighted_male <- apply(males, 2, function(x) {
      sum(x * n_nests, na.rm = TRUE) / sum(n_nests, na.rm = TRUE)
    })
  
    
    
    mainline = as.data.frame(matrix(nrow = 128, ncol = 3))
    mainline[,1] = as.numeric(weighted_male)
    mainline[,2] = unique(meltmap$Date)
    mainline[,3] = "Total"
    colnames(mainline) = c("Males", "Date", "Beach")
    #vline = paste(input$data_column, "2018", sep = " ")
    #vline = mdy(vline)

    ggplot(data = meltmap, aes(x = Date, y = Males, group = Beach, col = Country))+
      ylim(c(0,1))+
      geom_line(alpha = 0.3)+
      geom_line(data = mainline, aes(x = Date, y = Males), color = "black", linewidth = 1.5)+
      #geom_vline(xintercept = vline, color = "red")+
      theme_classic()
  })
  
  output$weighted_pie <- renderPlot({
    map_filtered <- adjusted_data()  # Use adjusted data here
    
    # Avoid NA issues
    if (nrow(map_filtered) == 0) {
      return(NULL)  # No valid data, return nothing
    }
    
    # Compute weighted proportions based on selected columns for each country
    unique_countries <- unique(map_filtered$country)
    weighted_male <- 0
    weighted_female <- 0


    
    # Iterate over each unique country in the dataset
    for (i in 1:nrow(map_filtered)) {
      selected_column <- input[[paste0("select_", map_filtered[i, "country"])]]  # Get the selected column for the country in the current row
      males <- map_filtered[[selected_column]][i]  # Extract males data from the selected column for the current row
      n_nests <- map_filtered$n_nest[i]  # Get the number of nests for the current row
      # Calculate the weighted male proportion for the current row
      total_nests <- sum(map_filtered$n_nest, na.rm = TRUE)  # Total number of nests in the dataset (not by row)
      weighted_male <- weighted_male + (males * n_nests) / total_nests  # Add the weighted male to the cumulative sum
    }
    # Calculate the weighted female proportion
  
    weighted_female <- 1 - weighted_male  # Female proportion is the complement of male proportion
    

    
    # Data for ggplot pie chart
    pie_data <- data.frame(
      Category = c("Male", "Female"),
      Value = c(weighted_male, weighted_female)
    )

    

    # Plot pie chart
    ggplot(pie_data, aes(x = "", y = Value, fill = Category)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void() +  
      scale_fill_manual(values = c("Male" = "black", "Female" = "grey")) +
      geom_text(aes(label = scales::percent(Value, accuracy = 0.1)), 
                position = position_stack(vjust = 0.5), size = 12, color = "white") +
      ggtitle("Weighted Mean Male vs Female Proportion")
  })
}

# Run the application
shinyApp(ui = ui, server = server)