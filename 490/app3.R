library(shiny)
library(shinydashboard)
library(readr)
library(ggplot2)
library(dplyr)
library(glue)
library(leaflet)
library(lubridate)
library(readxl)
library(psych)
library(forcats)
library(forecast)

complaints <- read.csv('C:\\Users\\Gerard Jaena\\Documents\\School Data Analytics\\Data 490\\New Project\\Data\\Code_Complaints_and_Violations_20231112.csv', header = T)

building <- read_excel('C:\\Users\\Gerard Jaena\\Documents\\School Data Analytics\\Data 490\\New Project\\Data\\iolp-buildings-11.2.23.xlsx')

describe(complaints)
describe(building)

##
complaints$Latitude <- round(complaints$Latitude, 2)
complaints$Longitude <- round(complaints$Longitude,2)
##
building$Latitude <- round(building$Latitude, 2)
building$Longitude <- round(building$Longitude,2)

main_df <- building %>% 
  left_join(complaints, by = c('Latitude','Longitude'))


main_df <- main_df %>% 
  na.omit() %>% 
  select("Owned or Leased","Latitude","Longitude","Building Rentable Square Feet" ,
         "Building Status" ,"Construction Date","Available Square Feet", "Real Property Asset Type",
         "OpenDate", "LastInspDate","RecordTypeDesc","Description")
dim(main_df)


## Re-code the response variable( RecordTypeDesc )
main_df$RecordTypeDesc <- fct_collapse(main_df$RecordTypeDesc,
                                       Weed = c("Shoreline", "Tree", "Tree , Weeds", "Weeds"),
                                       Noise = "Noise",
                                       Construction = c("Construction", "Construction , ECA", "Construction , Emergency", "Construction , Emergency , Tree", "Construction , LandLord/Tenant", "Construction , Noise", "Construction , Tree", "ECA"),
                                       Emergency = c("Emergency", "Emergency , LandLord/Tenant", "Emergency , Vacant Building"),
                                       Landlord_Tenant = c("LandLord/Tenant", "LandLord/Tenant , Noise", "LandLord/Tenant" , "Vacant Building", "LandLord/Tenant", "Weeds", "LandLord/Tenant , Vacant Building", "LandLord/Tenant , Weeds"),
                                       LandUse = c("Land Use", "Land UseConstruction ,", "Land UseEmergency ,", "Land UseLandLord/Tenant ,", "Land UseNoise ,", "Land UseTree ,", "Land UseVacant Building ,")
)


fct_count(main_df$RecordTypeDesc)

main_df$RecordTypeDesc<-as.factor(main_df$RecordTypeDesc)

main_df <- main_df %>% filter(!is.na(`Construction Date`) & !is.na(`Building Rentable Square Feet`))

filtered_df <- na.omit(main_df[, c("RecordTypeDesc", "Building Status")])


# Perform data scaling and clustering
numeric_features <- main_df %>% 
  select(`Building Rentable Square Feet`, `Construction Date`, `Available Square Feet`)

scaled_features <- scale(numeric_features)
kmeans_model <- kmeans(scaled_features, centers = 5, nstart = 10)
main_df$Cluster <- as.factor(kmeans_model$cluster)

# Overview Tab Content with separate boxes
overviewTab <- tabItem(tabName = "overview",
                       fluidRow(
                         box(title = "Project Overview", status = "primary", solidHeader = TRUE, width = 12,
                             HTML("This project investigates complaints and analyzes building information to identify trends and correlations. Our data sources include a comprehensive complaints dataset and a detailed building information dataset.<br><br>",
                                  "1. How do factors like a building's total rentable square footage and its construction date affect the availability of square footage within the building?<br><br>",
                                  "2. Can buildings be effectively grouped into distinct clusters based on key characteristics such as rentable square feet and construction date? And how do these clusters relate to the types and frequencies of complaints?<br><br>",
                                  "3. What is the future trend in building rentable square feet over the next year? And how might this impact the planning for building maintenance and complaint management?<br><br>")
                         ),
                         box(title = "Data Sources", status = "info", width = 6,
                             HTML("1. Complaints Dataset: Includes various reported issues related to property maintenance and construction.<br><br>",
                                  "2. Building Information Dataset: Provides detailed information about buildings, including construction dates, square footage, etc.")
                         ),
                         box(title = "Engineering Methods", status = "info", width = 6,
                             "The latitude and longitude in both datasets are rounded to two decimal places for standardization and matching coordinates between the two datasets.
                             The two datasets are then merged based on the latitude and longitude via a left join. Allowing all rows from the building dataset to be kept and all matching rows from the complaints dataset to be added where coordinates match.
                             The merged dataset is deemed main_df, and is cleaned futher by removing rows with missing values."
                                  
                         ),
                         # You can add more boxes here as needed
                       ),
                       fluidRow(
                         box(title = "Visualizations", status = "warning", solidHeader = TRUE, width = 12,
                             "This tab is used to show the exploratory data analysis of the project.
                             A scatter plot showing the relationship between construction date and rentable square feet having a nonlinear relationship.
                             A violin plot shows the different types of complaints across active buildings.
                             Then a barplot shows the frequency of complaint types by keyword descriptions.
                             Lastly a geospatial map showing the distribution of locations."
                         ),
                       ),
                       fluidRow(
                         box(title = "Tabulations", status = "warning", solidHeader = TRUE, width = 12,
                             "This tab is used to show the combined dataset in a easier to read manner."
                         ),
                       ),
                       fluidRow(
                         box(title = "Models", status = "warning", solidHeader = TRUE, width = 12,
                             "This tab is used to show the K-means and Arima forecasting part of the project. It contains sliders to also change the number of clusters or periods.
                             The K-means model helps us group buildings into clusters by building characteristics. Then by turning back to the tabulations tab we can see which cluster it is apart of and the complaints also attached to it to find trends.
                             Model forecasts the next 12 periods (months) of building rentable square footage. "
                         ),
                       ),
                       fluidRow(
                         box(title = "Models2", status = "warning", solidHeader = TRUE, width = 12,
                             "This tab is used to show a static linear regression model that was used in the project. And an interactive one to change between the attributes of the combined dataset.
                             The static model analyzes the relationship between the available square footage, rentable square footage, and construction date.
                             Identifying trends in building usage and space allocation over time. Allowing for forecasting space usage for similar buildings and possibly correlating with building complaints."
                         ),
                       )
)




# Visualizations Tab Content
visualizationsTab <- tabItem(tabName = "visualizations",
                             fluidRow(
                               box(title = "Scatter Plot", status = "primary", solidHeader = TRUE, width = 6,
                                   plotOutput("scatterPlot")
                               ),
                               box(title = "Violin Plot", status = "primary", solidHeader = TRUE, width = 6,
                                   plotOutput("violinPlot")
                               )
                             ),
                             fluidRow(
                               box(title = "Bar Plot of Complaint Types", status = "primary", solidHeader = TRUE, width = 6,
                                   plotOutput("barPlot")
                               ),
                               box(title = "Geospatial Distribution of Complaints", status = "primary", solidHeader = TRUE, width = 6,
                                   leafletOutput("leafletMap")
                               )
                             )
)



# Tabulations Tab Content
tabulationsTab <- tabItem(tabName = "tabulations",
                          fluidRow(
                            box(title = "Data Table", status = "primary", solidHeader = TRUE, width = 12,
                                DT::dataTableOutput("dataTable")
                            )
                          )
)



# Models Tab Content
modelsTab <- tabItem(tabName = "models",
                              fluidRow(
                                box(title = "Model Controls", status = "primary", solidHeader = TRUE, width = 12,
                                    sliderInput("numClusters", "Number of Clusters:", min = 2, max = 10, value = 5),
                                    sliderInput("forecastPeriods", "Forecasting Periods:", min = 1, max = 24, value = 12)
                                )
                              ),
                              fluidRow(
                                box(title = "KNN Clustering", status = "primary", solidHeader = TRUE, width = 6,
                                    plotOutput("knnPlot")
                                ),
                                
                                box(title = "Clustering of distributions", status = "primary", solidHeader = TRUE, width = 6,
                                    plotOutput("barPlot2")
                                ),
                                box(title = "ARIMA Forecasting", status = "primary", solidHeader = TRUE, width = 6,
                                    plotOutput("arimaPlot")
                                )
                              )
)



# Adding to the Models 2 Tab
models2Tab <- tabItem(tabName = "models2",
                      fluidRow(
                        box(title = "Static Regression Model", status = "primary", solidHeader = TRUE, width = 12,
                            plotOutput("staticRegressionPlot"),
                            verbatimTextOutput("staticRegressionSummary")
                        ),
                        box(title = "Interactive Regression Model", status = "primary", solidHeader = TRUE, width = 12,
                            selectInput("predictorVariable", "Choose a Predictor Variable:", choices = names(main_df)),
                            selectInput("responseVariable", "Choose a Response Variable:", choices = names(main_df)),
                            plotOutput("interactiveRegressionPlot"),
                            verbatimTextOutput("interactiveRegressionSummary")
                        )
                      )
)




##########
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Data 490 Final Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Visualizations", tabName = "visualizations", icon = icon("image")),
      menuItem("Tabulations", tabName = "tabulations", icon = icon("table")),
      menuItem("Models", tabName = "models", icon = icon("chart-line")),
      menuItem("Models 2", tabName = "models2", icon = icon("chart-line"))
      
    )
  ),
  dashboardBody(
    tabItems(
      overviewTab, # Overview tab
      visualizationsTab,
      tabulationsTab,
      modelsTab,
      models2Tab
      # Removed tabItem for Reactive Widgets
    )
  )
  
)


server <- function(input, output) {
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(main_df, aes(x = `Construction Date`, y = `Building Rentable Square Feet`)) +
      geom_point(color = "blue", alpha = 0.5) +
      labs(title = "Relationship between Construction Date and Building Rentable Square Feet",
           x = "Construction Date", y = "Building Rentable Square Feet") +
      theme_minimal()
  })
  
  # Violin Plot
  output$violinPlot <- renderPlot({
    ggplot(filtered_df, aes(x = `Building Status`, y = RecordTypeDesc, fill = `Building Status`)) +
      geom_violin(scale = "count", trim = FALSE) +
      labs(title = "Distribution of Complaint Types across Building Status",
           x = "Building Status", y = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability
    
  })
  
  # Bar Plot
  output$barPlot <- renderPlot({
    record_counts <- main_df %>% 
      group_by(RecordTypeDesc) %>% 
      summarise(count = n())
    
    ggplot(record_counts, aes(x = RecordTypeDesc, y = count)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Frequency Distribution of RecordTypeDesc",
           x = "RecordTypeDesc", y = "Frequency Count") +
      theme_minimal()
  })
  
  # Geospatial Distribution of Complaints (Leaflet Map)
  output$leafletMap <- renderLeaflet({
    geo_data <- na.omit(main_df[, c("Latitude", "Longitude", "RecordTypeDesc")])
    
    # Create a leaflet map
    m <- leaflet(geo_data) %>%
      addTiles() %>%  # Add default OpenStreetMap tiles as a base layer
      addCircleMarkers(
        lng = ~Longitude, 
        lat = ~Latitude,
        radius = 5,
        color = "red",
        popup = ~RecordTypeDesc,
        label = ~RecordTypeDesc
      )  # Add circle markers representing complaints with a popup and label indicating complaint type
    
    # Display the map
    m
  })
  
  # Server function for the Tabulations Tab
  output$dataTable <- DT::renderDataTable({
    DT::datatable(main_df, options = list(pageLength = 10))
  })
  
  
  # KNN Clustering Plot
  output$knnPlot <- renderPlot({
    # Use reactive number of clusters
    kmeans_model <- kmeans(scaled_features, centers = input$numClusters, nstart = 10)
    main_df$Cluster <- as.factor(kmeans_model$cluster)
    
    ggplot(main_df, aes(x = `Building Rentable Square Feet`, y = `Construction Date`, color = Cluster)) +
      geom_point() +
      labs(title = "K-Means Clustering",
           x = "Building Rentable Square Feet",
           y = "Construction Date",
           color = "Cluster") +
      theme_minimal()
  })
  
  output$barPlot2 <- renderPlot({
    # Summarize the count of each complaint type in each cluster
    complaint_summary <- main_df %>%
      group_by(Cluster, RecordTypeDesc) %>%
      summarize(Count = n())
    
    # Calculate the percentage of each complaint type within each cluster
    complaint_summary <- complaint_summary %>%
      group_by(Cluster) %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    # Stacked bar chart of complaint types by cluster
    ggplot(complaint_summary, aes(x = Cluster, y = Percentage, fill = RecordTypeDesc)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(title = "Distribution of Complaint Types by Cluster",
           x = "Cluster",
           y = "Percentage of Complaints",
           fill = "Complaint Type") +
      theme_minimal()
  })
  
  # ARIMA Forecasting Plot
  output$arimaPlot <- renderPlot({
    # Ensure 'main_df' and its columns are available here
    
    # Convert 'OpenDate' to a Date object
    main_df$OpenDate <- as.Date(main_df$OpenDate, format = "%m/%d/%Y")
    
    # Create a time series object
    ts_data <- ts(main_df$`Building Rentable Square Feet`, frequency = 12)  # Assuming monthly data
    
    # Split the data into training and testing sets
    train_size <- floor(0.8 * length(ts_data))
    train_data <- window(ts_data, end = train_size)
    
    # Fit an ARIMA model
    arima_model <- auto.arima(train_data)
    
    # Forecast future values using the slider input
    forecast_values <- forecast(arima_model, h = input$forecastPeriods)
    
    # Plot the forecast
    plot(forecast_values, main = "ARIMA Forecast")
  })
  
  
  # Static Regression Model
  output$staticRegressionPlot <- renderPlot({
    # Your existing plot code for the static model
    ggplot(main_df, aes(x = `Available Square Feet`, y = predictions)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Linear Regression Model for Available Square Feet Prediction",
           x = "Actual Available Square Feet",
           y = "Predicted Available Square Feet")
  })
  
  output$staticRegressionSummary <- renderPrint({
    summary(linear_model)
  })
  
  # Interactive Regression Model
  output$interactiveRegressionPlot <- renderPlot({
    reactive({
      req(input$predictorVariable, input$responseVariable)  # Ensure these inputs are selected
      formula <- as.formula(paste(input$responseVariable, "~", input$predictorVariable))
      interactive_model <- lm(formula, data = main_df)
      ggplot(main_df, aes_string(x = input$responseVariable, y = input$predictorVariable)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = "Interactive Linear Regression Model",
             x = paste("Actual", input$responseVariable),
             y = paste("Predicted", input$predictorVariable))
    })()
  })
  
  output$interactiveRegressionSummary <- renderPrint({
    reactive({
      req(input$predictorVariable, input$responseVariable)
      formula <- as.formula(paste(input$responseVariable, "~", input$predictorVariable))
      interactive_model <- lm(formula, data = main_df)
      summary(interactive_model)
    })()
  })
  
}



# Run the application
shinyApp(ui = ui, server = server)
