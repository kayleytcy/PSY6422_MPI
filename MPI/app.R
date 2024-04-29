

library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(markdown)

# Preparing the data for the visualisations in Shiny
original_data <- read_excel("./data/MPI_Data2023.xlsx", skip = 7)
threshold <- 5 
mpi_data <- original_data[rowSums(!is.na(original_data)) >= threshold,]
names(mpi_data) <- c("ISO Numeric Code", "ISO Country Code", "Country","World Region", "Survey Type", "Year", "MPI", "Headcount Ratio", "Intensity of Deprivation", "Vulnerability to Poverty", "Percentage in Severe Poverty", "Population in MPI Destitute Poverty", "Proportion of MPI poor", "Inequality among the Poor", "Year of the Survey", "Population 2020", "Population 2021", "Year of the Survey for MPI Poor", "Population 2020 for MPI Poor", "Population 2021 for MPI Poor", "Number of Indicators", "Missing Indicator")
write.csv(mpi_data, "./data/MPI_Data2023.csv", row.names = FALSE)
mpitime_data <- read_excel("./data/MPI_Data_Over_Time.xlsx", 
                           sheet = "6.1 Harmonised MPI",
                           skip = 7, 
                           range = cell_cols(c("C", "D", "I", "K")))
threshold <- 8
mpi_time <- mpitime_data[rowSums(!is.na(mpitime_data)) >= threshold, ]
names(mpi_time) <- c("Country", "World Region", "Survey Type", "t0", "Survey Type", "t1", "t0 MPI", "t1 MPI", "Change in MPI")
write.csv(mpi_time, "./data/MPI_Data_Over_Time.csv", row.names = FALSE)
mpi_time <- read.csv("./data/MPI_Data_Over_Time.csv")

world_map <- ne_countries(scale = "medium", returnclass = "sf")
mpi_data <- read.csv("./data/MPI_Data2023.csv")
mpi_data$Country <- gsub("Congo, Democratic Republic of the", "Democratic Republic of the Congo", mpi_data$Country)
mpi_data$Country <- gsub("eSwatini", "Kingdom of eSwatini", mpi_data$Country)
mpi_data$Country <- gsub("Gambia", "The Gambia", mpi_data$Country)
mpi_data$Country <- gsub("Palestine, State of", "Palestine", mpi_data$Country)
mpi_data$Country <- gsub("Sao Tome and Principe", "São Tomé and Principe", mpi_data$Country)
mpi_data$Country <- gsub("Viet Nam", "Vietnam", mpi_data$Country)
world_data <- merge(world_map, mpi_data, by.x = "name_long", by.y = "Country", all.x = TRUE)
world_data <- select(world_data, "name_long", "World.Region", "Year", "MPI", "Population.2021")


# Create the visualisations using shiny app
ui <- dashboardPage(
  dashboardHeader(title = "MPI in Developing Countries", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      # Set the names for different tabs
      menuItem("Introduction", tabName = "background", icon = icon("info-circle")),
      menuItem("MPI Map", tabName = "mpi_map", icon = icon("globe")),
      menuItem("MPI Changes Over Time", tabName = "mpi_changes", icon = icon("bar-chart")),
      menuItem("Interpretations", tabName = "interpretations", icon = icon("file-alt"))
    )
  ),
  # Include all the necessary contents
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "background",
              includeMarkdown("background.Rmd")),
      # Second tab content
      tabItem(tabName = "mpi_map",
              fluidRow(
                box(selectInput("region", "Select World Region:",
                                choices = c("All", sort(unique(na.omit(world_data$World.Region))))),
                    width = 3),
                box(leafletOutput("map", height = "500px"), width = 9))),
      # Third tab content
      tabItem(tabName = "mpi_changes",
              fluidRow(
                box(helpText("Select a world region to view the changes in MPI."),
                    selectInput("regionInput", "Select a Region:", 
                                choices = c("All", sort(unique(na.omit(mpi_time$`World.Region`))))),
                    width = 3),
                box(plotlyOutput("mpiPlot", height = "500px"), width = 9))),
      # Forth tab content
      tabItem(tabName = "interpretations",
              includeMarkdown("summary.Rmd"))
    )
  )
)

# Starts the visualisation by introducing the definition of the server function for Shiny
server <- function(input, output) {
  # Creates a color palette for better visualisation
  color_palette <- colorNumeric(palette = "YlOrRd", domain = world_data$MPI)
  # Output for the interactive map
  output$map <- renderLeaflet({
    # Filters world_data based on the World.Region selected
    filtered <- if (input$region != "All") {
      filter(world_data, World.Region == input$region)
    } else {
      world_data} 
    
    leaflet(data = filtered) %>%
      # Adjust these values to set the initial view
      setView(lng = 0, lat = 0, zoom = 1) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~color_palette(MPI),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste(name_long,
                       "<br>", "MPI:", MPI,
                       "<br>", "Population 2021:", Population.2021,
                       "<br>", "Year Data Collected:", Year)
      ) %>%
      addLegend("bottomright", pal = color_palette, values = ~MPI, title = "MPI", opacity = 1)
  })
  
  # Ouput for the bar graphs showing the changes in MPI
  output$mpiPlot <- renderPlotly({
    filtered_data <- mpi_time
    if (input$regionInput != "All") {
      filtered_data <- mpi_time[mpi_time$`World.Region` == input$regionInput, ]
    }
    #Setting different colors for each region
    color_mapping <- c("Arab States" = "#fffacd", 
                       "East Asia and the Pacific" = "#ffd8b1",
                       "Europe and Central Asia" = "#caffbf", 
                       "Latin America and the Caribbean" = "#ffd1dc", 
                       "South Asia" = "#c3b1e1", 
                       "Sub-Saharan Africa" = "#8be0fe")
    
    p <- ggplot(filtered_data, aes(x = Country, y = `Change.in.MPI`, fill = `World.Region`)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      scale_fill_manual(values = color_mapping) +
      theme_minimal() +
      labs(title = paste("Changes in MPI for", input$regionInput), 
           x = "Country", 
           y = "Change in MPI") +
      facet_wrap(~`World.Region`, scales = "free_x") +
      theme(axis.text.x = element_text(angle = 45, hjust =1))
    
    if(input$regionInput == "All"){
      # Removes the country names when "All" is selected
      p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
        labs(x = NULL)}
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)