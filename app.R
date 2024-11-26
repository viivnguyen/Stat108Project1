library(shiny)
library(shinydashboard)
library(mosaic)
library(tidyverse)
library(glue)
library(sf)
library(leaflet)
library(DT)

# Load data
preRoedata <- read_csv("NationalAndStatePregnancy_PublicUse.csv")
postRoedata <- read_csv("MonthlyAbortionProvisionMonthly_2023-2024.csv")
state_policies <- read_csv("AbortionStatePolicies.csv", show_col_types = FALSE)

valid_years <- c(
  seq(1988, 2000, by = 4),  # 1988, 1992, 1996, 2000
  seq(2005, 2020, by = 1)   # 2005 through 2020
)

# Data Wrangling
preRoedata1 <- preRoedata %>%
  select(state, year, abortionstotal) %>%
  filter(year %in% valid_years)

postRoedata$year <- year(mdy(postRoedata$month))

postRoedata1 <- postRoedata %>%
  select(state, year, median) %>%
  group_by(state, year) %>%
  summarize(abortionstotal = sum(median))

abortion_data <- full_join(preRoedata1, postRoedata1, by = c("state", "year"))

abortion_data1 <- abortion_data %>%
  mutate(abortionstotal = coalesce(abortionstotal.x, abortionstotal.y)) %>%
  select(-abortionstotal.x, -abortionstotal.y) %>%
  arrange(state, year)

state_policies1 <- state_policies %>%
  select(State, 'Status of Abortion') %>%
  rename(Status = 'Status of Abortion')

# Load state geometry data
states <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")

# Merge state names
statenames <- read_csv("state_names.csv") %>%
  select("State", "Alpha code")

states <- full_join(states, statenames, by = c("name" = "State"))


# Merge geometry with abortion data
abortion_data_geom <- left_join(abortion_data1, states, by = c("state" = "Alpha code"))

# Ensure geometry column exists and convert to sf object
abortion_data_geom <- st_as_sf(abortion_data_geom)

print(names(abortion_data_geom))

# UI
ui <- fluidPage(
  titlePanel("Abortion Data by State"),
  p("This interactive Shiny app utilizes data about abortion before and after the overturn of Roe v. Wade"),
  p("Users can select the state, year, and degree of restriction."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:",
                  choices = valid_years,
                  selected = min(valid_years)),
      
      selectInput("state", "State:",
                  choices = unique(abortion_data_geom$name),
                  multiple = TRUE),
      
      selectInput("status", "Restriction Level:",
                  choices = c("All" = "all", unique(state_policies1$Status))),
      
      hr(),
      
      helpText("This dashboard visualizes abortion data across the United States. Use the slider to select a year and the dropdown to choose a specific state.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", height = "600px"),
                 hr(),
                 h4("About the Map"),
                 p("This map shows multiple layers of abortion-related state-level abortion policies. After the ruling, several states implemented near-total bans, which has led to shifts in abortion access and corresponding rate changes across the country.")
        ),
        
        tabPanel("Line Graph",
                 h3("Total Abortions Over Time"),
                 selectInput("lineStates", "Select States:",
                           choices = unique(abortion_data1$state),
                           selected = unique(abortion_data1$state)[1],
                           multiple = TRUE),
                 plotOutput("lineGraph"),
                 hr(),
                 h4("About the Graph")),
        tabPanel("Data Table", 
                 h3("State-by-State Abortion Data"),
                 DT::dataTableOutput("stateTable"),
                 hr(),
                 h4("About the Data"),
                 p("This table provides detailed abortion statistics for each state in the selected year. You can sort and search the data using the table controls."),
                 p("Abortion rates, ratios, and totals vary significantly between states due to a complex interplay of factors. These include the availability of healthcare facilities, state-level legislative restrictions, population demographics, and proximity to states with different abortion laws. For instance, some states have seen increased demand for abortion services due to residents from nearby states with stricter regulations traveling across borders for care."),
                 p("The overturning of Roe v. Wade in June 2022 led to increased variability in abortion access across the United States. This table helps provide insight into the impact of policy changes over time, including reductions in access in some areas and increases in others where laws remain more permissive.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  bins <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
  
  # Reactive filtered data based on selected year and restriction level
  filtered_data <- reactive({
    req(input$year)
    
    # Filter data based on year
    filtered <- abortion_data_geom %>%
      filter(year == input$year)
    
    # Only apply status filter if it's selected
    if (!is.null(input$status) && input$status != "all") {
      filtered <- filtered %>%
        left_join(state_policies1, by = c("name" = "State")) %>%
        filter(Status == input$status)
    }
    
    return(filtered)
  })
  
  # Line Graph: Filter data based on selected states
  filtered_line_data <- reactive({
    req(input$lineStates)
    abortion_data1 %>%
      filter(state %in% input$lineStates)
  })
  
  # Render Line Graph
  output$lineGraph <- renderPlot({
    data <- filtered_line_data()
    
    ggplot(data, aes(x = year, y = abortionstotal, color = state)) +
      geom_line(size = 1) +
      labs(title = "Total Abortions Over Time by State",
           x = "Year",
           y = "Total Abortions",
           color = "State") +
      theme_minimal()
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    req(input$year)
    req(input$year %in% valid_years)  # Only proceed if year is valid
    
    # Get the filtered data using reactive
    data <- filtered_data()
    
    # Check if the filtered data is NULL
    if (is.null(data) || !"geometry" %in% names(data)) {
      # If no data, show empty map with warning
      leaflet() %>%
        setView(-96, 37.8, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addControl(html = "<b>No data available for the selected year and restrictions.</b>", 
                  position = "topright")
    } else {
      # Create map with data
      pal <- colorBin("YlOrRd", domain = data$abortionstotal, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g total abortions",
        data$name, data$abortionstotal
      ) %>% lapply(htmltools::HTML)
      
      leaflet(data) %>%
        setView(-96, 37.8, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(abortionstotal),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels) %>%
        addLegend(pal = pal, values = ~abortionstotal, 
                 title = "Total Abortions", 
                 position = "bottomright")
    }
  })
  
  # Data table rendering
  output$stateTable <- DT::renderDataTable({
    data <- filtered_data()
    
    if (is.null(data)) {
      return(DT::datatable(
        data.frame(
          Message = "No data available for the selected year and restrictions."
        ),
        options = list(pageLength = 1),
        rownames = FALSE
      ))
    }
    
    data %>%
      st_drop_geometry() %>%
      select(name, abortionstotal) %>%  # Only select columns we know exist
      rename(
        State = name,
        `Total Abortions` = abortionstotal
      ) %>%
      DT::datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui = ui, server = server)
