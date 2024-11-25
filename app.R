library(shiny)
library(mosaic)
library(tidyverse)
library(glue)
library(sf)
library(leaflet)
library(DT)

# Load data
preRoedata <- read_csv("OG_NationalAndStatePregnancy_PreDobbs_1988-2020.csv")
postRoedata <- read_csv("OG_MonthlyAbortionProvisionMonthly_2023-2024.csv")

# Pre-process data
preRoedata1 <- preRoedata %>%
  select(state, year, abortionstotal, abortionratetotal, abortionratiototal) %>%
  mutate(
    clinical_abortions = abortionstotal * 0.6,  # Adjust these proportions based on actual data
    medical_abortions = abortionstotal * 0.4,
    restriction_level = case_when(
      state %in% c("TX", "MS", "AL", "LA", "KY", "WV") ~ "high",
      state %in% c("FL", "GA", "SC", "NC", "OH", "IN") ~ "moderate",
      TRUE ~ "low"
    )
  )

postRoedata$year <- year(mdy(postRoedata$month))

postRoedata1 <- postRoedata %>%
  select(state, year, median) %>%
  group_by(state, year) %>%
  summarize(abortionstotal = sum(median)) %>%
  mutate(
    clinical_abortions = abortionstotal * 0.55,  # Adjust these proportions based on actual data
    medical_abortions = abortionstotal * 0.45,
    restriction_level = case_when(
      state %in% c("TX", "MS", "AL", "LA", "KY", "WV") ~ "high",
      state %in% c("FL", "GA", "SC", "NC", "OH", "IN") ~ "moderate",
      TRUE ~ "low"
    )
  )

abortion_data <- full_join(preRoedata1, postRoedata1, by = c("state", "year"))

abortion_data1 <- abortion_data %>%
  mutate(abortionstotal = coalesce(abortionstotal.x, abortionstotal.y)) %>%
  select(-abortionstotal.x, -abortionstotal.y) %>%
  arrange(state, year)

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

# UI
ui <- fluidPage(
  titlePanel("Abortion Data by State"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  min = min(abortion_data1$year),
                  max = max(abortion_data1$year),
                  value = min(abortion_data1$year),
                  step = 1,
                  sep = ""),
      
      selectInput("state", "State:",
                  choices = unique(abortion_data_geom$name)),
      
      selectInput("abortionType", "Abortion Type:",
                  choices = c("All Types" = "all",
                              "Clinical" = "clinical",
                              "Medical (Pill)" = "medical")),
      
      selectInput("restrictionLevel", "Restriction Level:",
                  choices = c("All Levels" = "all",
                              "Highly Restrictive" = "high",
                              "Moderately Restrictive" = "moderate",
                              "Less Restrictive" = "low")),
      
      hr(),
      
      helpText("This dashboard visualizes abortion data across the United States. Use the slider to select a year and the dropdown to choose a specific state.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", 
                 checkboxGroupInput("mapLayers", "Map Layers:",
                                    choices = c(
                                      "Total Abortions" = "total",
                                      "Clinical Abortions" = "clinical",
                                      "Medical Abortions" = "medical",
                                      "Restriction Levels" = "restrictions"
                                    ),
                                    selected = "total"),
                 leafletOutput("map", height = "600px"),
                 hr(),
                 h4("About the Map"),
                 p("This map shows multiple layers of abortion-related data. Toggle layers using the checkboxes above.")
        ),
        tabPanel("Data Table", 
                 h3("State-by-State Abortion Data"),
                 DT::dataTableOutput("stateTable"),
                 hr(),
                 h4("About the Data"),
                 p("This table provides detailed abortion statistics for each state in the selected year. You can sort and search the data using the table controls.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  bins <- c(0, 5, 10, 15, 20, 25, 30, 35, Inf)
  
  # Reactive filtered data based on selected year
  filtered_data <- reactive({
    data <- abortion_data_geom %>%
      filter(year == input$year)
    
    # Filter by abortion type if not "all"
    if (input$abortionType != "all") {
      data <- data %>%
        mutate(selected_abortions = case_when(
          input$abortionType == "clinical" ~ clinical_abortions,
          input$abortionType == "medical" ~ medical_abortions,
          TRUE ~ abortionstotal
        ))
    } else {
      data <- data %>%
        mutate(selected_abortions = abortionstotal)
    }
    
    # Filter by restriction level if not "all"
    if (input$restrictionLevel != "all") {
      data <- data %>%
        filter(restriction_level == input$restrictionLevel)
    }
    
    st_drop_geometry(data)
  })
  
  # Map rendering
  output$map <- renderLeaflet({
    req(input$year)
    
    abortion_data_year <- abortion_data_geom %>%
      filter(year == input$year)
    
    # Create different color palettes for each layer
    pal_total <- colorBin("YlOrRd", domain = abortion_data_year$abortionratetotal, bins = bins)
    pal_clinical <- colorBin("Blues", domain = abortion_data_year$clinical_abortions, bins = bins)
    pal_medical <- colorBin("Greens", domain = abortion_data_year$medical_abortions, bins = bins)
    pal_restrictions <- colorFactor(
      palette = c("red", "orange", "yellow"),
      domain = c("high", "moderate", "low")
    )
    
    # Base map
    map <- leaflet(abortion_data_year) %>%
      setView(-96, 37.8, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # Add layers based on selection
    if ("total" %in% input$mapLayers) {
      labels_total <- sprintf(
        "<strong>%s</strong><br/>Total: %g per 1,000 women",
        abortion_data_year$name, abortion_data_year$abortionratetotal
      ) %>% lapply(htmltools::HTML)
      
      map <- map %>%
        addPolygons(
          group = "Total Abortions",
          fillColor = ~pal_total(abortionratetotal),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels_total,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal_total,
          values = ~abortionratetotal,
          title = "Total Abortion Rate",
          group = "Total Abortions"
        )
    }
    
    if ("clinical" %in% input$mapLayers) {
      labels_clinical <- sprintf(
        "<strong>%s</strong><br/>Clinical: %g",
        abortion_data_year$name, abortion_data_year$clinical_abortions
      ) %>% lapply(htmltools::HTML)
      
      map <- map %>%
        addPolygons(
          group = "Clinical Abortions",
          fillColor = ~pal_clinical(clinical_abortions),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = labels_clinical
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal_clinical,
          values = ~clinical_abortions,
          title = "Clinical Abortions",
          group = "Clinical Abortions"
        )
    }
    
    # need to add similar blocks for medical abortions and restrictions...
    
    map
  })
  
  # Data table rendering
  output$stateTable <- DT::renderDataTable({
    # Debug print statements
    print("Starting data table render")
    print(names(filtered_data()))  # This will show us available columns
    
    data <- filtered_data() %>%
      as.data.frame() %>%
      # Only select columns we know exist from our data preprocessing
      select(name, abortionstotal, clinical_abortions, medical_abortions, restriction_level) %>%
      mutate(
        across(where(is.numeric), ~replace_na(., 0))
      )
    
    print("Data after processing:")
    print(head(data))  # This will show us the first few rows
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)