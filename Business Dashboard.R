library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(bslib)
library(ggplot2)
library(plotly)
library(highcharter)
library(reactable)
library(rio)
library(waiter)
library(shinyFeedback)
library(shinycssloaders)
library(shinyalert)
library(magrittr)
library(tidyverse)
library(rnaturalearth)

# Load and process data
online_retail <- tryCatch({
  rio::import("online_retail.csv")
}, error = function(e) {
  stop("Failed to load 'online_retail.csv'. Please ensure the file exists and is accessible.")
})

cleaned_data <- online_retail %>%
  na.omit() %>%
  setNames(c("InvoiceNo", "StockCode", "Description", "Quantity",
             "InvoiceDate", "UnitPrice", "CustomerID", "Country")) %>%
  mutate(Revenue = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate, format = "%d/%m/%Y %H:%M")) %>%
  filter(!is.na(InvoiceDate))  # Remove rows where date conversion failed


daily_sales <- cleaned_data %>%
  group_by(InvoiceDate) %>%
  summarise(DailyRevenue = sum(Revenue, na.rm = TRUE))

# Load required packages
library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(bslib)
library(ggplot2)
library(plotly)
library(highcharter)
library(reactable)
library(rio)
library(waiter)
library(shinyFeedback)
library(shinycssloaders)
library(shinyalert)
library(magrittr)
library(tidyverse)

# Load and process data
online_retail <- tryCatch({
  rio::import("online_retail.csv")
}, error = function(e) {
  stop("Failed to load 'online_retail.csv'. Please ensure the file exists and is accessible.")
})

cleaned_data <- online_retail %>%
  na.omit() %>%
  setNames(c("InvoiceNo", "StockCode", "Description", "Quantity",
             "InvoiceDate", "UnitPrice", "CustomerID", "Country")) %>%
  mutate(Revenue = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate, format = "%d/%m/%Y %H:%M")) %>%
  filter(!is.na(InvoiceDate))

# UI definition
ui <- fluidPage(
  useWaiter(),  # Initialize waiter
  useShinyFeedback(),  # Initialize shinyFeedback
  useShinyalert(),  # Initialize shinyalert
  theme = bs_theme(version = 5, bootswatch = "minty"),
  waiterPreloader(),  # Full-screen loading
  navbarPage(
    "Sales Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Dashboard Filters"),
        dateRangeInput(
          "dateRange",
          "Date Range",
          start = min(cleaned_data$InvoiceDate),
          end = max(cleaned_data$InvoiceDate),
          min = min(cleaned_data$InvoiceDate),
          max = max(cleaned_data$InvoiceDate)
        ),
        selectInput(
          "countryFilter",
          "Select Countries",
          choices = unique(cleaned_data$Country),
          selected = unique(cleaned_data$Country)[1:5],
          multiple = TRUE
        ),
        sliderInput(
          "revenueRange",
          "Revenue Range",
          min = min(cleaned_data$Revenue),
          max = max(cleaned_data$Revenue),
          value = c(min(cleaned_data$Revenue), max(cleaned_data$Revenue)),
          step = 100
        ),
        selectInput(
          "topN",
          "Top N Products",
          choices = c(5, 10, 15, 20),
          selected = 10
        ),
        loadingButton(
          "refresh",
          "Refresh Data",
          loadingLabel = "Refreshing..."
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          tabPanel("Sales Trends",
                   actionButton("readme", "Read Me"),
                   withSpinner(plotlyOutput("sales_trend"))),
          tabPanel("Product Performance",
                   withSpinner(highchartOutput("product_performance"))),
          tabPanel("Regional Sales",
                   withSpinner(leafletOutput("regional_sales"))),
          tabPanel("Sales Data",
                   withSpinner(dataTableOutput("sales_data"))),
          tabPanel("Customer Insights",
                   withSpinner(reactableOutput("customer_insights")))
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Welcome popup on app start
  shinyalert(
    title = "Welcome!",
    text = "Welcome to the Sales Dashboard. Use the sidebar to filter data and explore various visualizations.",
    type = "info",
    timer = 5000,
    showConfirmButton = FALSE
  )
  
  # Waiter loading screen
  waiter <- Waiter$new()
  waiter$show()
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$dateRange, input$countryFilter, input$revenueRange)
    
    # Validate inputs
    if (input$revenueRange[1] < 0) {
      showFeedbackWarning("revenueRange", "Revenue cannot be negative")
      return(NULL)
    }
    if (length(input$countryFilter) == 0) {
      showFeedbackWarning("countryFilter", "Please select at least one country")
      return(NULL)
    }
    
    hideFeedback("revenueRange")
    hideFeedback("countryFilter")
    
    cleaned_data %>%
      filter(
        InvoiceDate >= input$dateRange[1],
        InvoiceDate <= input$dateRange[2],
        Country %in% input$countryFilter,
        Revenue >= input$revenueRange[1],
        Revenue <= input$revenueRange[2]
      )
  })
  
  # Refresh button logic
  observeEvent(input$refresh, {
    loadingButtonReset("refresh")
    Sys.sleep(1)  # Simulate data refresh
    loadingButtonReset("refresh")
  })
  
  # Read Me popup
  observeEvent(input$readme, {
    showModal(modalDialog(
      title = "How to Use the Sales Dashboard",
      "1. Use the Date Range to filter by time period\n
       2. Select Countries to focus on specific regions\n
       3. Adjust Revenue Range to filter transactions\n
       4. Choose Top N Products to see best performers\n
       5. Click Refresh to update visualizations\n
       6. Explore tabs for different insights",
      easyClose = TRUE
    ))
  })
  
  # Sales Trend Plotly
  output$sales_trend <- renderPlotly({
    req(filtered_data())
    sales_data <- filtered_data() %>%
      group_by(InvoiceDate, Country) %>%
      summarize(Revenue = sum(Revenue)) %>%
      ungroup()
    
    plot_ly(
      data = sales_data,
      x = ~InvoiceDate,
      y = ~Revenue,
      type = 'scatter',
      mode = 'lines',
      color = ~Country
    )
  })
  
  # Product Performance Highchart
  output$product_performance <- renderHighchart({
    req(filtered_data())
    total_selling_products <- filtered_data() %>%
      group_by(Description) %>%
      summarize(TotalQuantity = sum(Quantity)) %>%
      arrange(desc(TotalQuantity)) %>%
      head(10)
    
    hchart(
      total_selling_products,
      "bar",
      hcaes(x = Description, y = TotalQuantity)
    )
  })
  
  # Regional Sales Leaflet
  output$regional_sales <- renderLeaflet({
    req(filtered_data())
    country_coords <- data.frame(
      Country = c("USA", "UK", "Nigeria"),
      Latitude = c(37.0902, 55.3781, 9.0820),
      Longitude = c(-95.7129, -3.4360, 8.6753)
    )
    
    country_sales <- filtered_data() %>%
      group_by(Country) %>%
      summarize(TotalSales = sum(Revenue)) %>%
      left_join(country_coords, by = "Country")
    
    if (any(is.na(country_sales$Latitude))) {
      warning("Some countries in the data lack coordinate mappings.")
    }
    
    leaflet(country_sales %>% filter(!is.na(Latitude))) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~TotalSales / 100000,
        popup = ~paste(Country, ": $", TotalSales)
      )
  })
  
  # Sales Data DT
  output$sales_data <- renderDataTable({
    req(filtered_data())
    datatable(
      filtered_data() %>% sample_n(min(1000, nrow(filtered_data()))),
      options = list(pageLength = 10)
    )
  })
  
  # Customer Insights Reactable
  output$customer_insights <- renderReactable({
    req(filtered_data())
    customer_data <- filtered_data() %>%
      group_by(CustomerID) %>%
      summarize(TotalRevenue = sum(Revenue)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(100)
    
    eactable(
      customer_data,
      columns = list(
        TotalRevenue = colDef(cell = function(value) {
          if (value > 1000) {
            paste0("⭐ ", value)
          } else {
            value
          }
        })
      )
    )
  })
}
  
  # Hide waiter when all outputs are ready
  observe({
    if (!is.null(filtered_data())) {
      waiter$hide()
    }
  })


# Run the app
shinyApp(ui = ui, server = server)