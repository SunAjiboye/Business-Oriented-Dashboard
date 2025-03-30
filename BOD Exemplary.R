# Load required packages
library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(bslib)
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


# Load Dataset
online_retail <- data
head(online_retail)

# Clean data
cleaned_data <- online_retail %>%
  na.omit() %>%
  setNames(c("InvoiceNo", "StockCode", "Description", "Quantity",
             "InvoiceDate", "UnitPrice", "CustomerID", "Country")) %>%
  mutate(Revenue = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate, format = "%d/%m/%Y %H:%M")) %>%
  filter(!is.na(InvoiceDate))

# Define Country coordinates
country_coordinates <- list(
  "United Kingdom" = c(51.165691, -0.118092), "Germany" = c(51.165691, 10.451526),
  "France" = c(46.603354, 1.888334), "Netherlands" = c(52.132633, 5.291266),
  "Australia" = c(-25.274398, 133.775136), "Canada" = c(56.1304, -106.3468),
  "India" = c(20.5937, 78.9629), "Japan" = c(36.2048, 138.2529),
  "Brazil" = c(-14.2350, -51.9253), "EIRE" = c(53.1424, -7.6921),
  "Nigeria" = c(9.0820, 8.6753), "Portugal" = c(39.3999, -8.2245),
  "Luxembourg" = c(49.8153, 6.1296), "UAE" = c(23.4241, 53.8478),
  "USA" = c(37.0902, -95.7129), "Belgium" = c(50.8503, 4.3517),
  "Denmark" = c(56.2639, 9.5018), "Poland" = c(51.9194, 19.1451),
  "Spain" = c(40.4637, -3.7492), "Channel Islands" = c(49.3723, -2.3644),
  "Italy" = c(41.8719, 12.5674), "Cyprus" = c(35.1264, 33.4299),
  "Greece" = c(39.0742, 21.8243), "Norway" = c(60.4720, 8.4689),
  "Austria" = c(47.5162, 14.5501), "Sweden" = c(60.1282, 18.6435),
  "Finland" = c(61.9241, 25.7482), "Switzerland" = c(46.8182, 8.2275),
  "Malta" = c(35.8997, 14.5146), "Bahrain" = c(26.0667, 50.5577),
  "South Africa" = c(-30.5595, 22.9375), "Bermuda" = c(32.3078, -64.7505),
  "Hong Kong" = c(22.3193, 114.1694), "Singapore" = c(1.3521, 103.8198),
  "Thailand" = c(15.8700, 100.9925), "Israel" = c(31.0461, 34.8516),
  "Lithuania" = c(55.1694, 23.8813), "West Indies" = c(16.2650, -61.5510),
  "Lebanon" = c(33.8547, 35.8623), "South Korea" = c(35.9078, 127.7669),
  "Iceland" = c(64.9631, -19.0208)
)

# UI Definition
ui <- fluidPage(
  useWaiter(),
  useShinyFeedback(),
  useShinyalert(),
  theme = bs_theme(version = 5, bootswatch = "minty"),
  waiterPreloader(),
  navbarPage(
    "Sales Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Dashboard Filters"),
        dateRangeInput(
          "dateRange", "Date Range",
          start = min(cleaned_data$InvoiceDate),
          end = max(cleaned_data$InvoiceDate),
          min = min(cleaned_data$InvoiceDate),
          max = max(cleaned_data$InvoiceDate)
        ),
        selectInput(
          "countryFilter", "Select Countries",
          choices = unique(cleaned_data$Country),
          selected = unique(cleaned_data$Country)[1:5],
          multiple = TRUE
        ),
        sliderInput(
          "revenueRange", "Revenue Range",
          min = min(cleaned_data$Revenue),
          max = max(cleaned_data$Revenue),
          value = c(min(cleaned_data$Revenue), max(cleaned_data$Revenue)),
          step = 100
        ),
        selectInput(
          "topN", "Top N Products",
          choices = c(5, 10, 15, 20),
          selected = 10
        ),
        actionButton("refresh", "Refresh Data")
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


# Server Logic
server <- function(input, output, session) {
  # Welcome popup
  shinyalert(
    title = "Welcome!",
    text = "Welcome to the Sales Dashboard. Use the sidebar to filter data and explore visualizations.",
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
    showNotification("Data refreshed!", type = "message")
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
      type = "scatter",
      mode = "lines",
      color = ~Country
    ) %>%
      layout(
        title = "Sales Trend (Revenue by Date)",
        xaxis = list(title = "Invoice Date"),
        yaxis = list(title = "Revenue")
      )
  })
  
  # Product Performance Highchart
  output$product_performance <- renderHighchart({
    req(filtered_data())
    total_selling_products <- filtered_data() %>%
      group_by(Description) %>%
      summarize(TotalQuantity = sum(Quantity)) %>%
      arrange(desc(TotalQuantity)) %>%
      head(as.numeric(input$topN))
    
    hchart(
      total_selling_products,
      "bar",
      hcaes(x = Description, y = TotalQuantity)
    ) %>%
      hc_title(text = "Top Selling Products by Quantity")
  })
  
  # Regional Sales Leaflet
  output$regional_sales <- renderLeaflet({
    req(filtered_data())
    country_sales <- filtered_data() %>%
      group_by(Country) %>%
      summarize(TotalSales = sum(Revenue)) %>%
      mutate(
        Latitude = sapply(Country, function(x) country_coordinates[[x]][1]),
        Longitude = sapply(Country, function(x) country_coordinates[[x]][2])
      ) %>%
      filter(!is.na(Latitude) & !is.na(Longitude))
    
    leaflet(country_sales) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~log(TotalSales + 1) * 5,
        popup = ~paste(Country, ": $", round(TotalSales, 2)),
        color = "blue",
        fillOpacity = 0.7
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
    
    reactable(
      customer_data,
      columns = list(
        TotalRevenue = colDef(cell = function(value) {
          if (value > 1000) paste0("⭐ ", round(value, 2)) else round(value, 2)
        })
      )
    )
  })
  
  # Hide waiter when outputs are ready
  observe({
    if (!is.null(filtered_data())) waiter$hide()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
