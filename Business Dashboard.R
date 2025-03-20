
# Library -----------------------------------------------------------------

library(shiny)
library(dplyr)
library(DT)
library(leaflet)
library(bslib)
library(ggplot2)
library(plotly)
library(tidyr)
library(lubridate)
library(highcharter)
library(reactable)


# Import Data -------------------------------------------------------------

cleaned_data <- rio::import("data/cleaned_data.csv")
sales_trend <- rio::import("data/sales_trend.csv")
regional_sales <- rio::import("data/regional_sales.csv")
customer_behaviour <- rio::import("data/customer_behaviour.csv")
product_performance <- rio::import("data/product_performance.csv")

# Ui ----------------------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  navbarPage(
    "Sales Dashboard",
    tabPanel("Sales Trends", plotlyOutput("sales_trend")),
    tabPanel("Product Performance", highchartOutput("product_performance")),
    tabPanel("Regional Sales", leafletOutput("regional_sales")),
    tabPanel("Sales Data", dataTableOutput("sales_data")),
    tabPanel("Customer Insights", reactableOutput("customer_insights"))
  )
)


# Server ------------------------------------------------------------------

# Server logic
server <- function(input, output, session) {
  
  # Sales Trend Plotly
  output$sales_trend <- renderPlotly({
    sales_data <- cleaned_data %>%
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
    total_selling_products <- cleaned_data %>%
      group_by(Description) %>%
      summarize(TotalQuantity = sum(Quantity)) %>%
      arrange(desc(TotalQuantity)) %>%
      head(10)
    
    hchart(
      total_selling_products,
      "bar",
      hcaes(x = Description, y = TotalQuantity),
      stacking = "normal"
    ) %>%
      hc_title(text = "Top Selling Products")
  })
  
  # Regional Sales Leaflet (Updated with USA, UK, Nigeria)
  output$regional_sales <- renderLeaflet({
    # Coordinates for USA, UK, and Nigeria
    country_coords <- data.frame(
      Country = c("USA", "UK", "Nigeria"),
      Latitude = c(37.0902, 55.3781, 9.0820),    # Approximate central coordinates
      Longitude = c(-95.7129, -3.4360, 8.6753)
    )
    
    country_sales <- cleaned_data %>%
      filter(Country %in% c("USA", "UK", "Nigeria")) %>% # Filter to only these countries
      group_by(Country) %>%
      summarize(TotalSales = sum(Revenue)) %>%
      left_join(country_coords, by = "Country")
    
    leaflet(country_sales) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~TotalSales / 100000, # Adjust divisor based on your data scale
        popup = ~paste(Country, ": $", TotalSales)
      )
  })
  
  # Sales Data DT
  output$sales_data <- renderDataTable({
    datatable(
      cleaned_data %>% sample_n(min(1000, nrow(cleaned_data))),
      options = list(
        pageLength = 10,
        searching = TRUE,
        ordering = TRUE
      )
    )
  })
  
  # Customer Insights Reactable
  output$customer_insights <- renderReactable({
    customer_data <- cleaned_data %>%
      group_by(`Customer ID`) %>%
      summarize(TotalRevenue = sum(Revenue)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(100)
    
    reactable(
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

# Run the app
shinyApp(ui = ui, server = server)





