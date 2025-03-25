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
library(usethis)
library(shinymanager)


# Load dataset (replace with your actual data source)
data <- (online_retail)  # Adjust this line
head(data)
online_retail <- data
# Data cleaning and processing
cleaned_data <- online_retail %>%
  na.omit() %>%
  rename(UnitPrice = Price, InvoiceNo = Invoice, CustomerID = Customer.ID) %>%
  mutate(Revenue = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate))

# Pre-compute summaries
sales_trend <- cleaned_data %>%
  group_by(month = floor_date(InvoiceDate, "month")) %>%
  summarise(TotalRevenue = sum(Revenue))

regional_sales <- cleaned_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))

customer_behaviour <- cleaned_data %>%
  group_by(CustomerID, Country, StockCode) %>%
  summarise(TotalRevenue = sum(Revenue),
            PurchaseFrequency = n()) %>%
  arrange(desc(TotalRevenue))

product_performance <- cleaned_data %>%
  group_by(StockCode, Description) %>%
  summarise(TotalRevenue = sum(Revenue),
            TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalRevenue))


# 1. Define credentials using environment variables
credentials <- data.frame(
  user = c(Sys.getenv("SHINY_ADMIN_USER", "admin"), Sys.getenv("SHINY_USER", "user")),
  password = c(Sys.getenv("SHINY_ADMIN_PWD", "admin123"), Sys.getenv("SHINY_USER_PWD", "user123")),
  admin = c(TRUE, FALSE),  # Role definition: TRUE = admin, FALSE = regular user
  stringsAsFactors = FALSE
)

# 2. UI definition with shinymanager wrapper
ui <- secure_app(
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    navbarPage(
      "Sales Dashboard",
      tabPanel("Sales Trends", plotlyOutput("sales_trend")),
      tabPanel("Product Performance", highchartOutput("product_performance")),
      tabPanel("Regional Sales", leafletOutput("regional_sales")),
      tabPanel("Sales Data", dataTableOutput("sales_data")),
      tabPanel("Customer Insights", reactableOutput("customer_insights"))
    )
  ),
  # Optional: Customize login page
  tags_top = tags$div(
    tags$h4("Sales Dashboard Login", style = "text-align: center;"),
    tags$img(src = "https://via.placeholder.com/150", style = "display: block; margin: 0 auto;")
  )
)

# 3. Server logic with role-based access control
server <- function(input, output, session) {
  # Check authentication result and user role
  auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Reactive value to store user info
  user_info <- reactive({ auth$user })
  is_admin <- reactive({ auth$admin })
  
  # Sales Trend Plotly (accessible to all)
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
  
  # Product Performance Highchart (accessible to all)
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
  
  # Regional Sales Leaflet (accessible to all)
  output$regional_sales <- renderLeaflet({
    country_coords <- data.frame(
      Country = c("USA", "UK", "Nigeria"),
      Latitude = c(37.0902, 55.3781, 9.0820),
      Longitude = c(-95.7129, -3.4360, 8.6753)
    )
    
    country_sales <- cleaned_data %>%
      filter(Country %in% c("USA", "UK", "Nigeria")) %>%
      group_by(Country) %>%
      summarize(TotalSales = sum(Revenue)) %>%
      left_join(country_coords, by = "Country")
    
    leaflet(country_sales) %>%
      addTiles() %>%
      addCircleMarkers(
        lat = ~Latitude,
        lng = ~Longitude,
        radius = ~TotalSales / 100000,
        popup = ~paste(Country, ": $", TotalSales)
      )
  })
  
  # Sales Data DT (admin only)
  output$sales_data <- renderDataTable({
    if (is_admin()) {
      datatable(
        cleaned_data %>% sample_n(min(1000, nrow(cleaned_data))),
        options = list(
          pageLength = 10,
          searching = TRUE,
          ordering = TRUE
        )
      )
    } else {
      showNotification("Admin access required for Sales Data", type = "error")
      return(NULL)
    }
  })
  
  # Customer Insights Reactable (admin only)
  output$customer_insights <- renderReactable({
    if (is_admin()) {
      customer_data <- cleaned_data %>%
        group_by(CustomerID) %>%
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
    } else {
      showNotification("Admin access required for Customer Insights", type = "error")
      return(NULL)
    }
  })
}

# 4. Run the app
shinyApp(ui = ui, server = server)

