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
library(rio)
library(shinymanager)


# Load dataset 
data <- (online_retail)  
head(data)
print(data)


# Data cleaning and processing
cleaned_data <- data %>%
  na.omit() %>%  # Remove rows with missing values
  rename(UnitPrice = Price, InvoiceNo = Invoice, Customer_ID = `Customer ID`) %>%  # Rename columns
  mutate(
    Revenue = Quantity * UnitPrice,  # Calculate Revenue
    InvoiceDate = as.Date(InvoiceDate, format = "%d/%m/%Y")  # Convert InvoiceDate to proper Date format
  )


# Pre-compute summaries
sales_trend <- cleaned_data %>%
  group_by(month = floor_date(InvoiceDate, "month")) %>%
  summarise(TotalRevenue = sum(Revenue))

regional_sales <- cleaned_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))

customer_behaviour <- cleaned_data %>%
  group_by(Customer_ID, Country, StockCode) %>%
  summarise(TotalRevenue = sum(Revenue),
            PurchaseFrequency = n()) %>%
  arrange(desc(TotalRevenue))

product_performance <- cleaned_data %>%
  group_by(StockCode, Description) %>%
  summarise(TotalRevenue = sum(Revenue),
            TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalRevenue))


# 1. Define credentials (two users: admin and regular)
credentials <- data.frame(
  user = c(Sys.getenv("SHINY_ADMIN_USER", "admin"), Sys.getenv("SHINY_USER", "user")),
  password = c(Sys.getenv("SHINY_ADMIN_PWD", "admin123"), Sys.getenv("SHINY_USER_PWD", "user123")),
  admin = c(TRUE, FALSE),  # TRUE = admin, FALSE = regular user
  stringsAsFactors = FALSE
)

# 2. UI definition with shinymanager wrapper and sidebar
ui <- secure_app(
  fluidPage(
    theme = bs_theme(version = 5, bootswatch = "minty"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("revenue_range", "Revenue Range",
                    min = min(cleaned_data$Revenue, na.rm = TRUE),
                    max = max(cleaned_data$Revenue, na.rm = TRUE),
                    value = c(min(cleaned_data$Revenue, na.rm = TRUE), 
                              max(cleaned_data$Revenue, na.rm = TRUE)),
                    step = 10,
                    width = "300px"),
        selectInput("country_filter", "Select Country",
                    choices = c("All", unique(cleaned_data$Country)),
                    selected = "All",
                    multiple = TRUE),
        downloadButton("download_data", "Download Processed Data"),
        selectInput("download_format", "Select Format",
                    choices = c("CSV" = "csv", "Excel" = "xlsx"),
                    selected = "csv"),
        downloadButton("download_report", "Generate Report")
      ),
      mainPanel(
        navbarPage(
          "Sales Dashboard",
          tabPanel("Sales Trends", plotlyOutput("sales_trend")),
          tabPanel("Product Performance", highchartOutput("product_performance")),
          tabPanel("Regional Sales", leafletOutput("regional_sales")),
          tabPanel("Sales Data", dataTableOutput("sales_data")),
          tabPanel("Customer Insights", reactableOutput("customer_insights")),
          uiOutput("admin_tab")
        )
      )
    )
  ),
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
  
  # Reactive values for user info
  user_info <- reactive({ auth$user })
  is_admin <- reactive({ auth$admin })
  
  # Reactive filtered data based on slider and dropdown
  filtered_data <- reactive({
    data <- cleaned_data %>%
      filter(Revenue >= input$revenue_range[1] & Revenue <= input$revenue_range[2])
    
    if (!"All" %in% input$country_filter) {
      data <- data %>% filter(Country %in% input$country_filter)
    }
    
    return(data)
  })
  
  # Dynamically render Admin tab only for admin users
  output$admin_tab <- renderUI({
    if (is_admin()) {
      tabPanel("Admin Panel", 
               h3("Admin-Only Controls"),
               p("This tab is visible only to admin users."),
               actionButton("example_btn", "Example Admin Action"),
               downloadButton("download_raw_data", "Download Raw Dataset (CSV)"),
               h4("Registered Users"),
               DTOutput("user_list")
      )
    } else {
      NULL
    }
  })
  
  # Download handler for processed data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("processed_sales_data_", Sys.Date(), ".", input$download_format, sep = "")
    },
    content = function(file) {
      data_to_export <- filtered_data()
      if (input$download_format == "csv") {
        write.csv(data_to_export, file, row.names = FALSE)
      } else if (input$download_format == "xlsx") {
        rio::export(data_to_export, file)
      }
      showNotification(
        paste("Processed data downloaded successfully as", input$download_format),
        type = "message"
      )
    }
  )
  
  # Download handler for report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("sales_report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("www/report.Rmd", temp_report, overwrite = TRUE)
      
      params <- list(
        revenue_min = input$revenue_range[1],
        revenue_max = input$revenue_range[2],
        countries = if("All" %in% input$country_filter) "All" else input$country_filter,
        data = filtered_data()
      )
      
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
      
      showNotification("Report generated successfully!", type = "message")
    }
  )
  
  # Sales Trend Plotly (accessible to all)
  output$sales_trend <- renderPlotly({
    sales_data <- filtered_data() %>%
      group_by(InvoiceDate, Country) %>%
      summarise(Revenue = sum(Revenue)) %>%
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
    total_selling_products <- filtered_data() %>%
      group_by(Description) %>%
      summarise(TotalQuantity = sum(Quantity)) %>%
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
    
    country_sales <- filtered_data() %>%
      filter(Country %in% c("USA", "UK", "Nigeria")) %>%
      group_by(Country) %>%
      summarise(TotalSales = sum(Revenue)) %>%
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
        filtered_data() %>% sample_n(min(1000, nrow(filtered_data()))),
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
      customer_data <- filtered_data() %>%
        group_by(Customer_ID) %>%
        summarise(TotalRevenue = sum(Revenue)) %>%
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
  
  # Download handler for raw dataset (admin only)
  output$download_raw_data <- downloadHandler(
    filename = function() {
      paste("sales_raw_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (is_admin()) {
        write.csv(filtered_data(), file, row.names = FALSE)
        showNotification("Raw dataset downloaded successfully!", type = "message")
      } else {
        showNotification("Admin access required to download raw data", type = "error")
      }
    }
  )
  
  # Render table of registered users (admin only)
  output$user_list <- renderDT({
    if (is_admin()) {
      datatable(
        credentials %>% select(user, admin) %>% 
          mutate(Role = ifelse(admin, "Admin", "User")) %>% 
          select(User = user, Role),
        options = list(
          pageLength = 5,
          searching = FALSE,
          lengthChange = FALSE
        )
      )
    } else {
      return(NULL)
    }
  })
}

# 4. Run the app
shinyApp(ui = ui, server = server)