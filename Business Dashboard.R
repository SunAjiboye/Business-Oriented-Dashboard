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
library(rio)
library(rmarkdown)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
library(shinymanager)
library(usethis)
  
# Load dataset dataset
data <- "online_retail.csv"
  summary(online_retail)
  str(online_retail)
  print(online_retail)
  
# Data Cleaning and Processing
  # Data cleaning and processing
  cleaned_data <- online_retail %>%
    na.omit() %>%
    rename(UnitPrice = Price, InvoiceNo = Invoice) %>%
    mutate(Revenue = Quantity * UnitPrice,
           InvoiceDate = as.Date(InvoiceDate))
  
  # Rename column
  colnames(cleaned_data)[which(colnames(cleaned_data) == "Price")] <- "UnitPrice"
  colnames(cleaned_data)[which(colnames(cleaned_data) == "Invoice")] <- "InvoiceNo"
  colnames(cleaned_data)[which(colnames(cleaned_data) == "customer.ID")] <- "customerID"
  head(cleaned_data)
  str(cleaned_data)
  
  # Pre-compute summaries
  sales_trend <- cleaned_data %>%
    group_by(month = floor_date(InvoiceDate, "month")) %>%
    summarise(TotalRevenue = sum(Revenue), .groups = "drop")
  
  regional_sales <- cleaned_data %>%
    group_by(Country) %>%
    summarise(TotalRevenue = sum(Revenue), .group = "drop") %>%
    arrange(desc(TotalRevenue))
  
  customer_behaviour <- cleaned_data %>%
    group_by(customerID, Country, StockCode) %>%
    summarise(TotalRevenue = sum(Revenue),
              PurchaseFrequency = n(), .group = "drop") %>%
    arrange(desc(TotalRevenue))
  
  product_performance <- cleaned_data %>%
    group_by(StockCode, Description) %>%
    summarise(TotalRevenue = sum(Revenue),
              TotalQuantity = sum(Quantity), .groups = "drop") %>%
    arrange(desc(TotalRevenue))

 
  
  # UI definition with login modal
  ui <- fluidPage(
    useShinyjs(),
    useShinyalert(),
    theme = bs_theme(version = 5, bootswatch = "minty"),
    # Initially hide the main UI
    div(id = "main_ui", style = "display: none;",
        navbarPage(
          "Sales Dashboard",
          tabPanel("Sales Trends", withSpinner(plotlyOutput("sales_trend"))),
          tabPanel("Product Performance", withSpinner(highchartOutput("product_performance"))),
          tabPanel("Regional Sales", withSpinner(leafletOutput("regional_sales"))),
          tabPanel("Sales Data", 
                   fluidRow(
                     column(12,
                            withSpinner(dataTableOutput("sales_data")),
                            selectInput("download_format", "Download Format", 
                                        choices = c("CSV" = "csv", "Excel" = "xlsx")),
                            downloadButton("download_data", "Download Processed Data"),
                            selectInput("country_filter", "Select Country", 
                                        choices = c("All", unique(cleaned_data$Country)),
                                        selected = "All"),
                            downloadButton("download_report", "Generate Report")
                     )
                   )
          ),
          tabPanel("Customer Insights", withSpinner(reactableOutput("customer_insights")))
        )
    )
  )
  
  # Server logic
  server <- function(input, output, session) {
    
    # Get credentials from .Renviron
    APP_USERNAME <- Sys.getenv("SHINY_USERNAME")
    APP_PASSWORD <- Sys.getenv("SHINY_PASSWORD")
    
    # Show login modal on app start
    observe({
      shinyalert(
        title = "Login Required",
        text = "Please enter your credentials to access the dashboard.",
        type = "input",
        inputType = "text",
        inputId = "username",
        inputPlaceholder = "Username",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Next",
        callbackR = function(value) {
          # Store username and prompt for password
          session$userData$username <- value
          shinyalert(
            title = "Enter Password",
            text = "Please enter your password.",
            type = "input",
            inputType = "password",
            inputId = "password",
            inputPlaceholder = "Password",
            showConfirmButton = TRUE,
            showCancelButton = FALSE,
            confirmButtonText = "Login",
            callbackR = function(value) {
              # Verify credentials
              if (session$userData$username == APP_USERNAME && value == APP_PASSWORD) {
                shinyjs::show("main_ui")  # Show the main UI
                shinyalert("Welcome!", "Login successful!", type = "success")
              } else {
                shinyalert("Error", "Invalid credentials. Please try again.", type = "error")
                # Restart login process
                session$reload()
              }
            }
          )
        }
      )
    })
    
    # Reactive filtered data based on country selection
    filtered_data <- reactive({
      if (input$country_filter == "All") {
        cleaned_data
      } else {
        cleaned_data %>% filter(Country == input$country_filter)
      }
    })
    
    # Sales Trend Plotly
    output$sales_trend <- renderPlotly({
      sales_data <- filtered_data() %>%
        group_by(InvoiceDate, Country) %>%
        summarize(Revenue = sum(Revenue), .groups = "drop") %>%
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
      total_selling_products <- filtered_data() %>%
        group_by(Description) %>%
        summarize(TotalQuantity = sum(Quantity), .groups = "drop") %>%
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
    
    # Regional Sales Leaflet
    output$regional_sales <- renderLeaflet({
      country_coords <- data.frame(
        Country = c("USA", "UK", "Nigeria"),
        Latitude = c(37.0902, 55.3781, 9.0820),
        Longitude = c(-95.7129, -3.4360, 8.6753)
      )
      
      country_sales <- filtered_data() %>%
        filter(Country %in% c("USA", "UK", "Nigeria")) %>%
        group_by(Country) %>%
        summarize(TotalSales = sum(Revenue), .groups = "drop") %>%
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
    
    # Sales Data DT
    output$sales_data <- renderDataTable({
      datatable(
        filtered_data() %>% sample_n(min(1000, nrow(filtered_data()))),
        options = list(
          pageLength = 10,
          searching = TRUE,
          ordering = TRUE
        )
      )
    })
    
    # Customer Insights Reactable
    output$customer_insights <- renderReactable({
      customer_data <- filtered_data() %>%
        group_by(`Customer ID`) %>%
        summarize(TotalRevenue = sum(Revenue), .groups = "drop") %>%
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
    
    # Download handler for processed data
    output$download_data <- downloadHandler(
      filename = function() {
        paste("sales_data_", Sys.Date(), ".", input$download_format, sep = "")
      },
      content = function(file) {
        shinyjs::disable("download_data")
        on.exit(shinyjs::enable("download_data"))
        
        data_to_export <- filtered_data() %>% 
          sample_n(min(1000, nrow(filtered_data())))
        
        if (input$download_format == "csv") {
          rio::export(data_to_export, file, format = "csv")
        } else {
          rio::export(data_to_export, file, format = "xlsx")
        }
        
        shinyalert("Success!", "Data downloaded successfully.", type = "success")
      }
    )
    
    # Download handler for R Markdown report
    output$download_report <- downloadHandler(
      filename = function() {
        paste("sales_report_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shinyjs::disable("download_report")
        on.exit(shinyjs::enable("download_report"))
        
        temp_report <- file.path(tempdir(), "report.Rmd")
        file.copy("www/report.Rmd", temp_report, overwrite = TRUE)
        
        params <- list(country = input$country_filter)
        
        rmarkdown::render(
          temp_report,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        shinyalert("Success!", "Report generated successfully!", type = "success")
      }
    )
    
    # Add reactive input for format selection
    observe({
      updateSelectInput(
        session,
        "download_format",
        label = "Select Download Format",
        choices = c("csv", "xlsx"),
        selected = "csv"
      )
    })
  }
  
  # Run the app
  shinyApp(ui = ui, server = server)