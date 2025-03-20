online_retail <- rio::import("data/online_retail.csv")
# Load dataset dataset
summary(online_retail)
str(online_retail)
print(online_retail)
colnames(online_retail)


# Data cleaning and processing
cleaned_data <- online_retail %>%
  na.omit() %>%
  rename(UnitPrice = Price, InvoiceNo = Invoice) %>%
  mutate(Revenue = Quantity * UnitPrice,
         InvoiceDate = as.Date(InvoiceDate))


# Rename column
colnames(cleaned_data)[which(colnames(cleaned_data) == "Price")] <- "UnitPrice"
colnames(cleaned_data)[which(colnames(cleaned_data) == "Invoice")] <- "InvoiceNo"



# Pre-compute summaries
sales_trend <- cleaned_data %>%
  group_by(month = floor_date(InvoiceDate, "month")) %>%
  summarise(TotalRevenue = sum(Revenue))

regional_sales <- cleaned_data %>%
  group_by(Country) %>%
  summarise(TotalRevenue = sum(Revenue)) %>%
  arrange(desc(TotalRevenue))

customer_behaviour <- cleaned_data %>%
  group_by(`Customer ID`, Country, StockCode) %>%
  summarise(TotalRevenue = sum(Revenue),
            PurchaseFrequency = n()) %>%
  arrange(desc(TotalRevenue))

product_performance <- cleaned_data %>%
  group_by(StockCode, Description) %>%
  summarise(TotalRevenue = sum(Revenue),
            TotalQuantity = sum(Quantity)) %>%
  arrange(desc(TotalRevenue))



rio::export(cleaned_data, "data/cleaned_data.csv")
rio::export(sales_trend, "data/sales_trend.csv")
rio::export(regional_sales, "data/regional_sales.csv")
rio::export(customer_behaviour, "data/customer_behaviour.csv")
rio::export(product_performance, "data/product_performance.csv")

