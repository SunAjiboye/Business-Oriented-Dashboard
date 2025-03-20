# Business-Oriented-Dashboard
Practice from classwork
# 🛠 Shiny Coding Activity: Business-Oriented Dashboard

### **📌 Goal**
Build a **professional business-oriented Shiny app** using dynamic visualizations. The app will analyze **sales performance, customer behavior, and geographic trends** for a retail company.

---

## **📊 Dataset: Online Retail Transactions**
We will use the **Online Retail Dataset** from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/502/online+retail). This dataset contains transactional data from a UK-based online store (2010–2011).

**📝 Key Columns:**
- `InvoiceNo` – Transaction ID
- `StockCode` – Product identifier
- `Description` – Product name
- `Quantity` – Number of items bought
- `InvoiceDate` – Date of purchase
- `UnitPrice` – Price per item
- `CustomerID` – Unique customer identifier
- `Country` – Country of purchase

💡 **Business Use Case:**  
A retail company needs a Shiny dashboard to **track sales trends, customer behavior, and product performance** to optimize inventory and marketing.

---

## **📂 Step 1: Dataset Preparation**
### **1️⃣ Download & Store in GitHub**
1. Download the dataset: [Online Retail Dataset](https://archive.ics.uci.edu/dataset/502/online+retail)
2. Save the CSV file as `online_retail.csv`
3. Place it inside your GitHub repository in a `/data/` folder.

### **2️⃣ Import & Clean the Data**
Use the `rio` package for efficient data import:
```r
library(rio)
retail_data <- import("data/online_retail.csv")
```
Perform basic data cleaning:
```r
library(dplyr)

retail_data <- retail_data %>%
  filter(!is.na(CustomerID)) %>%  # Remove missing customers
  mutate(Revenue = Quantity * UnitPrice)  # Calculate revenue
```

---

## **📊 Step 2: Individual Exercise & Visualizations**
This is an **individual exercise**. Each participant will implement all visualizations within their own Shiny app, ensuring **UI consistency and interactivity**.

🔹 **Note:** For each visualization, you may need to **manipulate the data** (e.g., `group_by()`, `mutate()`, `summarize()`) before plotting.

### **1️⃣ Plotly - Sales Trends**
📌 **Goal:** Create an **interactive sales trend** (total revenue over time).  
🎯 **What to implement:**

- Line chart: **Revenue by InvoiceDate**

- Zooming, tooltips, and filtering by `Country`

- Use `plotly`

---

### **2️⃣ Highcharter - Top Products**
📌 **Goal:** Create a **stacked bar chart** showing the **top-selling products**.  
🎯 **What to implement:**

- Compare **quantity sold per product**

- Use `highcharter`

---

### **3️⃣ Leaflet - Sales by Country**
📌 **Goal:** Create a **map of sales volume per country**.  
🎯 **What to implement:**

- Interactive **marker map** displaying **total sales per country**

- Use `leaflet`

- **You will need to add country coordinates manually** (e.g., using a dataset of country lat/lon values)

- **Group sales by country** before plotting

---

### **4️⃣ DT - Interactive Transaction Table**
📌 **Goal:** Create an **interactive table** for sales transactions.  
🎯 **What to implement:**

- Sorting, searching, and pagination

- Use `DT`

---

### **5️⃣ Reactable - Customer Insights**
📌 **Goal:** Create an **interactive table** of **top customers**.  
🎯 **What to implement:**

- Conditional formatting for high-value customers

- Sorting, filtering, and interactive styling

- Use `reactable`

---

## **🚀 Step 3: Final App Assembly**
🎯 **Integrate all features into a single app.**  
📌 **Ensure:**

- A **professional UI** using `bslib` or the framework of your choice

- A **reactive** user experience (`reactive()`, `observe()`, `eventReactive()`, ...)

- **Usability & layout** (sidebars, navigation)

### **🔗 Step 4: Store in GitHub**
📂 **Folder Structure**

```
/business_dashboard
├── app.R                   # Main Shiny script
├── data/                   # Dataset folder
│   ├── online_retail.csv  
├── README.md               # Documentation
```
📌 **Submit GitHub Repo Link** at the end.

---

## **📢 Presentation & Review**

- Each participant presents their visualization (~5 min).

- Discuss challenges & improvements.

- Feedback on real-world business applicability.

🎯 **🚀 Goal:** Create a professional, interactive, and **business-relevant Shiny app**!

---
