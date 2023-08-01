# AMOD5250H-Term Project:
Repository for Term Project

# Overview:
The Inventory Management System R Shiny Prototype App is a web-based application designed to help businesses manage their inventory effectively. The app allows users to upload their inventory data in CSV format and provides various features to analyze the inventory, generate reorder recommendations, visualize inventory data, and calculate total costs and projected revenue.

# Features:
- Data Exploration: The "Data Exploration" tab displays the uploaded inventory data in a table format, allowing users to inspect the dataset.
- Reorder Recommendations: The "Reorder Recommendations" tab presents a table with reorder quantity recommendations based on selected inventory management techniques (Just-in-Time or Economic Order Quantity). It also calculates the total costs for the recommended reorder quantities.
- Inventory Visualizations: The "Inventory Visualizations" tab provides different visualization options, including product type distribution, product type amassed revenue, product type cost breakdown, SKU unit revenue breakdown, and projected revenue vs. project cost. Users can select a visualization type from the dropdown menu.
- Costs and Revenue: In the "Costs and Revenue" tab, the app displays the total costs and projected revenue based on the selected reorder recommendations.
- Order Report: Users can download a CSV report containing the reorder recommendations by clicking the "Download Order Report" button in the "Order Report" tab.

# How to Use:
1. Upload Data: Start by copying and pasting the URL of your inventory data in CSV format into the "Enter CSV URL" text input box provided in the sidebar panel.
2. Select Inventory Technique: Choose an inventory management technique (Just-in-Time or Economic Order Quantity) from the "Select Inventory Management Technique" dropdown in the sidebar.
3. Calculate Reorder Recommendations: Click the "Calculate Reorder Quantity and Total Costs" button to generate reorder recommendations and total costs based on the selected technique.
4. Explore Data and Visualize: Use the tabs to explore the data, visualize inventory information, and view reorder recommendations.
5. Download Order Report: If desired, download a CSV report with the reorder recommendations by clicking the "Download Order Report" button.

# Requirements:
- R Shiny library
- dplyr library
- readr library
- DT library
- ggplot2 library

# Note
This prototype app showcases the capabilities of an inventory management system and is intended for demonstration purposes.
