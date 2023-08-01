# Load required libraries
library(shiny)
library(dplyr)
library(readr)
library(shinyFiles) # Load the shinyFiles package for file input from URL
library(DT)  # Load the DT package for displaying data tables
library(ggplot2)  # Load the ggplot2 package for creating visualizations

# Define the user interface (UI)
ui <- fluidPage(
  titlePanel("Inventory Management System"),
  
  sidebarLayout(
    sidebarPanel(
      # shinyFilesButton for uploading CSV file from URL
      shinyFilesButton("data_file", label = "Upload CSV File",  title = "Choose a CSV File", multiple = FALSE),
      
      # Select input widget to choose an inventory management technique
      selectInput("inventory_technique", "Select Inventory Management Technique",
                  c("JIT (Just-in-Time)", "EOQ (Economic Order Quantity)")),
      
      # Action button to trigger the calculation of reorder quantity and total costs
      actionButton("calculate_btn", "Calculate Reorder Quantity and Total Costs")
    ),
    
    mainPanel(
      tabsetPanel(
        # Tab for Data Exploration: Display the uploaded data in a table
        tabPanel("Data Exploration", dataTableOutput("data_table")),
        
        # Tab for Reorder Recommendations: Display the reorder recommendations in a table
        tabPanel("Reorder Recommendations", dataTableOutput("recommendations_table")),
        
        # Tab for Inventory Visualizations: Allows the user to select various visualizations
        tabPanel("Inventory Visualizations",
                 selectInput("visualization_type", "Select Visualization",
                             c("Product Type Distribution", "Product Type Amassed Revenue",
                               "Product Type Cost Breakdown", "SKU Unit Revenue Breakdown", 
                               "Projected Revenue vs. Project Cost")),
                 plotOutput("visualization_plot")),
        
        # Tab for Costs and Revenue: Display total costs and projected revenue
        tabPanel("Costs and Revenue",
                 h4("Total Costs:"),
                 verbatimTextOutput("total_costs_text"),
                 h4("Projected Revenue:"),
                 verbatimTextOutput("projected_revenue_text")),
        
        # Tab for Order Report: Download a CSV report with reorder recommendations
        tabPanel("Order Report", downloadButton("download_report", "Download Order Report")),
      )
    )
  )
)
# Define the server logic
server <- function(input, output) {
  # Reactive function to read data from the CSV URL
  data <- reactive({
    req(input$data_url)
    
    # Check if the URL ends with ".csv"
    if (!endsWith(input$data_url, ".csv")) {
      return(NULL)
    }
    
    tryCatch({
      # Try to read the CSV file from the URL
      inv_df <- read_csv(input$data_url, col_types = cols(reorder_point = col_double()))
      return(inv_df)
    }, error = function(e) {
      # Return NULL if there is an error reading the file
      return(NULL)
    })
  })
  
  # Step 1: Data Exploration - display uploaded data in a table
  output$data_table <- renderDataTable({
    data()
  })
  
  # Step 2: Select inventory management technique
  selected_technique <- reactive({
    if (input$inventory_technique == "JIT (Just-in-Time)") {
      return("JIT")
    } else {
      return("EOQ")
    }
  })
  
  # Steps 3 - 7: Allow App to generate recommendations and total costs calculation
  calculate_ordered_quantity_jit <- function(inv_df) {
    # Sort items based on the highest demand (descending order of last_month_num_of_products_sold)
    inv_df <- inv_df %>%
      arrange(desc(last_month_num_of_products_sold))
    
    remaining_capacity <- 6000 - sum(inv_df$inventory_levels)
    ordered_quantity <- integer(nrow(inv_df))
    
    for (i in 1:nrow(inv_df)) {
      if (remaining_capacity >= (inv_df$last_month_num_of_products_sold[i] - inv_df$inventory_levels[i])) {
        ordered_quantity[i] <- inv_df$last_month_num_of_products_sold[i] - inv_df$inventory_levels[i]
        remaining_capacity <- remaining_capacity - ordered_quantity[i]
      } else {
        ordered_quantity[i] <- remaining_capacity
        break
      }
    }
    
    # Ensure ordered_quantity is non-negative using pmax
    inv_df$ordered_quantity <- pmax(ordered_quantity, 0)
    
    # Calculate reorder point for JIT
    inv_df <- inv_df %>%
      mutate(
        remaining_inventory = inventory_levels + ordered_quantity - (last_month_daily_quantity_sold * lead_time),
        reorder_point = floor(30 - lead_time + 1 - (remaining_inventory / last_month_daily_quantity_sold))
      )
    
    # Set reorder point to 0 if it's negative
    inv_df$reorder_point[inv_df$reorder_point < 0] <- 0
    
    return(inv_df)
  }
  
  calculate_ordered_quantity_eoq <- function(inv_df) {
    # Calculate EOQ here
    inv_df <- inv_df %>%
      mutate(
        EOQ = sqrt((2 * last_month_daily_quantity_sold * shipping_cost_per_item) / (holding_cost_per_item_per_day * 30.44)),
        ordered_quantity = ceiling(EOQ)  # Round EOQ to the nearest whole number
      ) %>%
      select(-c(EOQ))  # Remove the unnecessary columns for EOQ
    
    # Calculate reorder point for EOQ
    inv_df <- inv_df %>%
      mutate(
        reorder_point = floor(30 - lead_time + 1 - (inventory_levels / last_month_daily_quantity_sold))
      )
    
    # Calculate remaining inventory for EOQ
    inv_df <- inv_df %>%
      mutate(
        remaining_inventory_eoq = inventory_levels + ordered_quantity - (last_month_daily_quantity_sold * lead_time)
      )
    
    # Set reorder point to 0 if it's negative
    inv_df$reorder_point[inv_df$reorder_point < 0] <- 0
    
    return(inv_df)
  }
  # Reactive function to calculate reorder recommendations
  reorder_recommendations <- reactive({
    req(data())  # Need to require total_order_quantity here
    
    inv_df <- data()
    inv_df <- inv_df %>%
      mutate(reorder_point = as.numeric(reorder_point))
    
    if (selected_technique() == "JIT") {
      inv_df <- calculate_ordered_quantity_jit(inv_df)
    } else {
      inv_df <- calculate_ordered_quantity_eoq(inv_df)
    }
    
    # Calculate total costs
    inv_df <- inv_df %>%
      mutate(
        total_costs = (ordered_quantity * shipping_cost_per_item) + 
          (30.44 * holding_cost_per_item_per_day * inventory_levels) + 
          (30.44 - lead_time) * holding_cost_per_item_per_day * 
          (inventory_levels + ordered_quantity - (last_month_daily_quantity_sold * lead_time))
      )
    
    return(inv_df)
  })
  
  
  # Step 8: Generate Order Report
  reorder_recommendations_data <- reactive({
    reorder_recommendations()
  })
  
  # Render reorder recommendations as a data table
  output$recommendations_table <- renderDataTable({
    reorder_recommendations_data()
  })
  
  # Reactive function to calculate total costs
  total_costs <- reactive({
    sum(reorder_recommendations_data()$total_costs)
  })
  
  # Reactive function to calculate projected revenue
  projected_revenue <- reactive({
    inv_df <- reorder_recommendations_data()
    total_items <- inv_df$ordered_quantity + inv_df$inventory_levels
    sum(total_items * inv_df$retail_price)
  })
  
  # Render total costs and projected revenue as text outputs
  output$total_costs_text <- renderText({
    paste("Total Costs: $", total_costs())
  })
  
  output$projected_revenue_text <- renderText({
    paste("Projected Revenue: $", projected_revenue())
  })
  
  # Generate Order Report
  output$download_report <- downloadHandler(
    filename = function() {
      paste("order_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(reorder_recommendations(), file)
    }
  )
  # Step 9: Define the Visualization Logic to update the plot based on the selected visualization
  observeEvent(input$visualization_type, {
    output$visualization_plot <- renderPlot({
      req(data())
      inv_df <- data()
      
      if (input$visualization_type == "Product Type Distribution") {
        # Bar chart for product type distribution
        ggplot(inv_df, aes(x = product_type)) +
          geom_bar(fill = "steelblue") +
          labs(title = "Product Type Distribution", x = "Product Type", y = "Count")
        
      } else if (input$visualization_type == "Product Type Amassed Revenue") {
        # Bar chart for product type amassed revenue
        ggplot(inv_df, aes(x = product_type, y = revenue_generated)) +
          geom_bar(stat = "summary", fun = "sum", fill = "orange") +
          labs(title = "Product Type Amassed Revenue", x = "Product Type", y = "Revenue")
        
      } else if (input$visualization_type == "Product Type Cost Breakdown") {
        # Bar chart for product type cost breakdown
        ggplot(inv_df, aes(x = reorder(product_type, -total_costs), y = total_costs)) +
          geom_bar(stat = "summary", fun = "sum", fill = "blue") +
          labs(title = "Product Type Cost Breakdown", x = "Product Type", y = "Total Costs") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      } else if (input$visualization_type == "SKU Unit Revenue Breakdown") {
        # Bar chart for SKU unit revenue breakdown
        ggplot(inv_df, aes(x = reorder(SKU, -revenue_generated), y = revenue_generated)) +
          geom_bar(stat = "summary", fun = "sum", fill = "green") +
          labs(title = "SKU Unit Revenue Breakdown", x = "SKU", y = "Revenue") +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
      } else if (input$visualization_type == "Projected Revenue vs. Project Cost") {
        # Bar chart for projected revenue vs. project cost
        total_cost_by_technique <- sum(reorder_recommendations_data()$total_costs)
        projected_revenue_by_technique <- projected_revenue()
        
        data_df <- data.frame(
          Type = c("Total Costs", "Projected Revenue"),
          Value = c(total_cost_by_technique, projected_revenue_by_technique)
        )
        
        ggplot(data_df, aes(x = Type, y = Value)) +
          geom_bar(stat = "identity", fill = c("red", "green")) +
          labs(title = "Projected Revenue vs. Project Cost", x = "Category", y = "Amount")
      }
    })
  })
}
# Run the Shiny app
shinyApp(ui, server)
