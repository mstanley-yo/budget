# Load packages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(ggplot2)
  library(forcats)
  library(lubridate)
})

# Configuration
wd <- file.path("~/Python/budget")
db_path <- file.path(wd, "budget.db")

# Function to read budget data from SQLite
read_budget_db <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  
  budget <- dbGetQuery(con, "
    SELECT 
      entry_datetime,
      store_name as category,
      amount
    FROM budget_entries
    ORDER BY entry_datetime
  ")
  
  dbDisconnect(con)
  
  budget %>%
    mutate(
      entry_datetime = as.POSIXct(entry_datetime),
      date = as.Date(entry_datetime),
      year_month = format(date, "%Y-%m"),
      amount = as.numeric(amount)
    )
}

# UI
ui <- page_navbar(
  title = "Budget Expense Tracker",
  theme = bs_theme(bootswatch = "flatly"),
  
  nav_panel(
    title = "Monthly Expenses",
    
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        
        selectInput(
          "year_month",
          "Select Year-Month:",
          choices = NULL,
          selected = NULL
        ),
        
        hr(),
        
        h4("Summary"),
        div(
          style = "font-size: 16px; line-height: 1.8;",
          textOutput("rent_text"),
          textOutput("expenses_text"),
          strong(textOutput("total_text"))
        ),
        
        hr(),
        
        actionButton(
          "refresh",
          "Refresh Data",
          icon = icon("sync"),
          class = "btn-primary",
          width = "100%"
        )
      ),
      plotOutput("expense_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to trigger data refresh
  refresh_trigger <- reactiveVal(0)
  
  # Observe refresh button
  observeEvent(input$refresh, {
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # Read budget data (reactive)
  budget_data <- reactive({
    refresh_trigger()  # Depend on refresh trigger
    
    if (!file.exists(db_path)) {
      showNotification("Database file not found!", type = "error")
      return(NULL)
    }
    
    tryCatch({
      read_budget_db(db_path)
    }, error = function(e) {
      showNotification(
        paste("Error reading database:", e$message),
        type = "error"
      )
      return(NULL)
    })
  })
  
  # Update year-month choices when data changes
  observe({
    data <- budget_data()
    
    if (!is.null(data) && nrow(data) > 0) {
      year_months <- unique(data$year_month) %>% sort(decreasing = TRUE)
      current_ym <- format(Sys.Date(), "%Y-%m")
      
      updateSelectInput(
        session,
        "year_month",
        choices = year_months,
        selected = if (current_ym %in% year_months) current_ym else year_months[1]
      )
    }
  })
  
  # Filter data by selected year-month
  filtered_data <- reactive({
    data <- budget_data()
    
    if (is.null(data) || nrow(data) == 0 || is.null(input$year_month)) {
      return(NULL)
    }
    
    data %>% filter(year_month == input$year_month)
  })
  
  # Calculate summary statistics
  summary_stats <- reactive({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(list(rent = 69330, expenses = 0, total = 69330))
    }
    
    expenses <- sum(data$amount)
    list(
      rent = 69330,
      expenses = expenses,
      total = expenses + 69330
    )
  })
  
  # Render summary text
  output$rent_text <- renderText({
    stats <- summary_stats()
    paste0("Rent: ¥", format(stats$rent, big.mark = ","))
  })
  
  output$expenses_text <- renderText({
    stats <- summary_stats()
    paste0("Other Expenses: ¥", format(stats$expenses, big.mark = ","))
  })
  
  output$total_text <- renderText({
    stats <- summary_stats()
    paste0("Total: ¥", format(stats$total, big.mark = ","))
  })

  output$expense_plot <- renderPlot({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available for selected month", cex = 1.5)
      return()
    }
    stats <- summary_stats()

    category_totals <- data %>%
      group_by(category) %>%
      summarise(total_amount = sum(amount), .groups = "drop")

    plot_data <- data %>%
      left_join(category_totals, by = "category")

    ggplot(
      plot_data,
      aes(x = fct_reorder(category, total_amount),y = amount)
    ) +
      geom_col(
        colour = "black",
        width = 0.8
      ) +
      coord_flip() +
      labs(
        x = "",
        y = "Amount (¥)",
        title = paste("Monthly Expenses for", input$year_month),
        subtitle = paste0(
          "Rent: ¥", format(stats$rent, big.mark = ","), ", ",
          "Other: ¥", format(stats$expenses, big.mark = ","), ", ",
          "Total: ¥", format(stats$total, big.mark = ",")
        )
      ) +
      scale_y_continuous(labels = scales::comma) +
      ggprism::theme_prism()
  })
}

# Run app
shinyApp(ui = ui, server = server)
