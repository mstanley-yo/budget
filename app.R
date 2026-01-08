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
      id,
      entry_datetime,
      store_name as category,
      amount,
      logged_at
    FROM budget_entries
    ORDER BY entry_datetime DESC
  ")
  
  dbDisconnect(con)
  
  budget %>%
    mutate(
      entry_datetime = as.POSIXct(entry_datetime),
      logged_at = as.POSIXct(logged_at),
      date = as.Date(entry_datetime),
      year_month = format(date, "%Y-%m"),
      amount = as.numeric(amount)
    )
}

# Function to delete entry from database
delete_entry <- function(db_path, entry_id) {
  con <- dbConnect(SQLite(), db_path)
  
  result <- tryCatch({
    dbExecute(con, "DELETE FROM budget_entries WHERE id = ?", params = list(entry_id))
    TRUE
  }, error = function(e) {
    FALSE
  }, finally = {
    dbDisconnect(con)
  })
  
  return(result)
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
  ),
  
  nav_panel(
    title = "All Entries",
    
    layout_sidebar(
      sidebar = sidebar(
        open = "always",
        
        selectInput(
          "entries_year_month",
          "Select Year-Month:",
          choices = NULL,
          selected = NULL
        ),
        
        hr(),
        
        h4("Month Summary"),
        div(
          style = "font-size: 16px; line-height: 1.8;",
          textOutput("entries_count"),
          textOutput("entries_total")
        ),
        
        hr(),
        
        actionButton(
          "refresh_entries",
          "Refresh Data",
          icon = icon("sync"),
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      div(
        style = "padding: 20px;",
        uiOutput("entries_list")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to trigger data refresh
  refresh_trigger <- reactiveVal(0)
  
  # Observe refresh buttons
  observeEvent(input$refresh, {
    refresh_trigger(refresh_trigger() + 1)
  })
  
  observeEvent(input$refresh_entries, {
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
  
  # Update year-month choices when data changes (for Monthly Expenses tab)
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
  
  # Update year-month choices for entries tab
  observe({
    data <- budget_data()
    
    if (!is.null(data) && nrow(data) > 0) {
      year_months <- unique(data$year_month) %>% sort(decreasing = TRUE)
      current_ym <- format(Sys.Date(), "%Y-%m")
      
      updateSelectInput(
        session,
        "entries_year_month",
        choices = year_months,
        selected = if (current_ym %in% year_months) current_ym else year_months[1]
      )
    }
  })
  
  # Filter data by selected year-month (Monthly Expenses tab)
  filtered_data <- reactive({
    data <- budget_data()
    
    if (is.null(data) || nrow(data) == 0 || is.null(input$year_month)) {
      return(NULL)
    }
    
    data %>% filter(year_month == input$year_month)
  })
  
  # Filter data by selected year-month (All Entries tab)
  entries_filtered_data <- reactive({
    data <- budget_data()
    
    if (is.null(data) || nrow(data) == 0 || is.null(input$entries_year_month)) {
      return(NULL)
    }
    
    data %>% filter(year_month == input$entries_year_month)
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
  
  # Render summary text (Monthly Expenses tab)
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
  
  # Render summary text (All Entries tab)
  output$entries_count <- renderText({
    data <- entries_filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return("Entries: 0")
    }
    paste0("Entries: ", nrow(data))
  })
  
  output$entries_total <- renderText({
    data <- entries_filtered_data()
    if (is.null(data) || nrow(data) == 0) {
      return("Total: ¥0")
    }
    paste0("Total: ¥", format(sum(data$amount), big.mark = ","))
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
  
  # Render entries list
  output$entries_list <- renderUI({
    data <- entries_filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(div(
        style = "text-align: center; padding: 50px; color: #999;",
        h4("No entries for selected month")
      ))
    }
    
    entries <- lapply(1:nrow(data), function(i) {
      row <- data[i, ]
      
      div(
        style = paste0(
          "border: 1px solid #ddd; ",
          "border-radius: 8px; ",
          "padding: 15px; ",
          "margin-bottom: 10px; ",
          "background-color: white; ",
          "display: flex; ",
          "justify-content: space-between; ",
          "align-items: center;"
        ),
        
        div(
          style = "flex: 1;",
          div(
            style = "font-weight: bold; font-size: 16px; margin-bottom: 5px;",
            row$category
          ),
          div(
            style = "color: #666; font-size: 14px;",
            format(row$entry_datetime, "%Y-%m-%d %H:%M:%S")
          ),
          div(
            style = "color: #2c3e50; font-size: 18px; font-weight: bold; margin-top: 5px;",
            paste0("¥", format(row$amount, big.mark = ","))
          )
        ),
        
        actionButton(
          inputId = paste0("delete_", row$id),
          label = NULL,
          icon = icon("times"),
          class = "btn-danger btn-sm",
          style = "padding: 8px 12px;",
          onclick = sprintf(
            "Shiny.setInputValue('delete_entry', %d, {priority: 'event'})",
            row$id
          )
        )
      )
    })
    
    tagList(entries)
  })
  
  # Handle delete entry
  observeEvent(input$delete_entry, {
    entry_id <- input$delete_entry
    
    showModal(modalDialog(
      title = "Confirm Delete",
      "Are you sure you want to delete this entry?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
    
    # Store the entry_id for confirmation
    session$userData$pending_delete_id <- entry_id
  })
  
  # Handle confirmed delete
  observeEvent(input$confirm_delete, {
    entry_id <- session$userData$pending_delete_id
    
    if (!is.null(entry_id)) {
      success <- delete_entry(db_path, entry_id)
      
      if (success) {
        showNotification("Entry deleted successfully", type = "message")
        refresh_trigger(refresh_trigger() + 1)
      } else {
        showNotification("Failed to delete entry", type = "error")
      }
      
      session$userData$pending_delete_id <- NULL
    }
    
    removeModal()
  })
}

# Run app
shinyApp(ui = ui, server = server)
