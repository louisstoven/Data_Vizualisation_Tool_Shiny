library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)  
library(colourpicker)  
library(bslib)  

# UI
ui <- fluidPage(
    tags$head(
    tags$style(
      HTML("
        body {
          background-color: #cccccc;  
          color: white; 
        }
        .title-panel {
          text-align: center;  
        }
      ")
    )
  ),
  
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  
  titlePanel(
    div(
      class = "title-panel",
      "Data Visualization Tool"
    ),
    windowTitle = "Data Viz Tool"
  ),
    tabsetPanel(
    
    # Step 1: Upload CSV file
    tabPanel("Upload CSV",
             fluidRow(
               column(12, 
                      tags$div(style = "margin-bottom: 7px;"), 
                      fileInput("file1", "Upload a File",
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                                buttonLabel = "Browse",
                                placeholder = "No file selected")
               )
             ),
             br()
    ),
    
    # Step 2: Select columns
    tabPanel("Select Columns",
             fluidRow(
               column(12, 
                      tags$div(style = "margin-bottom: 7px;"), 
                      uiOutput("column_selection"),
                      actionButton("validate_columns", "Validate and Proceed", class = "btn-primary")
               )
             ),
             br()
    ),
    
    # Step 3: Visualize Data
    tabPanel("Visualize Data",
             fluidRow(
               # Plot Type and Color inputs at the top
               column(6, 
                      tags$div(style = "margin-bottom: 7px;"), 
                      selectInput("plot_type", "Plot Type",
                                  choices = c("Scatter Plot", "Histogram", "Bar Plot", "Box Plot", 
                                              "Line Plot", "Density Plot"),
                                  selectize = TRUE)
               ),
               column(6, 
                      colourInput("plot_color", "Select Plot Color", value = "darkred", showColour = "both")
               )
             ),
             
             fluidRow(
               # X and Y variable selectors on the same line
               column(6, 
                      selectInput("xvar", "Select X Variable", choices = NULL, selectize = TRUE)
               ),
               column(6, 
                      uiOutput("yvar_ui")  # Dynamic Y-axis UI controlled by plot type
               )
             ),
             
             fluidRow(
               # Option to show smoothing line
               column(6, 
                      checkboxInput("show_smooth", "Add Smoothing", value = FALSE)
               )
             ),
             br(),
             
             # The main plot output area
             plotOutput("main_plot", height = "600px")
    )
  ),
)

# Server logic
server <- function(input, output, session) {
  data <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    df  
  })
  
  # Reactive value to store selected columns (from Step 2)
  selected_columns <- reactiveVal(NULL)
  
  # Step 2: Display the column selection after the file is uploaded
  output$column_selection <- renderUI({
    req(input$file1)  # Ensure the file is uploaded
    df <- data()
    
    all_cols <- names(df)  # All columns
    
    tagList(
      checkboxGroupInput("selected_columns", "Select Columns for X and Y", 
                         choices = all_cols, selected = all_cols)
    )
  })
  
  observeEvent(input$validate_columns, {
    req(input$selected_columns)  # Ensure columns are selected
    
    selected_columns(input$selected_columns)
    updateSelectInput(session, "xvar", choices = selected_columns())
    updateSelectInput(session, "yvar", choices = selected_columns())
  })
  
  observe({
    req(input$plot_type)
    df <- data()
    
    # Get the columns that were selected in Step 2
    valid_cols <- selected_columns()
    
    # Determine the appropriate columns based on the plot type
    if (input$plot_type == "Scatter Plot") {
      valid_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_cols)
      updateSelectInput(session, "yvar", choices = valid_cols)
    } else if (input$plot_type == "Histogram") {
      valid_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_cols)
      updateSelectInput(session, "yvar", choices = NULL)  # No need for Y variable
    } else if (input$plot_type == "Bar Plot") {
      valid_x_cols <- valid_cols[sapply(df[valid_cols], function(col) is.factor(col) || is.character(col))]
      valid_y_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_x_cols)
      updateSelectInput(session, "yvar", choices = valid_y_cols)
    } else if (input$plot_type == "Box Plot") {
      valid_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_cols)
      updateSelectInput(session, "yvar", choices = valid_cols)
    } else if (input$plot_type == "Line Plot") {
      valid_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_cols)
      updateSelectInput(session, "yvar", choices = valid_cols)
    } else if (input$plot_type == "Density Plot") {
      valid_cols <- valid_cols[sapply(df[valid_cols], function(col) is.numeric(col) || inherits(col, "Date"))]
      updateSelectInput(session, "xvar", choices = valid_cols)
      updateSelectInput(session, "yvar", choices = NULL)  # No need for Y variable
    }
  })
    output$yvar_ui <- renderUI({
    req(input$plot_type)
    
    if (input$plot_type == "Density Plot") {
      # Hide Y-axis when Density Plot is selected
      shinyjs::hide("yvar")
    } else {
      # Show the Y-axis selector for other plot types
      shinyjs::show("yvar")
      return(selectInput("yvar", "Select Y Variable", choices = selected_columns()))
    }
  })
  
  # Generate the plot 
  output$main_plot <- renderPlot({
    req(input$xvar)
    
    df <- data()  # Get the uploaded data
    
    x_is_numeric_or_date <- is.numeric(df[[input$xvar]]) || inherits(df[[input$xvar]], "Date")
    y_is_numeric_or_date <- is.numeric(df[[input$yvar]]) || inherits(df[[input$yvar]], "Date")
    
    if (input$plot_type == "Scatter Plot" && x_is_numeric_or_date && y_is_numeric_or_date) {
      p <- ggplot(df, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
        geom_point(color = input$plot_color) +
        labs(x = input$xvar, y = input$yvar, title = paste("Scatter Plot of", input$xvar, "vs", input$yvar)) +
        theme_minimal()
      
      if (input$show_smooth) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue")
      }
    } else if (input$plot_type == "Histogram" && x_is_numeric_or_date) {
      p <- ggplot(df, aes(x = .data[[input$xvar]])) +
        geom_histogram(bins = 30, fill = input$plot_color, color = "white") +
        labs(x = input$xvar, y = "Frequency", title = paste("Histogram of", input$xvar)) +
        theme_minimal()
    }
    
    # Return p
    print(p)
  })
}

# Run the application
shinyApp(ui, server)
