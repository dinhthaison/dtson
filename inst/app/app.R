library(shiny)
library(rhandsontable)
library(readxl)
library(haven)
library(dplyr)

# Define file types and corresponding read functions
file_types <- list(
  xlsx = readxl::read_excel,
  csv = read.csv,
  dta = haven::read_dta,
  sav = haven::read_sav,
  sas = haven::read_sas,
  rec = foreign::read.xport
)

# Define UI
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      "Upload Data",
      fileInput(
        "file", "Upload file",
        accept = paste0(".", names(file_types), collapse = ", ")
      ),
      br(),
      actionButton("reset", "Reset"),
      br(),
      textOutput("file_name"),
      downloadButton("download", "Download Data")
      
    ),
    tabPanel("View Data", tableOutput("data_table")),
    tabPanel(
      "Edit Data",
      rHandsontableOutput("hot"),
      actionButton("save", "Save changes")
    ),
    tabPanel(
      "Recode",
      selectInput("variable", "Select a variable:", choices = NULL),
      uiOutput("values"),
      br(),
      textInput("new_value", "New value:"),
      
      actionButton("update_value", "Update")
    ),
    
    
    tabPanel(
      "Create New Variable",
      selectInput("cut_var", "Select a variable to cut:", choices = NULL),
      numericInput("num_cut", "Enter the number of cuts:", value = 2, min = 2, max = 10),
      uiOutput("cut_points_ui"),
      br(),
      actionButton("create_var", "Create new variable")
    ),
    
    tabPanel(
      "Handle Missing Data",
      selectInput("missing_var", "Select a variable to handle missing data:", choices = NULL),
      selectInput("missing_method", "Select a method to handle missing data:",
                  choices = c("Drop Missing Data", "Mean Imputation", "Median Imputation")),

      actionButton("missing_update", "Update")
    )
    
    
  )
)




# Define server
server <- function(input, output, session) {
  
  data_orig <- reactiveValues()
  data_edited <- reactiveValues()
  
  observeEvent(input$file, {
    file_type <- tolower(tools::file_ext(input$file$datapath))
    if (!file_type %in% names(file_types)) {
      stop("Invalid file type!")
    }
    data_orig$df <- file_types[[file_type]](input$file$datapath)
    data_edited$df <- file_types[[file_type]](input$file$datapath)
  })
  
  output$file_name <- renderText({
    if (is.null(input$file)) {
      "No file selected"
    } else {
      input$file$name
    }
  })
  
  output$data_table <- renderTable({
    data_edited$df
  })
  
  observeEvent(input$reset, {
    data_edited$df <- data_orig$df
    output$data_table <- NULL
  })
  
  output$hot <- renderRHandsontable({
    rhandsontable(data_edited$df)
  })
  
  observeEvent(input$save, {
    data_edited$df <- hot_to_r(input$hot)
    output$data_table <- renderTable({
      data_edited$df
    })
  })
  
  
  # server phần recode 
  
  vars <- reactive({
    names(data_edited$df)
  })
  
  observe({
    updateSelectInput(session, "variable", choices = vars())
  })
  
  output$values <- renderUI({
    if (!is.null(input$variable)) {
      choices <- unique(data_edited$df[[input$variable]])
      selectInput("value", "Select a value:", choices = choices)
    }
  })
  
  observeEvent(input$add_value, {
    if (!is.null(input$variable) && !is.null(input$new_value)) {
      new_value <- input$new_value
      col_name <- input$variable
      data_edited$df[[col_name]] <- c(data_edited$df[[col_name]], new_value)
      output$data_table <- renderTable({
        data_edited$df
      })
    }
  })
  
  observeEvent(input$update_value, {
    if (!is.null(input$variable) && !is.null(input$value) && !is.null(input$new_value)) {
      new_value <- input$new_value
      col_name <- input$variable
      data_edited$df[[col_name]][data_edited$df[[col_name]] == input$value] <- new_value
      output$data_table <- renderTable({
        data_edited$df
      })
    }
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_edited$df, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$convert_to_factor, {
    if (!is.null(input$var_to_convert)) {
      data_edited$df[[input$var_to_convert]] <- as.factor(data_edited$df[[input$var_to_convert]])
      output$data_table <- renderTable({
        data_edited$df
      })
    }
  })
  
  
  
  
  # server tabpanel tạo biến   
  observe({
    updateSelectInput(session, "cut_var", choices = vars())
  })
  
  output$cut_points_ui <- renderUI({
    if (!is.null(input$cut_var)) {
      lapply(1:input$num_cut, function(i) {
        numericInput(paste0("cut_point_", i), 
                     label = paste0("Cut point ", i, ":"), 
                     value = 0, 
                     min = 0, 
                     max = 100)
      })
    }
  })
  
  observeEvent(input$create_var, {
    if (!is.null(input$cut_var)) {
      col_name <- paste0(input$cut_var, "_cut")
      cut_points <- sort(sapply(paste0("cut_point_", 1:input$num_cut), function(x) {input[[x]]}))
      data_edited$df[[col_name]] <- cut(data_edited$df[[input$cut_var]], cut_points)
      output$data_table <- renderTable({
        data_edited$df
      })
    }
  })
  
  
  # server phần missing
  
  # server phần Handle Missing Data
  observe({
    updateSelectInput(session, "missing_var", choices = vars())
  })
  
  output$missing_ui <- renderUI({
    if (!is.null(input$missing_var)) {
      choices <- c("Drop Missing Data", "Mean Imputation", "Median Imputation")
      selectInput("missing_method", "Select a method to handle missing data:", choices = choices)
    }
  })
  
  observeEvent(input$missing_update, {
    if (!is.null(input$missing_var) && !is.null(input$missing_method)) {
      var_name <- input$missing_var
      method <- input$missing_method
      
      # Count the number of missing data
      missing_count <- sum(is.na(data_edited$df[[var_name]]))
      
      # Drop missing data
      if (method == "Drop Missing Data") {
        data_edited$df <- data_edited$df[!is.na(data_edited$df[[var_name]]),]
      }
      # Mean imputation
      else if (method == "Mean Imputation") {
        mean_value <- mean(data_edited$df[[var_name]], na.rm = TRUE)
        data_edited$df[[var_name]][is.na(data_edited$df[[var_name]])] <- mean_value
      }
      # Median imputation
      else if (method == "Median Imputation") {
        median_value <- median(data_edited$df[[var_name]], na.rm = TRUE)
        data_edited$df[[var_name]][is.na(data_edited$df[[var_name]])] <- median_value
      }
      
      # Update the data table
      output$data_table <- renderTable({
        data_edited$df
      })
    }
  })
  
  
  
  
}


shinyApp(ui, server)