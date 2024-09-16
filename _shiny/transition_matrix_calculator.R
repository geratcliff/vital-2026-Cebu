library(shiny)
library(expm)


ui <- fluidPage(
  titlePanel("Transition Probability Matrix Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_states", "Number of Health States", min = 2, max = 10, value = 5, step = 1, ticks = FALSE),
      uiOutput("state_names_input"),
      textAreaInput("rate_matrix_input", 
                    "Enter Rate Matrix (comma-separated rows):", 
                    value = paste0("0,.02,0,.001,0\n",
                                   "0,0,0.1,0.001,0\n",
                                   "0,0,0,0.001,0.02\n",
                                   "0,0,0,0,0\n",
                                   "0,0,0,0,0")),
      textInput("time_step", "Time Step", value = "1"),
      downloadButton("download", "Download Transition Probability Matrix")
    ),
    
    mainPanel(
      h4("Rate Matrix"),
      tableOutput("rate_input"),
      h4("Transition Probability Matrix"),
      tableOutput("output_matrix")
    )
  )
)

server <- function(input, output, session) {
  
  default_states <- c("H", "AD", "PD", "DOC", "DD")
  
  output$state_names_input <- renderUI({
    num_states <- input$num_states
    lapply(1:num_states, function(i) {
      default_value <- if(i <= length(default_states)) default_states[i] else paste("State", i)
      textInput(paste0("state_", i), label = paste("State", i), value = default_value)
    })
  })
  
  state_names <- reactive({
    num_states <- input$num_states
    sapply(1:num_states, function(i) input[[paste0("state_", i)]])
  })


  # Helper function to create rate matrix
  create_matrix <- reactive({
    matrix_data <- as.matrix(read.table(text = input$rate_matrix_input, sep = ","))
    validate(
      need(nrow(matrix_data) == ncol(matrix_data), "Rate matrix must be square"),
      need(nrow(matrix_data) == input$num_states, "Rate matrix size must match number of states")
    )
    rownames(matrix_data) <- state_names()
    colnames(matrix_data) <- state_names()
    matrix_data
  })
  
  # Reactive to create the transition probability matrix
  calculate_transition_matrix <- reactive({
    R <- create_matrix()
    
    # Set diagonal elements based on the sum of rows
    diag(R) <- -rowSums(R)
    
    time_step <- parse_time_step()
    P <- expm(R * time_step) # Calculate the embedded transition matrix
    
    P
  })
  
  # Output the transition probability matrix with row names
  output$output_matrix <- renderTable({
    matrix_data <- calculate_transition_matrix()
    matrix_data
  }, digits = 5, rownames = TRUE)
  
  # Display the input rate matrix with row names
  output$rate_input <- renderTable({
    matrix_data <- create_matrix()
    matrix_data
  }, digits = 5, rownames = TRUE)
  
  # Modify the download handler to include state names as row names
  output$download <- downloadHandler(
    filename = function() {
      paste("transition_matrix_time_step_", input$time_step, ".csv", sep = "")
    },
    content = function(file) {
      matrix_data <- calculate_transition_matrix()
      rownames(matrix_data) <- state_names()
      colnames(matrix_data) <- state_names()
      write.csv(matrix_data, file, row.names = TRUE)
    }
  )
  
  parse_time_step <- reactive({
    expr <- parse(text = input$time_step)
    eval(expr)
  })
}


shinyApp(ui = ui, server = server)
