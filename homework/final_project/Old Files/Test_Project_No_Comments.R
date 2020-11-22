library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel(
    h1("Time Point Analyses", align = "center")),
  
  fluidRow(
    
    column(3, offset = 0,
           wellPanel(
             fileInput("data_file",
                       "Select CSV File", 
                       accept = ".csv",
                       buttonLabel = "Browse..."),
             checkboxInput("header", "CSV Header", TRUE),
             textInput("user_graph_title", "Graph Title", "Title"),
             textInput("x_label", "X-axis Label", "Time"),
             textInput("y_label", "Y-axis Label", "Numeric_Distribution")
           )
    ),
    
    column(8, offset = 1,
           tableOutput("csv.data")
    ),
    
    column(8, offset = 2,
           plotOutput("csv.plot"))
  )
)

server <- function(input,output){

  
  output$csv.data <- renderTable({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    read.csv(data$datapath, header = input$header)
  })
  
  output$csv.plot <- renderPlot({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    x_axis_label <- input$x_label
    y_axis_label <- input$y_label
    
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header)
    long_data <- pivot_longer(data_1,
                              cols = !contains('Sample'),
                              names_to = "Time",
                              values_to = Numeric_Distribution
    )
    rename(long_data, x_axis_label = Time)
    rename(long_data, y_axis_label = Numeric_Distribution)
    ggplot(long_data, 
           aes_string(x = x_axis_label, y = y_axis_label))+
      geom_point()
  })
}


shinyApp(ui = ui, server = server)