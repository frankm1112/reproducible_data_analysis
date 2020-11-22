library(shiny)
library(tidyverse)
library(ggplot2)

ui <- fluidPage(
  ### Page Title
  titlePanel(
    h1("Time Point Analyses", align = "center")),
  
  fluidRow(
    ### User input/control panel
    column(3, offset = 0,
           wellPanel(
             fileInput("data_file",
                       "Select CSV File", 
                       accept = ".csv",
                       buttonLabel = "Browse..."),
             checkboxInput("header", "CSV Header", TRUE),
             textInput("user_graph_title", "Graph Title", "Title"),
             textInput("x_label", "X-axis Label", "Time"),
             textInput("y_label", "Y-axis Label", "Number"),
             submitButton("Update")
           )
    ),
    ### Table output of user uploaded data.
    column(8, offset = 1,
           tableOutput("csv.data")
    ),
    ### Graphic output of user uploaded/control panel manipulated data.
    column(8, offset = 2,
           plotOutput("csv.plot"))
  )
)

server <- function(input,output){

  ### Data table output object.
  output$csv.data <- renderTable({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    read.csv(data$datapath, header = input$header)
  })
  ### Data plot output object.
  output$csv.plot <- renderPlot({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    x_axis_label <- input$x_label
    y_axis_label <- input$y_label
    
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header)
    
    ### pivot_longer does not like non-string inputs. Essentially forced
    ### to put something here as a place holder.
    long_data <- pivot_longer(data_1,
                              cols = !contains('Sample'),
                              names_to = "Time",
                              values_to = "Number")
    ### Attempt to rename data from what pivot_longer implemented to the
    ### previously objects created from user input.
    long_data_2 <- rename(long_data, x_axis_label = Time,
                          y_axis_label = Number)

    
    ggplot(long_data_2,
           aes_string(x = x_axis_label, y = y_axis_label)
           )+
      geom_point()
  })
}

### Attempt to plot based on this. So far unsuccessful in linking the
### input values to table columns and/or graph labels. If you change the 
### 'X-axis Label' and 'Y-axis Label' to say "x_axis_label" and 
### "y_axis_label" respectively, the graph output appears. Only in this 
### regard though. It is obviously not feeding into the code as it should.



shinyApp(ui = ui, server = server)