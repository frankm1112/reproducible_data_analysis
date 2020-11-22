library(shiny)
library(tidyverse)
library(ggplot2)
library(viridis)

ui <- fluidPage(
  
  ### Title panel
  
  titlePanel(
    h1("Time Point Analyses", align = "center")),
  
  ### In order to easily adjust page alignment in upcoming versions,
  ### the utilized page layout argument is 'fluidRow'. It allows for 
  ### specified page location designation via an array of arguments.
  
  fluidRow(
    
    ### Important information panel
    column(4, offset = 0,
           titlePanel(
             h4("Important Information", align = "center")),
           wellPanel(
             h5("Please follow all formatting guidelines below. Graphic
                representation provided to the right.", 
                align = "center"),
             h5("1. Ensure the first column and ONLY the first column,
                contains the string 'Sample'."),
             h5("2. Ensure all non-column and row header values are
                numeric."),
             h5("3. Ensure that uploaded file type is '.csv'."),
             h5("Failure to follow these guidelines will result in graphing
                errors.")
           )
           
    ),
    ### Sample Formatting Panel
    column(7, offset = 1,
           titlePanel(
             h4("Sample Formatting", align = "center")),
           wellPanel(
             h5("Place", align = "center"),
             h5("holder", align = "center"),
             h5("for", align = "center"),
             h5("upcoming", align = "center"),
             h5("sample", align = "center"),
             h5("formatting", align = "center"),
             h5("table", align = "center"),
             h5("example.", align = "center")
           )
    ),
    ### File Upload Panel
    column(2, offset = 1,
           titlePanel(
             h4("File Upload", align = "center")),
           wellPanel(
             fileInput("data_file",
                       "Select CSV File", 
                       accept = ".csv",
                       buttonLabel = "Browse..."),
             checkboxInput("header", "CSV Header", TRUE)
           )
    ),
    ### Table Output Panel
    column(7, offset = 1,
           tableOutput("csv.data")
    ),
    
    ### Graph Title Panel
    column(2, offset = 1,
           titlePanel(
             h4("Graph Title Features", align = "center")),
           wellPanel(
             textInput("user_graph_title", "Graph Title", "Title"),
             numericInput("user_graph_title_size","Title size", 20),
             textInput("user_title_color", "Title Color", "#666666"),
             submitButton("Update")
           )
    ),
    ### Axes Labels Panel
    column(3, offset = 1,
           titlePanel(
             h4("Axes Label Features", align = "center")),
           wellPanel(
             textInput("user_x_axis_label", "X-axis Label", "Time"),
             textInput("user_y_axis_label", "Y-axis Label", 
                       "Numeric Distribution"),
             textInput("user_axis_color", "Axis Label Color", "#666666"),
             numericInput("user_axis_size", "Axis Label Size", 15),
             numericInput("user_xylabel_size", "Axis Text", 12),
             submitButton("Update")
           )
    ),
    ### Color and Stats Panel
    column(2, offset = 1,
           titlePanel(
             h4("Graph Colors and Statistics", align = "center")),
           wellPanel(
             radioButtons("user_color_palette", "Choose Color Scheme",c("A","B","C","D"),"D"),
             radioButtons("user_stat_choice","Choose Statiscal Method", c("None","T-Test","ANOVA")),
             submitButton("Update")
           )
    ),
    ### Plot Panel
    column(8, offset = 2,
           plotOutput("csv.plot"))
  )
)



server <- function(input,output){
  
  ### This output generates a table using the uploaded ".csv" file. It
  ### first generates the object 'data' from the 'input''data_file'. The
  ### extension is then obtained using the tools::file_ext command, to 
  ### confirm the file extension is indeed ".csv". The following arguments
  ### will then require 2 things to be true.
  ### 1. The 'data' object is required, req(), to be present. This prevents
  ###    generation of an empty data plot.
  ### 2. The 'ext' object must be validated, validate(), as "csv". This 
  ###    confirms that the uploaded data must be in a '.csv' format. If it 
  ###    is not, an error message will be sent asking the user to confirm 
  ###    the file format being uploaded.
  ### Assuming these are met, 'output$csv.data' object will be stored as a 
  ### dataframe from the 'read.csv' command enacted on the uploaded file. 
  ### It will also designate the "header" as the previously mentioned
  ### 'input$header' value, ensuring these two are equivalent.
  
  output$csv.data <- renderTable({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    read.csv(data$datapath, header = input$header)
  })
  
  ### Plot progress so far. The file is successfully connected to file upload.
  ### The 'data' and 'ext' features can likely be streamlined in the future,
  ### as they are repetitive, and will be used if we integrate stats as well, 
  ### or any other tool. req() and validate() as well. 
  
  ### The first two steps are straight forward, saving the data as an object.
  ### This, again, can probably be streamlined later. It stores the uploaded
  ### file as 'data_1'. It then pivots the data to a long format, taking all
  ### input columns and shifting them into three columns (Check my hw_09 
  ### print(long_colony_counts), the output would be the same). The commands 
  ### will probably need adjusting, but essentially here are the requirements.
  ### 1. The first data column with data to be compared MUST be named 'Sample'.
  ###    There are a couple of issues here. '.csv' files can be saved several
  ###    ways. If you save it as the default, comma delimited file type, this
  ###    will not work. This is due to the first column being named
  ###    differently than it appears in a program like excel. Instead of 
  ###    saving as 'Sample' it will save as 'i..Sample'. I tried adjusting the
  ###    code to account for this but got some errors.
  
  output$csv.plot <- renderPlot({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header)
    long_data <- pivot_longer(data_1,
                              cols = !contains('Sample'),
                              names_to = "Time_Points",
                              values_to = "Number"
    )
    
    ### ggplot takes many togglable options from the user 
    ### this allows the user to format the axis titles , Graph title,
    ### aixs labels, and overall color scheme. All color schemes are cupposed to be color blind friendly
    
    ### UTF-8 formatted '.csv' files add additional characters to the first
    ### column header. This changes the format to be compatible with the 
    ### app, if necessary.
    long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
    
    ggplot(long_data_final, aes(x = Time_Points, y = Number, color = Sample, group = Sample))+
      geom_line(linetype = "solid") +
      geom_point() +
      xlab(input$user_x_axis_label) + ### x-axis label text
      ylab(input$user_y_axis_label) + ### y-axis label text
      ggtitle(input$user_graph_title) + ## Graph title text
      theme(plot.title = element_text(color = input$user_title_color, ### Graph Title format
                                      face = "bold", size = input$user_graph_title_size, hjust = 0),
            axis.title = element_text(color = input$user_axis_color, ### Axis title font and font color
                                      face = "bold", size = input$user_axis_size),
            axis.text.x = element_text(face= "plain", color = "#666666" , size = input$user_xylabel_size))+ ### Axis label size, color is currently hard coded
      scale_color_viridis(discrete = TRUE, option = input$user_color_palette) ### sets a color blind friendly color pallette
    
  })
}


shinyApp(ui = ui, server = server)