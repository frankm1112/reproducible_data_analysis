library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(viridis)
library(shinydashboard)

ui <- dashboardPage(
  
  ### The following code generates a title for the generated 'webpage'
  ### The first argument indicates what html header to use, h1 being 
  ### the first and largest. The first variable is the title itself,
  ### followed by the 'align' command, explaining how to align the 
  ### titlePanel with the generated 'webpage'.
  
  
  # Header information
  dashboardHeader(title = "Time Point Analysis"), 
  
  # Sidebar information
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")), 
      menuItem("Statistical Analysis", tabName = "Statistical Analysis", icon = icon("line-chart")),
      menuItem("Plots", tabName = "plots", icon = icon("calendar"))
    )
  ),
  
  # Body of the website information
  dashboardBody(
    tabItem(tabName = "statistical analysis",
            h2("stats information")
    ),
    
    tabItem(tabName = "plots",
            h2("This is where plots will go")
    ),
    
    ### In order to easily adjust page alignment in upcoming versions,
    ### the utilized page layout argument is 'fluidRow'. It allows for 
    ### specified page location designation via an array of arguments
    
    ### The first column takes up '3' of a total of 12 total columns
    ### on the generated webpage. This column takes up the initial 3
    ### columns as the offset is 0 (offset does not need to be
    ### mentioned if it is 0, but is mentioned here for clarity).
    ### The wellPanel command generates a panel in this designated 
    ### area with the following objects.
    ### 1. fileInput: This argument generates a file upload tab that
    ###    allows for users to upload a desired file. "Select CSV File"
    ###    is included to inform the user, only upload a CSV file. The
    ###    input will only 'accept' this ".csv" format. Lastly, the 
    ###    buttonLabel indicates the name of the button that needs to be 
    ###    clicked to actually upload the file by the user.
    ### 2. checkboxInput: This generates a checkbox for the designated 
    ###    "header", which will be titled "CSV Header". This enablers use
    ###    of the CSV column headers opposed to default vector
    ###    designations that will appear otherwise/if the box is left
    ###    unchecked.
    
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
               h5("2. Ensure all non-row header values are numeric. This 
                    includes column headers."),
               h5("3. Ensure that uploaded file type is '.csv'."),
               h5("Failure to follow these guidelines will result in graphing
                    errors.")
             )
             
      ),
      ### Sample Formatting Panel
      column(7, offset = 1,
             titlePanel(
               h4("Data Example", align = "center")
             ),
             tableOutput("sample.table")
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
)

server <- function(input,output) {
  
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
  output$sample.table <- renderTable({
    data.frame(Sample = c('A', 'B', 'C', 'D'), 
               '60' = c(20.00, 15.00, 17.00, 21.00),
               '120' = c(45.00, 40.00, 50.00, 60.00),
               '180' = c(60.00, 58.00, 62.00, 58.00),
               '240' = c(54.00, 54.00, 57.00, 52.00),
               '300' = c(52.00, 54.00, 56.00, 50.00),
               '360' = c(53.00, 53.00, 55.00, 48.00),
               '420' = c(52.00, 54.00, 59.00, 36.00),
               check.names = FALSE)
  })  
  
  output$csv.data <- renderTable({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    read.csv(data$datapath, header = input$header, check.names = FALSE)
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
    data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
    
    ### This 'for loop' ensures that the user is only inputting the appropriate
    ### information type
    for (i in colnames(data_1)){
      data_1_columns <- c(colnames(data_1))
      if (i == data_1_columns[1]){
        next
      }
      else if (class(data_1[[i]]) != "numeric"){
        if(class(data_1[[i]]) != "integer"){
          validate(need(class(data_1[[i]]) == "numeric", "A cell that is
                      neither a row nor column header holds a non-numeric value.
                      Please ensure ALL non-row header values are numeric."))
        }
      }
      else if (sum(is.na(as.numeric(data_1_columns))) != 1) {
        validate(need(sum(is.na(as.numeric(data_1_columns))) == '[1] 1', "At least 
                      one column header is not a numeric value. Please ensure all column 
                      headers are numeric."))
      }
      else
        next
    }
    
    long_data <- pivot_longer(data_1,
                              cols = !contains('Sample'),
                              names_to = "Time_Points",
                              values_to = "Number"
    )
    
    ### ggplot takes many togglable options from the user 
    ### this allows the user to format the axis titles , Graph title,
    ### aixs labels, and overall color scheme. All color schemes are cupposed to be color blind friendly
    long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
    
    ggplot(long_data_final, aes(x = reorder(Time_Points, sort(as.numeric(Time_Points))), y = Number, color = Sample, group = Sample))+
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
