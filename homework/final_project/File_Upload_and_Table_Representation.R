library(shiny)

ui <- fluidPage(
  
  ### The following code generates a title for the generated 'webpage'
  ### The first argument indicates what html header to use, h1 being 
  ### the first and largest. The first variable is the title itself,
  ### followed by the 'align' command, explaining how to align the 
  ### titlePanel with the generated 'webpage'.
  
  titlePanel(
    h1("Data Analysis", align = "center")),
  
  ### In order to easily adjust page alignment in upcoming versions,
  ### the utilized page layout argument is 'fluidRow'. It allows for 
  ### specified page location designation via an array of arguments.
  
  fluidRow(
    
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
    
    column(3, offset = 0,
           wellPanel(
             fileInput("data_file",
                       "Select CSV File", 
                       accept = ".csv",
                       buttonLabel = "Browse..."),
             checkboxInput("header", "CSV Header", TRUE)
           )
    ),
    
    ### This designates the next 8 columns, offset by 1, to be used for 
    ### the tableOutput defined by "csv.data". As columns 4-12 are vacant,
    ### the next 8 columns (offset by 1, so 5-12), will be used to plot 
    ### this table.

    column(8, offset = 1,
           tableOutput("csv.data")
    )
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
}


shinyApp(ui = ui, server = server)