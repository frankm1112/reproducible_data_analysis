library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(viridis)
library(shinydashboard)

ui <- fluidPage(
  ### Averaging Input Panel
  column(3, offset = 1,
         actionButton("add.entry", "Add a New Table"),
         uiOutput("add.data.tables"),
         actionButton("data.type", "Update")
  )

)

server <- function(input, output)({
  
  ### Average panel output 1
  output$Replicate_Table_1 <- renderTable({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
    long_data <- pivot_longer(data_1,
                              cols = !contains('Sample'),
                              names_to = "Time_Points",
                              values_to = "Number"
    )
    
    ### ggplot takes many togglable options from the user 
    ### this allows the user to format the axis titles , Graph title,
    ### aixs labels, and overall color scheme. All color schemes are cupposed to be color blind friendly
    long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
    back_to_wide <- pivot_wider(long_data_final,
                                names_from = contains('Sample'),
                                values_from = contains('Number'))
    
    CBA_replicates <- back_to_wide %>%
      select(contains(input$average_1))
    CBA_average <- as.data.frame(rowMeans(CBA_replicates)) %>%
      rename(
        Average = 'rowMeans(CBA_replicates)'
      )
    CBA_all <- cbind(CBA_replicates, CBA_average)
    
    input.count <- NULL
    
    observeEvent(input$add.entry, {
      if(is.null(input.count)){
        input.count <- 1
      } 
      else {
        input.count <- c(input.count, max(input.count)+1)
      }
      output$add.inputs <- renderUI({
        apply(1:length(input.count
                       function(i){
                         textInput(paste())
                       }))
        
      })
    })
    observeEvent(input$Update, {
      if (is.null(input.count)){
        output$add.data.tables <- renderPrint("No Data has been specified. 
                                              Please use a common string from
                                              samples you would like to observe
                                              (i.e. CB-A, if you have CB-A #1,
                                              CB-A #2, and CB-A #3).")
      } else {
        
        # Get ids for textboxes
        txtbox_ids <- sapply(1:length(input.count),function(i){
          paste("txtInput",ids[i],sep="")
        })
        
        # Get values
        for(i in 1:length(txtbox_ids)){
          out[[i]] <- sprintf("Txtbox #%d has value: %s",i,input[[ txtbox_ids[i] ]])
        }
        output$txtOut <- renderPrint({out})
        
        data.tables <- renderTable({
          data <- input$data_file
          ext <- tools::file_ext(data$datapath)
          
          req(data)
          validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
          data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
          long_data <- pivot_longer(data_1,
                                    cols = !contains('Sample'),
                                    names_to = "Time_Points",
                                    values_to = "Number"
          )
          
          ### ggplot takes many togglable options from the user 
          ### this allows the user to format the axis titles , Graph title,
          ### aixs labels, and overall color scheme. All color schemes are cupposed to be color blind friendly
          long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
          back_to_wide <- pivot_wider(long_data_final,
                                      names_from = contains('Sample'),
                                      values_from = contains('Number'))
          
          CBA_replicates <- back_to_wide %>%
            select(contains(input$average_1))
          CBA_average <- as.data.frame(rowMeans(CBA_replicates)) %>%
            rename(
              Average = 'rowMeans(CBA_replicates)'
            )
          CBA_all <- cbind(CBA_replicates, CBA_average)
      }
    })
      
      ### 1. Generate a button that adds new slots.
      ### 2. Have slots feed into a list.
      ### 3. Take values from list and feed them into table generation.
      observeEvent(input$add.entry, {
        
      })
  })})})
  


shinyApp(ui = ui, server = server)