library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      actionButton("addInput","Add Input"),
      uiOutput("inputs"),
      actionButton("getTexts","Get Input Values")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("txtOut")
    )
  )))

server <- shinyServer(function(input,output,session){
  ### Generates blank value
  ids <- NULL
  
  observeEvent(input$addInput,{
    print(ids)
    
    ### Identifies if ids is still NULL, indicating no new Input has been added.
    if (is.null(ids)){
      ids <<- 1
      ### Indicates an input has been added and therefore generates a vector 
      ### with the value of ids, and ids plus 1.
    }else{
      ids <<- c(ids, max(ids)+1)
    }
    ### Not sure how lapply comes into play or tagList. I know that sprintf 
    ### allows it to increase '%d' as a number. If you use regular print, it 
    ### doesnt change the '%d' value to 1-infinity.
    output$inputs <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          textInput(paste0("txtInput",ids[i]), sprintf("Text Input #%d",ids[i]))
        })
      )
    })
  })
  
  observeEvent(input$getTexts,{
    ### Simply states that if it is NULL still, nothing can be generated.
    if(is.null(ids)){
      output$txtOut <- renderPrint({"No textboxes"})
      ### Generates empty list to be used later.
    }else{
      out <- list()
      
      ### I believe this takes the names of the inputs and puts them into a vector.
      txtbox_ids <- sapply(1:length(ids),function(i){
        paste("txtInput",ids[i],sep="")
      })
      
      ### I believe this takes the values from the pre-established values. And
      ### just add them to a textbox.
      for(i in 1:length(txtbox_ids)){
        out[[i]] <- sprintf("Txtbox #%d has value: %s",i,input[[ txtbox_ids[i] ]])
      }
      
      ### This simply takes the final version of 'out' renderPrints it into a
      ### new object and that output object is displayed later.
      output$txtOut <- renderPrint({out})
    }
  })
  
})

shinyApp(ui=ui,server=server)