#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(viridis)
library(emmeans)
library(rstatix)



# Define UI for application that draws a histogram
ui <-dashboardPage(
  dashboardHeader(title = "Data Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Statistical Analysis", tabName = "statistics", icon = icon("line-chart")),
      menuItem("Plots", tabName = "plots", icon = icon("calendar"))
    )
  ), #close sidebar
  
  ## Body content
  dashboardBody(
    tabItems(
      ####### First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                ### Important information panel
                column(5, offset = 0,
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
                column(5, offset = 1,
                       titlePanel(
                         h4("Data Example", align = "center")),
                       tableOutput("sample.table")
                ),
              ),#close first row
              fluidRow(
                
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
                ### User input display Panel
                column(7, offset = 0,
                       h4("Your input data set", align = "center"),
                       tableOutput("csv.data")
                ),
                
                
              ) # close final row
              
      ),## close dashboard tab
      
      ######### Stats tab content
      tabItem(tabName = "statistics",
              h2("Stats stuff goes here"),
              fluidRow(
                ### Stats Panel
                column(2, offset = 1,
                       titlePanel(
                         h4("Statistics Options", align = "center")),
                       wellPanel(
                         radioButtons("user_stat_choice","Choose Statiscal Method", c("None","T-Test","One Way ANOVA", "2 Way ANOVA")),
                         submitButton("Update")
                       )
                )
              ),
              
              fluidRow(
                column(2, offset = 1,
                       titlePanel(
                         h4("Statistical Analysis Results", align = "center")),
                       wellPanel(
                         textOutput("output$sig")
                       ),
                       column(7,offset = 0,
                              h4("Summary statistics", align = "center"),
                              tableOutput("sum_stats")                                
                       )
                )# for output tables etc
              )
      ),# close second tab
      ######### Plots tab content
      tabItem (tabName = "plots",
               h2("This is where plots will go"),
               fluidRow(       
                 #### Graph Title Panel
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
                 column(2, offset = 1,
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
                          radioButtons("user_stats_graph", "Include stats on graph", c("Error only", "Preliminary", "Pairwise")),
                          submitButton("Update")
                        )
                 ),
                 
               ),#close first row
               fluidRow(
                 ### Plot Panel
                 column(8, offset = 2,
                        plotOutput("csv.plot"))
               )#close bottom row
               
      )# close plots tab
    )#close tabItems container 
  )#close dashboard body
)# close ui


# Define server logic 
server <- function(input, output) {
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
  
  #### create reactive element to read in csv file for use later on
  sample_data <- reactive({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
    
    ### This 'for loop' ensures that the user is only inputting the appropriate
    ### information type
    
    #  for (i in colnames(data_1)){
    #   data_1_columns <- c(colnames(data_1))
    #   if (i == data_1_columns[1]){
    #     next
    #   }
    #   else if (class(data_1[[i]]) != "numeric"){
    #     if(class(data_1[[i]]) != "integer"){
    #       validate(need(class(data_1[[i]]) == "numeric", "A cell that is
    #               neither a row nor column header holds a non-numeric value.
    #               Please ensure ALL non-row header values are numeric."))
    #     }
    #   }
    #   # else if (sum(is.na(as.numeric(data_1_columns))) != 1) {
    #   #   validate(need(sum(is.na(as.numeric(data_1_columns))) == '[1] 1', "At least
    #   #             one column header is not a numeric value. Please ensure all column
    #   #             headers are numeric."))
    #   # }
    #   else{
    #
    #   }
    # }
  })#close sample_data
  
  
  #__________________ Output table that shows user data input __________  
  
  output$csv.data <- renderTable({
    data <- sample_data()
  })
  
  
  
  # # __________________ Statistical Analysis _____________________________  
  
  ################### Create Output   ###################
  
  ### reactive list to store results of the statistical analysis
  stat_results <- reactiveValues()
  
  ## output
  output$sum_stats <- renderTable({
    data <- stat_results$sum_stats
  })
  output$stat.test <- renderTable({
    data <- stat_results$stat.test
  })
  output$pwc <- renderTable({
    data <- pwc
  })
  output$sig <- renderText({
    if (stat_results$sig == TRUE){
      paste("Significance Found")
    }else{
      paste("No significance found")
    }
  })
  output$Test <- renderText({
    if (input$user_stat_choice == "None"){
      paste("Summary statisics performed")
    } else{
      paste(input$user_stat_choice," performed")
    }
  })
  
  
  ################### Do computation   ###################
  ## 1) Data is formatted, characters are converted to factors
  ## 2) Summary statistics are generated and outliers identified
  ## 3) Analysis methods assume that data is normal with homogeneous variance.
  ##    Normality is assessed using the Shapiro-Wilks test. For few groups with
  ##    many data points (>20), normality of each group individually is assessed.
  ##    For many groups with few data points (<20), normality of all groups
  ##    together is assessed by analyzing model (Sample vs Time points) residuals.
  ##    Homoegeneity of variances is asseed by levene test.
  ##    A p- value <0.05 indicates failure of these tests.
  ##    If these tests fail, the statisical analysis will still run but a
  ##    warning will issue
  ## 4) Statistical analysis is performed based on user choice
  ##### a) No stats
  ##### b) T-Test: pairwise students T-test with bonferoni p-value adjustment
  ##### c) One-way ANOVA: Different types of ANOVA are carried out based on the results
  #####    of the levene test for homogeneity of variances
  #####       -> Passed: One-way ANOVA is done. If and significant interactions
  #####         were found, pairwise comparisons are done using Tukey's method
  #####       -> Failed: A Welch ANOVA is performed. If significant, Games-Howell
  #####         post-hoc test is performed
  ##### d) Two-way ANOVA: A two-way ANOVA is carried out using Sample and Treatment
  #####    if data passed the Levene test. Otherwise one way ANOVAS are suggested
  #####       -> if significant, post hoc pairwise comparison is computed using
  #####         emmeans_test with bonferroni p-value adjustment, and simple main
  #####         effects are computed by one-way ANOVA on data grouped by Sample
  #####       -> if not significant, pairwise comparisons are carried out using
  #####         emmeans test with error based on model
  #####
  
  ## output variables:
  ## sum_stats = summary statistics including averages, standard deviation, error
  ## outliers = outlier values
  ## shapiro_result = TRUE/FALSE Normality of data
  ## levene_result = TRUE/FALSE Homogeneity of variance
  ## stat.test = anova result
  ## pwc = pairwise comparisons post hoc or t-test
  ## significance = TRUE/FALSE if any significance was found in ANOVA
  
  
  observe({
    
    ### Format data and do summary stats.
    ### Add the results to the stat_results list
    if (input$user_stat_choice == "2 Way ANOVA"){
      long_data_stats <- pivot_longer(sample_data(),
                                      cols = !contains('Sample') & !contains("Treatment"),
                                      names_to = "Time_points",
                                      values_to = "Number")
      
      stat_results$data_set <- mutate_if(long_data_stats, is.character, as.factor)
      groupings <- groupings <- c("Sample", "Treatment")## this allows groupings to be set by user if we so desir later rather than hard coded
      
      stat_results$outliers <-  data_set %>%
        group_by(groupings[1], groupings[2])%>%
        identify_outliers(Number)
      stat_results$sum_stats <- data_set %>%
        group_by(groupings[1], groupings[2]) %>%
        get_summary_stats(Number, type = "mean_sd")
      
      
    }else {
      long_data_stats <- pivot_longer(sample_data(),
                                      cols = !contains('Sample'),
                                      names_to = "Time_points",
                                      values_to = "Number")
      
      stat_results$data_set <- mutate_if(long_data_stats, is.character, as.factor)
      
      stat_results$outliers <-  data_set %>%
        group_by(Time_points,Sample)%>%
        identify_outliers(Number)
      stat_results$sum_stats <- data_set %>%
        group_by(Time_points,Sample) %>%
        get_summary_stats(Number, type = "mean_sd")
    }
    
    
    
    ###  --------- Check normality assumptions  ---------
    data_set_length <- sum_stats$n[1]
    
    if (input$user_stat_choice == "None"){
      shapiro_result <- NULL
      lev_result <- NULL
    }else if (input$user_stat_choice == "2 Way ANOVA"){ # two ways
      if (data_set_length <= 20){
        model <- lm(Number ~ Sample*Treatment, data = data_set)
        shapiro <- shapiro_test(residuals(model))
        scheck <- shapiro$p.value
        for (i in seq_along(scheck)){
          if (scheck[i] >0.05){shapiro_result = TRUE
          }else {
            shapiro_result = FALSE
            break
          }
        }
      }else{
        group_norm <- data_set%>%
          group_by(Sample,Treatment) %>%
          shapiro_test(Number)
        gncheck <- group_norm$p
        for (i in seq_along(gncheck)){
          if (gncheck[1] >0.05){shapiro_result = TRUE
          }else {shapiro_result = FALSE
          break
          }
        }
      }
      levene <- data_set %>%
        levene_test(Number ~ Sample*Treatment)
      levcheck <- levene$p
      for (i in seq_along(levcheck)){
        if (levcheck[1] >0.05){lev_result = TRUE
        }else {lev_result = FALSE
        break
        }
      }## close 2 way ANOVA
      
      
    }else{ ##one ways
      if (data_set_length >= 20){
        group_norm <- data_set%>%
          group_by(Sample) %>%
          shapiro_test(Number)
        gncheck <- group_norm$p
        for (i in seq_along(gncheck)){
          if (gncheck[i] >0.05){shapiro_result = TRUE
          }else {
            shapiro_result = FALSE
            break
          }
        }
      }else {
        model <- lm(Number ~ Sample, data = data_set)
        shapiro <- shapiro_test(residuals(model))
        scheck <- shapiro$p.value
        for (i in seq_along(scheck)){
          if (scheck[i] >0.05){shapiro_result = TRUE
          }else {
            shapiro_result = FALSE
            break
          }
        }
      }
      levene <- data_set%>%
        levene_test(Number ~ Sample)
      levcheck <- levene$p
      for (i in seq_along(levcheck)){
        if (levcheck[1] >0.05){lev_result = TRUE
        }else {lev_result = FALSE
        break
        }
      }
    }
    
    stat_results$shapiro_result <- shapiro_result
    stat_results$lev_result <- lev_result
    
    ### --------- Perform statistical analysis  ---------
    
    ### a function to call later that checks for significance after the
    ### test is run
    significance_check <- function(x){
      sig = FALSE
      for (i in seq_along(x)){
        if(x[i] == '*'){
          sig = TRUE
        }
      }
      return(sig)
    }
    
    ### The value for significance is initially set to FALSE to prevent
    ### any accidental carryover from previous calculations
    significance = FALSE
    
    ## Do T-test or ANOVA
    
    if(input$user_stat_choice == "None"){
      print("No statistical analysis preformed")
      
    }else if(input$user_stat_choice == "T-Test"){
      print("Pairwise student's T-test with p-value adjusted using Bonferroni method")
      stat.test <- data_set%>%
        group_by(Time_points)%>%
        pairwise_t_test(
          Number ~ Sample,
          p.adjust.method = "bonferroni"
        )
      pwc <- stat.test
    }else if(input$user_stat_choice == "One Way ANOVA"){
      
      ## perform normal anova if levin is true
      if(levene_result == TRUE){
        stat.test <- data_set %>%
          group_by(Time_points) %>%
          anova_test(Number ~ Sample)
        stat.test
        ## determine if significant
        stat_check <- stat.test[[7]]
        stat_check
        significance <- significance_check(stat_check)
        
        ## post hoc given significant ANOVA
        if (significance == TRUE){
          ## Tukey comparisons
          pwc <- data_set %>%
            group_by(Time_points) %>%
            tukey_hsd(Number ~ Sample)
          
        } else { print("No significance")}
        
        ## if levene test was failed, use Welch anova
      }else if ( levene_result == FALSE){
        stat.test <- data_set %>%
          group_by(Time_points)%>%
          welch_anova_test(Number ~ Sample)
        ## determine if significant
        stat_check <- stat.test[[7]]
        significance <- significance_check(stat_check)
        
        ## Games-Howell post hoc given significant ANOVA
        if (significance == TRUE){
          pwc <- data_set %>%
            group_by(Time_points)%>%
            games_howell_test(Number ~ Sample)
        } else { print("No significance")}
      }
      
      
    }else if (input$user_stat_choice == "2 Way ANOVA") {
      if(levene_result == TRUE){
        stat.test <- data_set %>% anova_test(Number ~ Sample*Treatment)
        stat_check <- stat.test[[6]]
        significance <- significance_check(stat_check)
        if (significance == TRUE){
          ###pairwise comparisons
          pwc <- data_set %>%
            group_by(Sample)%>%
            emmeans_test(Number ~ Treatment, p.adjust.method = "bonferroni")
        }
        ##simple main effects
        model<- lm(Number ~ Sample*Treatment, data = data_set)
        main_eff_Treat <- data_set %>%
          group_by(Sample) %>%
          anova_test(Number ~ Treatment, error = model)
      }else if(significance == FALSE){
        model <- lm(Number ~ Sample*Treatment, data = data_set)
        pwc <- emmeans_test(Number ~ Treatment, p.adjust.method = "bonferoni", model = model)
        
      }else if (levene_result == FALSE){
        print("Data failed levene test for homogeneity of variances. Consider one-way ANOVAS")
      }
    }
    
    stat_results$sig <- significance
    stat_results$stat_test <- stat.test
    stat_results$pwc <- pwc
    
  })
  
  
  
  
  
  #__________________ Generate Plot __________________________ 
  
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
    data <- sample_data()
    
    long_data <- pivot_longer(sample_data(),
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
}#close server


shinyApp(ui = ui, server = server)