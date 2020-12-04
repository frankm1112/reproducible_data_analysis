####
####
####_____________ Data Analysis tool_____________
####
#### Contributors: Frank May, Trystan Bordeau, Haley Dylewski
####
####
#### Description: This shiny app serves as a tool for analysis of time series data.
#### Given a user input data file of appropriate formatting, the app will perform
#### statistical analysis and generate a plot. Options for both can be set by the user.
#### 
####
#### Disclaimer: This app was created for future use as a data analysis tool for
#### data commonly generated in Microbiology labs. There are many functions that
#### were not able to be implemented in the given time frame such as: 2 Way ANOVA
#### proper, clean plotting of statistics, downloadable tables and plots

#____________________________ App code start _______________________

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(viridis)
library(emmeans)
library(rstatix)



#__________________________________________________________________________
#                               Ui 
#__________________________________________________________________________

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
      #______________________ Dashboard content______________________
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
                )
              ) # close final row
              
      ),## close dashboard tab
      
      #______________________ Stats tab content______________________
      
      tabItem(tabName = "statistics",
              h2("Stats stuff goes here"),
              sidebarLayout(
                sidebarPanel(
                  titlePanel(
                    h4("Statistics Options", align = "center")),
                  wellPanel(
                    radioButtons("user_stat_choice","Choose Statiscal Method", c("None","T-Test","One Way ANOVA", "2 Way ANOVA")),
                    submitButton("Update")
                  )
                ),
                mainPanel(
                  tabsetPanel(      
                    tabPanel("Data Set",
                             wellPanel(
                               h4("Tests for normality: "),
                               h5("1) Shaprio-Wilks test for normality of sample groups.
                                 For few groups with many data points (>20), each group is checked indiviually.
                                 Otherwise, used model residuals."),
                               textOutput("shapiro"),
                               h5("2) Levene's test for homogeneity of variance."),
                               textOutput("levene")
                             ),
                             tableOutput("data_set"),
                    ),
                    tabPanel("Summary", 
                             wellPanel(
                               h4("Summary statistics")
                             ),
                             tableOutput("sum_stats")),
                    tabPanel("Averages",
                             wellPanel(
                               h5("Input a common string within samples of your
                                  imported '.csv' file you would like to view. If
                                  you have 3 replicates of sample type Alpha, 
                                  simply type 'Alpha' in one of the 'Data Name'
                                  boxes to view only this sample set. Ensure 
                                  that 'Alpha', or whatever other string, is in
                                  every sample you wish to observe."),
                               textInput("average_1","Data Name 1", "i.e. CB-A"),
                               textInput("average_2","Data Name 2", "i.e. CB-D"),
                               textInput("average_3","Data Name 3", "i.e. Coculture"),
                               submitButton("Update")),
                             tableOutput("Replicate_Table_1"),
                             tableOutput("Replicate_Table_2"),
                             tableOutput("Replicate_Table_3")
                    ),
                    tabPanel("Results", 
                             wellPanel(
                               h5("Test Summary"),
                               textOutput("Test"),
                               textOutput("stat_msg"),
                               textOutput("sig"),
                               textOutput("msg"),
                               
                             ),
                             dataTableOutput("stat.test")),
                    tabPanel("Post Hoc", 
                             tableOutput("main.effects"),
                             tableOutput("pwc")
                    ) 
                    
                  )
                  
                )
              )
      ),# close second tab
      
      #______________________ Plots tab content______________________
      
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
                          radioButtons("user_plot_type", "Select Plot Type", c("Line","Box Plot"), "Box Plot"),
                          radioButtons("user_stats_graph", "Include stats on graph (Box plot only) ", c("None", "Pairwise")),
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

#__________________________________________________________________________
#                               Server
#__________________________________________________________________________


server <- function(input, output) {
  
  #### __________________ Example input data format table__________________ 
  
  ### Description of code:
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
  
  #### __________________ User data from csv__________________ 
  sample_data <- reactive({
    data <- input$data_file
    ext <- tools::file_ext(data$datapath)
    
    req(data)
    validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
    data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
    
    ### ---- The following loop was removed as it was causing issues with input
    ###----- but we did not want to delete it just in case :)
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
  
  ## Show formatted user input
  output$data_set <- renderTable({ 
    data <- stat_results$data_set 
  })
  ## Show summary statistics (average, standard deviation etc)
  output$sum_stats <- renderTable({
    data <- stat_results$sum_stats
  })
  
  ##  Display resulting table of the statistical test
  output$stat.test <- renderDataTable({
    data <- stat_results$stat_test
  })
  
  ## Pairwise comparisons for post hoc analysis of ANOVA
  output$pwc <- renderTable({
    data <- stat_results$pwc
  })
  
  # Was test significant
  output$sig <- renderText({
    if (stat_results$sig == TRUE){
      paste("Significance Found")
    }else{
      paste("No significance found")
    }
  })
  
  # Type of statistical analysis performed
  output$Test <- renderText({
    if (input$user_stat_choice == "None"){
      paste("No further statisics performed ")
    } else{
      paste(input$user_stat_choice," performed")
    }
  })
  
  ## Simple main effects for significant 2 Way ANOVA
  output$main.effects <- renderTable({
    if (input$user_stat_choice == "2 Way ANOVA" && stat_results$sig ==TRUE){
      data <- stat_results$meffects
    }else{data <- NULL}   
  })
  
  ### output for normality assumptions
  output$shapiro <- renderText({
    if(input$user_stat_choice != "None"){
      if(stat_results$shapiro_result == TRUE){
        paste("Passed Shapiro-Wilk's test")
      }else if(stat_results$shapiro_result == FALSE){
        paste("FAILED Shaprio-Wilk's test. Data does not meet assumption of normality")
      }
    }else{     paste("No Analysis performed")   }
  })
  
  output$levene <- renderText({
    if(input$user_stat_choice != "None"){  
      if(stat_results$lev_result ==TRUE){
        paste("Passed Levene's test")
      }else if(stat_results$lev_result == FALSE){
        paste("FAILED Levene's test. Data does not meet assumption of homogeneity of variance")
      }
    }else{     paste("No Analysis performed")   }
  })
  
  
  ################### Do computation   ###################
  ## NOTE: the option to select 2 Way ANOVA was removed but the code remains.
  ## I am not confident that the test is performed correctly as it is coded now
  ## but want to keep it here for future work.
  ##
  ## Analysis Description:
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
  
  ## output variables: (all as elements of reactive stat_results())
  ## sum_stats = summary statistics including averages, standard deviation, error
  ## outliers = outlier values
  ## shapiro_result = TRUE/FALSE Normality of data
  ## levene_result = TRUE/FALSE Homogeneity of variance
  ## stat.test = anova result
  ## pwc = pairwise comparisons post hoc or t-test
  ## significance or sig= TRUE/FALSE if any significance was found in ANOVA
  ## msg = any error or special information to report
  ## pwc_type = type of post hoc pairwise comparison done
  
  observe({
    
    # SCRATCH CODE
    # long_data_stats <- pivot_longer(sample_data(),
    #                                 cols = !contains('Sample') & !contains("Treatment"),
    #                                 names_to = "Time_points",
    #                                 values_to = "Number")
    # 
    # data_set <- mutate_if(long_data_stats, is.character, as.factor) 
    # stat_results$data_set <- data_set
    
    
    ###---------- Format data and do summary stats.----------
    ### ----------Add the results to the stat_results list----------
    
    if (input$user_stat_choice == "2 Way ANOVA"){
      long_data_stats <- pivot_longer(sample_data(),
                                      cols = !contains('Sample') & !contains("Treatment"),
                                      names_to = "Time_points",
                                      values_to = "Number")
      
      data_set <- mutate_if(long_data_stats, is.character, as.factor)
      
      
      groupings <- groupings <- c("Sample", "Treatment")## this allows groupings to be set by user if we so desir later rather than hard coded
      
      outliers <-  data_set %>%
        group_by(groupings[1], groupings[2])%>%
        identify_outliers(Number)
      sum_stats <- data_set %>%
        group_by(groupings[1], groupings[2]) %>%
        get_summary_stats(Number, type = "mean_sd")
      stat_results$sum_stats <- sum_stats
      stat_results$outliers <- outliers
      stat_results$data_set <- data_set
      data_set_length <- sum_stats$n[1]
    }else {
      long_data_stats <- pivot_longer(sample_data(),
                                      cols = !contains('Sample'),
                                      names_to = "Time_points",
                                      values_to = "Number")
      
      data_set <- mutate_if(long_data_stats, is.character, as.factor)
      
      if(input$user_stat_choice == "None"){
        stat_results$sum_stats <- NULL
        stat_results$outliers <- NULL
        stat_results$shapiro_result <- NULL
        stat_results$lev_result <- NULL
        stat_results$pwc <- NULL
        stat_results$sig <- NULL
        stat_results$stat_test <- NULL
        stat_results$stat_type <- "No analysis performed"
        stat_results$pwc_type<- NULL
        pwc_plot <- NULL
        
      } else {
        outliers <-  data_set %>%
          group_by(Time_points,Sample)%>%
          identify_outliers(Number)
        sum_stats <- data_set %>%
          group_by(Time_points,Sample) %>%
          get_summary_stats(Number, type = "mean_sd")
        
        
        stat_results$sum_stats <- sum_stats
        stat_results$outliers <- outliers
        data_set_length <- sum_stats$n[1]
      }
    }
    
    
    
    ###  --------- Check normality assumptions  ---------
    
    if (input$user_stat_choice == "None"){
      
    }else if (input$user_stat_choice == "2 Way ANOVA"){ # two ways
      if (data_set_length <= 20){
        model <- lm(Number ~ Sample*Treatment, data = data_set)
        shapiro <- shapiro_test(residuals(model))
        scheck <- shapiro$p.value
        for (i in seq_along(scheck)){
          if (scheck[i] >0.05){
            shapiro_result = TRUE
            stat_results$shapiro_result <- shapiro_result
          }else {
            shapiro_result = FALSE
            stat_results$shapiro_result <- shapiro_result
            break
          }
        }
      }else{
        group_norm <- data_set%>%
          group_by(Sample,Treatment) %>%
          shapiro_test(Number)
        gncheck <- group_norm$p
        for (i in seq_along(gncheck)){
          if (gncheck[1] >0.05){
            shapiro_result = TRUE
            stat_results$shapiro_result <- shapiro_result
          }else {
            shapiro_result = FALSE
            stat_results$shapiro_result <- shapiro_result
            break
          }
        }
      }
      levene <- data_set %>%
        levene_test(Number ~ Sample*Treatment)
      levcheck <- levene$p
      for (i in seq_along(levcheck)){
        if (levcheck[1] >0.05){
          lev_result = TRUE
          stat_results$lev_result <- lev_result
        }else {
          lev_result = FALSE
          stat_results$lev_result <- lev_result
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
          if (gncheck[i] >0.05){
            shapiro_result = TRUE
            stat_results$shapiro_result <- shapiro_result
          }else {
            shapiro_result = FALSE
            stat_results$shapiro_result <- shapiro_result
            break
          }
        }
      }else {
        model <- lm(Number ~ Sample, data = data_set)
        shapiro <- shapiro_test(residuals(model))
        scheck <- shapiro$p.value
        for (i in seq_along(scheck)){
          if (scheck[i] >0.05){
            shapiro_result = TRUE
            stat_results$shapiro_result <- shapiro_result
          }else {
            shapiro_result = FALSE
            stat_results$shapiro_result <- shapiro_result
            break
          }
        }
      }
      levene <- data_set%>%
        levene_test(Number ~ Sample)
      levcheck <- levene$p
      for (i in seq_along(levcheck)){
        if (levcheck[1] >0.05){
          lev_result = TRUE
          stat_results$lev_result <- lev_result
        }else {
          lev_result = FALSE
          stat_results$lev_result <- lev_result
          break
        }
      }
    }
    
    
    
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
    
    ## ---- Do T-test or ANOVA ----
    
    if(input$user_stat_choice == "None"){
      stat_results$stat_test <- "None"
      
    }else if(input$user_stat_choice == "T-Test"){
      stat_results$pwc_type <- "Pairwise student's T-test with p-value adjusted using Bonferroni method"
      stat.test <- data_set%>%
        group_by(Time_points)%>%
        pairwise_t_test(
          Number ~ Sample,
          p.adjust.method = "bonferroni"
        )
      pwc <- stat.test
      stat_results$stat_test <- stat.test
      stat_results$pwc <- pwc
      
    }else if(input$user_stat_choice == "One Way ANOVA"){
      
      ## perform normal anova if levin is true
      if(lev_result == TRUE){
        stat.test <- data_set %>%
          group_by(Time_points) %>%
          anova_test(Number ~ Sample)
        stat_results$stat_test <- stat.test
        
        ## determine if significant
        stat_check <- stat.test[[7]]
        significance <- significance_check(stat_check)
        stat_results$sig <- significance
        
        ## post hoc given significant ANOVA
        if (significance == TRUE){
          ## Tukey comparisons
          pwc <- data_set %>%
            group_by(Time_points) %>%
            tukey_hsd(Number ~ Sample)
          stat_results$pwc <- pwc
          stat_results$pwc_type <- "Tukey"
          
        } else { print("No significance")}
        
        ## if levene test was failed, use Welch anova
      }else if ( lev_result == FALSE){
        stat.test <- data_set %>%
          group_by(Time_points)%>%
          welch_anova_test(Number ~ Sample)
        stat_results$stat_test <- stat.test
        stat_results$stat_msg <- "Failed Levene's test, Welch ANOVA performed"
        
        ## determine if significant
        stat_check <- stat.test[[7]]
        significance <- significance_check(stat_check)
        stat_results$sig <- significance
        
        ## Games-Howell post hoc given significant ANOVA
        if (significance == TRUE){
          pwc <- data_set %>%
            group_by(Time_points)%>%
            games_howell_test(Number ~ Sample)
          stat_results$pwc <- pwc
          stat_results$pwc_type <- "Games-Howell"
        } else { stat_results$pwc <- "None" }
      }
      
      
    }else if (input$user_stat_choice == "2 Way ANOVA") {
      if(lev_result == TRUE){
        stat.test <- data_set %>% anova_test(Number ~ Sample*Treatment)
        stat_check <- stat.test[[6]]
        significance <- significance_check(stat_check)
        stat_results$sig <- significance
        stat_results$stat_test <- stat.test
        
        if (significance == TRUE){
          ###pairwise comparisons
          pwc <- data_set %>%
            group_by(Sample)%>%
            emmeans_test(Number ~ Treatment, p.adjust.method = "bonferroni")
          stat_results$pwc <- pwc
          stat_results$pwc_type <- "Emmeans test with Bonferoni p-value adjustment"
        }
        ##simple main effects
        model<- lm(Number ~ Sample*Treatment, data = data_set)
        main_eff_Treat <- data_set %>%
          group_by(Sample) %>%
          anova_test(Number ~ Treatment, error = model)
        stat_results$meffects <- main_eff_Treat
        
      }else if(significance == FALSE){
        model <- lm(Number ~ Sample*Treatment, data = data_set)
        pwc <- data_set %>%
          emmeans_test(Number ~ Treatment, p.adjust.method = "bonferroni", model = model)
        stat_results$pwc <- pwc
        stat_results$pwc_type <- "Emmeans test with Bonferoni p-value adjustment using error from model"
      }else if (lev_result == FALSE){
        stat_results$msg <-"Data failed levene test for homogeneity of variances. Consider one-way ANOVAS"
      }
    }
    
  })
  
  
  
  #__________________ Generate Plot __________________________ 
  
  
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
    ### IMPORTANT NOTE: We know the next bit of code is extremely repetitive. 
    ### It was attempted to be made more concise in several ways, such as 
    ### storing it in different locations, but the reactive functions in 
    ### Shiny kept returning errors. It was also attempted to generate a for
    ### loop, but this was unable to be completed by the deadline. In order
    ### for the code to be implemented and still useful, these three regions
    ### were hard-coded in below rather inefficiently. However, as we wanted
    ### a working representation, it was added so it is at least able to be
    ### utilized. 
    
    ### Average panel output 1
    output$Replicate_Table_1 <- renderTable({
      ### Stores data file.
      data <- input$data_file
      ### Obtains the file extension of the uploaded data file.
      ext <- tools::file_ext(data$datapath)
      
      ### Requires data extension to match the '.csv' file type, otherwise it
      ### will report and error.
      req(data)
      validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
      ### Reads the imported file and stores the data as an object.
      data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
      ### Data manipulation to ensure all imported data is in an accessible
      ### format for downstream manipulation.
      long_data <- pivot_longer(data_1,
                                cols = !contains('Sample'),
                                names_to = "Time_Points",
                                values_to = "Number"
      )
      
      long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
      back_to_wide <- pivot_wider(long_data_final,
                                  names_from = contains('Sample'),
                                  values_from = contains('Number'))
      ### Takes desired replicates from users, inserts them into a new column,
      ### then generates a new object with the 'Time', or the time points of 
      ### the imported data, and finally a new object with the average of the 
      ### the desired replicates. These are all then bound into one table that 
      ### is then returned as ouput$Replicate_Table_#.
      replicates_1 <- back_to_wide %>%
        select(contains(input$average_1))
      time_point_1 <- back_to_wide %>%
        select(contains('Time'))
      replicates_1_average <- as.data.frame(rowMeans(replicates_1)) %>%
        rename(
          Average = 'rowMeans(replicates_1)'
        )
      all_1 <- cbind(time_point_1, replicates_1, replicates_1_average)
    })
    
    ### Average panel output 2
    output$Replicate_Table_2 <- renderTable({
      ### Stores data file.
      data <- input$data_file
      ### Obtains the file extension of the uploaded data file.
      ext <- tools::file_ext(data$datapath)
      ### Requires data extension to match the '.csv' file type, otherwise it
      ### will report and error.
      req(data)
      validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
      ### Reads the imported file and stores the data as an object.
      data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
      ### Data manipulation to ensure all imported data is in an accessible
      ### format for downstream manipulation.
      long_data <- pivot_longer(data_1,
                                cols = !contains('Sample'),
                                names_to = "Time_Points",
                                values_to = "Number"
      )
      
      long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
      
      back_to_wide <- pivot_wider(long_data_final,
                                  names_from = contains('Sample'),
                                  values_from = contains('Number'))
      ### Takes desired replicates from users, inserts them into a new column,
      ### then generates a new object with the 'Time', or the time points of 
      ### the imported data, and finally a new object with the average of the 
      ### the desired replicates. These are all then bound into one table that 
      ### is then returned as ouput$Replicate_Table_2.
      replicates_2 <- back_to_wide %>%
        select(contains(input$average_2))
      time_point_2 <- back_to_wide %>%
        select(contains('Time'))
      replicates_2_average <- as.data.frame(rowMeans(replicates_2)) %>%
        rename(
          Average = 'rowMeans(replicates_2)'
        )
      all_2 <- cbind(time_point_2, replicates_2, replicates_2_average)
    })
    
    ### Average panel output 3
    output$Replicate_Table_3 <- renderTable({
      ### Stores data file.
      data <- input$data_file
      ### Obtains the file extension of the uploaded data file.
      ext <- tools::file_ext(data$datapath)
      ### Requires data extension to match the '.csv' file type, otherwise it
      ### will report and error.
      req(data)
      validate(need(ext == "csv", "Please confirm uploaded file extension is saved
                  in a '.csv' format."))
      data_1 <- read.csv(data$datapath, header = input$header, check.names = FALSE)
      ### Reads the imported file and stores the data as an object.
      long_data <- pivot_longer(data_1,
                                cols = !contains('Sample'),
                                names_to = "Time_Points",
                                values_to = "Number"
      )
      ### Data manipulation to ensure all imported data is in an accessible
      ### format for downstream manipulation.
      long_data_final <- rename(long_data, 'Sample' = contains('Sample'))
      
      back_to_wide <- pivot_wider(long_data_final,
                                  names_from = contains('Sample'),
                                  values_from = contains('Number'))
      ### Takes desired replicates from users, inserts them into a new column,
      ### then generates a new object with the 'Time', or the time points of 
      ### the imported data, and finally a new object with the average of the 
      ### the desired replicates. These are all then bound into one table that 
      ### is then returned as ouput$Replicate_Table_3.
      replicates_3 <- back_to_wide %>%
        select(contains(input$average_3))
      time_point_3 <- back_to_wide %>%
        select(contains('Time'))
      replicates_3_average <- as.data.frame(rowMeans(replicates_3)) %>%
        rename(
          Average = 'rowMeans(replicates_3)'
        )
      all_3 <- cbind(time_point_3, replicates_3, replicates_3_average)
    })
}#close server


shinyApp(ui = ui, server = server)