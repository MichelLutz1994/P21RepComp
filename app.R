# r shiny application for comparing two or more P21 Validation Reports
# just run: run App to start
# Author: Michel Lutz
# Date: 11.04.2022
# latest Update: 12.04.2022

library(pacman)
pacman::p_load(tidyverse, readxl, shiny, shinyWidgets, DT, openxlsx)
#this file and the tools.R must be in the same folder
#set the current working dir to the destination of this file, works just in RSTudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#for not RStudio setup, uses this command instead
#setwd(getSrcDirectory()[1]) 
source("tools.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
 
  tags$head(
    tags$style(
      ".title 
            {
                background:url('mainanalytics_logo.svg');
                background-repeat: no-repeat;
                background-size: 35% 110%;
            }")),
  
  headerPanel(
    h1("P21RepComp", class = "title")),
  
    #Sidebar - functions to display the buttons and input functionality,
    #be careful some are reactive and some are not
    sidebarLayout(
        sidebarPanel(
            #choose between the functionality, Compare and merge comments
            tabsetPanel(
              #buttons for the compare modus
              tabPanel("Show & Compare",
                fileInput("file_paths", "select P21 Reports", multiple = TRUE, accept=c(".xlsx")),
                selectizeInput("repNum", "select Report", choices = NULL),
                selectInput("sheet", "select sheet", choices = 
                            c("Validation Summary","Dataset Summary",
                              "Issue Summary", "Details", "Rules")),
                selectInput("compareWhat",
                        "compare:",
                        choices = c("-none-","Summary", "Dataset Summary","Issue Summary", "Details")),
                selectizeInput("focus", "focus on:", choices = NULL)),
              #buttons for the merge modus
              tabPanel("Merge Comments",
                  fileInput("file_paths_merge", "Select two Reports", multiple = TRUE, accept=c(".xlsx")),
                  radioButtons("youngOld", "current Report based on", c("Younger Report", "Older Report")),
                  #download is to be interpreted as "save" because the app is concepted as offline
                  downloadButton("downloadData", label = "save Report", class = NULL)
                ),
              id="tabset"
              
        ), width=3),
        
        # the big beast in the mid -> shows the tables and reports
        mainPanel(
           verbatimTextOutput("compare"),
           dataTableOutput("contents")
        ), fluid=TRUE
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #reports() contains all reports that will be compared as list
    reports <- reactive({loadReports(input$file_paths$datapath)})
    
    #functions for for merge functionality, the reports_merge is used, for
    #display the merged process and to create the merged comments cols
    reports_merge <- reactive({loadReports_merge(input$file_paths_merge$datapath)})
    
    #just for merge, used to get the report, which will be the base of the merged 
    #output
    younger_path <- reactive({
      if (length(input$file_paths_merge$datapath) == 2) {
        get_younger_path(
          reports_merge()[[1]],
          reports_merge()[[2]],
          input$file_paths_merge$datapath)
      }
    })
    
    #just for merge, used to get the report, which will be the base of the merged 
    #output
    older_path <- reactive({
      if (length(input$file_paths_merge$datapath) == 2) {
        get_older_path(
          reports_merge()[[1]],
          reports_merge()[[2]],
          input$file_paths_merge$datapath)
      }
    })
    
    #just for merge, used to get the report, which will be the base of the merged 
    #output
    younger_name <- reactive({
      if (length(input$file_paths_merge$datapath) == 2) {
        get_younger_fileName(
          reports_merge()[[1]],
          reports_merge()[[2]],
          input$file_paths_merge$datapath,
          input$file_paths_merge$name)
      }
    })
    
    #just for merge, used to get the report, which will be the base of the merged 
    #output
    older_name <- reactive({
      if (length(input$file_paths_merge$datapath) == 2) {
        get_older_fileName(
          reports_merge()[[1]],
          reports_merge()[[2]],
          input$file_paths_merge$datapath,
          input$file_paths_merge$name)
      }
    })
    
    #value to select the right comparison mode
    compareWhat <- reactive({
        switch(input$compareWhat,
               "-none-" = "none",
               "Summary" = "Summary",
               "Dataset Summary" = "Dataset Summary",
               "Issue Summary" = "issueSummary",
               "Details" = "Details")
    })
    
    #value to select the right focus
    focusSummary <- reactive({input$focus})
    
    #sheet selection
    showsheet <- reactive({input$sheet})
    
    #repot selection
    reportNumber <- reactive({as.integer(input$repNum)})
    
    #shows the specified comparison
    output$compare <- renderText({
        if(is_empty(reports()) || compareWhat() == "none" || compareWhat() != "Summary"){
            return(invisible())
        } else if (compareWhat() == "Summary"){
            if (focusSummary() == "CDISC Version Number"){return(getCompareOutput(reports(), "CDISC"))}
            if (focusSummary() == "MedDRA"){return(getCompareOutput(reports(), "MedDRA"))}
            if (focusSummary() == "UNII"){return(getCompareOutput(reports(), "UNII"))}
            if (focusSummary() == "MED-RT"){return(getCompareOutput(reports(), "MED-RT"))}
            if (focusSummary() == "Validation Engine Version"){return(getCompareOutput(reports(), "Validation Engine Version"))}
            if (focusSummary() == "Software Version"){return(getCompareOutput(reports(), "Software Version"))}
            if (focusSummary() == "Standard"){return(getCompareOutput(reports(), "Standard"))}
        } else {
            return(invisible())
        }
    })
    
    #shows the specified report and sheet
    output$contents <- DT::renderDataTable({
      #check witch modus the program runs, compare or merge comments
      if(input$tabset == "Show & Compare"){
          #when no report are load, show nothing
          if(is_empty(reports()))
            return(NULL)
          #show Mode: Details
          if(compareWhat() == "none" && showsheet() == "Details" && focusSummary() != "" &&  focusSummary() != "all"){
            list <- get_all_reports_with_details(reports())
            df <- list[[reportNumber()]] %>% filter(Domain == focusSummary())
            return(datatable(df , 
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          } else 
          #show Mode: show simply the raw data sheet
          if(compareWhat() == "none"){  
            df <- reports()[[reportNumber()]][[showsheet()]]
            if(input$sheet == "Validation Summary" && !is.na(reportNumber())){
              df[1,1] <- paste("FileName:", input$file_paths$name[reportNumber()])
            }
            return(datatable(df, 
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = 20 , search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "Summary" && focusSummary() =="all"){
            df <- get_Vers_Summary(reports())
            return(datatable(df, 
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                            %>% formatStyle('All Equal ?', backgroundColor = styleEqual(c("TRUE","FALSE"), c('green', 'red'))) )
          }
          if(compareWhat() == "issueSummary" && focusSummary() =="all"){
            reportList <- get_reportList(reports())
            reportListNew <- combine_id_and_message(reportList)
            df <- get_Issue_Summary_df(reports(), reportListNew)
            return(datatable(df,
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "issueSummary" && focusSummary() %in% flattentibbleslist(get_all_IssueSources(reports()))){
            reportList <- get_reportList(reports())
            reportListNew <- combine_id_and_message(reportList)
            df <- get_compare_Issue_df(focusSummary(), reportListNew)
            return(datatable(df,
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "Dataset Summary" && focusSummary() == "processed"){
            list <- get_all_reports_with_dataset(reports())
            df <- dataset_processed_first_try(list)
            return(datatable(df, 
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "Dataset Summary" && focusSummary() == "unprocessed"){
            list <- get_all_reports_with_dataset(reports())
            df <- dataset_unprocessed_first_try(list)
            return(datatable(df,
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "Dataset Summary" && focusSummary() == "all - colorset 1"){
            list <- get_all_reports_with_dataset(reports())
            df <- get_all_DatasetsSummary(list)
            if(nrow(df) == 4){
                return(datatable(df, 
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                       %>% formatStyle('Domain', backgroundColor = 'azure')
                       %>% formatStyle(c('Source Rep1', 'Records Rep1', 'Rejects Rep1'), backgroundColor = 'aquamarine') )
            } else {
                return(datatable(df, 
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                       %>% formatStyle('Domain', backgroundColor = 'azure')
                       %>% formatStyle(c('Source Rep1', 'Records Rep1', 'Rejects Rep1'), backgroundColor = 'aquamarine')
                       %>% formatStyle(c('Source Rep2', 'Records Rep2', 'Rejects Rep2'), backgroundColor = 'cyan'))
            }
            
          }
          if(compareWhat() == "Dataset Summary" && focusSummary() == "all - colorset 2"){
            list <- get_all_reports_with_dataset(reports())
            df <- get_all_DatasetsSummary(list)
            if(nrow(df) == 4){
                return(datatable(df, 
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                       %>% formatStyle('Domain', backgroundColor = 'azure')
                       %>% formatStyle(c('Source Rep1'), backgroundColor = 'aquamarine')
                       %>% formatStyle(c('Records Rep1'), backgroundColor = 'cyan')
                       %>% formatStyle(c('Rejects Rep1'), backgroundColor = 'lightcoral'))
            } else {
                return(datatable(df,
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                       %>% formatStyle('Domain', backgroundColor = 'azure')
                       %>% formatStyle(c('Source Rep1','Source Rep2' ), backgroundColor = 'aquamarine')
                       %>% formatStyle(c('Records Rep1', 'Records Rep2'), backgroundColor = 'cyan')
                       %>% formatStyle(c('Rejects Rep1', 'Rejects Rep2'), backgroundColor = 'lightcoral'))
            }
            
          }
          if(compareWhat() == "Dataset Summary" && focusSummary() == "all"){
            list <- get_all_reports_with_dataset(reports())
            df <- get_all_DatasetsSummary(list)
            return(datatable(df,
                             filter = list(position = 'top', clear = FALSE),
                             options = list(pageLength = nrow(df), search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
          if(compareWhat() == "Details" && !(focusSummary() == "reports missing" || focusSummary() == "" || focusSummary() == "and") ){
            list <- get_all_reports_with_details(reports())
            domain_list <- get_domain_list(list, domain=focusSummary())
            entry_set <- get_entry_set(domain_list)
            df <- get_domain_comp_df(domain_list) 
            if(ncol(df) == 3){
              return(datatable(df, 
                               filter = list(position = 'top', clear = FALSE),
                               options = list(pageLength = 50, 
                                              search = list(regex=TRUE, caseInsensitiv = FALSE),
                                              filter = list(position = 'top', clear = FALSE))) 
                     %>% formatStyle(c('Variables', "Values", "P21 ID"), backgroundColor = 'azure'))
            }
            if(ncol(df) == 5){
                return(datatable(df, 
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = 50, 
                                                    search = list(regex=TRUE, caseInsensitiv = FALSE),
                                                    filter = list(position = 'top', clear = FALSE))) 
                       %>% formatStyle(c('Variables', "Values", "P21 ID"), backgroundColor = 'azure')
                       %>% formatStyle(c('Record_Rep1'), backgroundColor = 'aquamarine')
                       %>% formatStyle(c('Count_Rep1'), backgroundColor = 'cyan'))
            }
            if(ncol(df) == 7){
                return(datatable(df, 
                                 filter = list(position = 'top', clear = FALSE),
                                 options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                       %>% formatStyle(c('Variables', "Values", "P21 ID"), backgroundColor = 'azure')
                       %>% formatStyle(c('Record_Rep1','Record_Rep2' ), backgroundColor = 'aquamarine')
                       %>% formatStyle(c('Count_Rep1', 'Count_Rep2'), backgroundColor = 'cyan'))
            }
            if(ncol(df) == 9){
              return(datatable(df, 
                               filter = list(position = 'top', clear = FALSE),
                               options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                     %>% formatStyle(c('Variables', "Values", "P21 ID"), backgroundColor = 'azure')
                     %>% formatStyle(c('Record_Rep1','Record_Rep2', 'Record_Rep3' ), backgroundColor = 'aquamarine')
                     %>% formatStyle(c('Count_Rep1', 'Count_Rep2', 'Count_Rep3'), backgroundColor = 'cyan'))
            } else {
              print(ncol(df))
              return(datatable(df, 
                               filter = list(position = 'top', clear = FALSE),
                               options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))) 
                     %>% formatStyle(c('Variables', "Values", "P21 ID"), backgroundColor = 'azure')
                     %>% formatStyle(c('Record_Rep1','Record_Rep2', 'Record_Rep3' ), backgroundColor = 'aquamarine')
                     %>% formatStyle(c('Count_Rep1', 'Count_Rep2', 'Count_Rep3'), backgroundColor = 'cyan'))
              
            }
        }}
      if(input$tabset == "Merge Comments"){
        #check if exactly two reports are selected
        if(length(input$file_paths_merge$datapath) > 2 || length(input$file_paths_merge$datapath) < 2){
          return(matrix("Please select tow Reports!",1,1))
          
        }
        #issue sheet identical -> merge is not necessary, return the younger report
        if (isTRUE(dplyr::all_equal(reports_merge()[[1]], reports_merge()[[2]]))) {
            if(get_creation_data(input$file_paths_merge$datapath[1])>get_creation_data(input$file_paths_merge$datapath[2])){
              return(datatable(reports_merge()[[1]], 
                               filter = list(position = 'top', clear = FALSE),
                               options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))))
            } else {
              return(datatable(reports_merge()[[2]], 
                               filter = list(position = 'top', clear = FALSE),
                               options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))))}
        }
        #merge modus - by default uses the younger report as base and merges the older comments
        if(input$youngOld == "Younger Report"){
          df1 <- reports_merge()[[1]]
          df2 <- reports_merge()[[2]]
          dfy <- get_younger_report(df1, df2, input$file_paths_merge$datapath)
          dfo <- get_older_report(df1, df2, input$file_paths_merge$datapath)
          merge_df <- get_merged_comments(dfy, dfo)
          dfds <- get_default_show_df(dfy,dfo)
          df <- get_show_df(dfds, merge_df, "young")
          return(datatable(df, 
               filter = list(position = 'top', clear = FALSE),
               options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))))
        } else {
          df1 <- reports_merge()[[1]]
          df2 <- reports_merge()[[2]]
          dfy <- get_younger_report(df1, df2, input$file_paths_merge$datapath)
          dfo <- get_older_report(df1, df2, input$file_paths_merge$datapath)
          merge_df <- get_merged_comments(dfo, dfy)
          dfds <- get_default_show_df(dfo,dfy)
          df <- get_show_df(dfds, merge_df, "old")
          return(datatable(df, 
                           filter = list(position = 'top', clear = FALSE),
                           options = list(pageLength = 50, search = list(regex=TRUE, caseInsensitiv = FALSE))))
          }
      }
      return(NULL)
    })
    
    #download the new merged p21 report
    output$downloadData <- downloadHandler(
      filename = function() {
        if(input$youngOld == "Younger Report"){
          paste(younger_name(), sep="")
      } else {
          paste(older_name(), sep="")
      }},
      content = function(file) {
        if(input$youngOld == "Younger Report"){
          write_new_report(younger_path(), reports_merge(),"younger", file, input$file_paths_merge$datapath)
        } else {
          write_new_report(older_path(), reports_merge(),"older", file, input$file_paths_merge$datapath)
        }
      }
    )

    #for focus choice
    observe({
        if(is_empty(reports())){
            updateSelectizeInput(session, "focus", choices = ("reports missing"), server=TRUE)
        }else {
            updateSelectizeInput(session, "repNum", choices = 1:length(reports()), server=TRUE)
            if(compareWhat() == "issueSummary") { updateSelectizeInput(session, "focus", choices = c("all", flattentibbleslist(get_all_IssueSources(reports()))), server=TRUE)}
            if(compareWhat() == "Summary") { 
                choicesList <- c("all", "Standard", "CDISC Version Number", "MedDRA", "UNII", "MED-RT", "Validation Engine Version",
                             "Software Version")
                updateSelectizeInput(session, "focus", choices = choicesList, server=TRUE)}
            if(compareWhat() == "Dataset Summary") { updateSelectizeInput(session, "focus", choices = c("all" ,"all - colorset 1", "all - colorset 2", "processed", "unprocessed"), server=TRUE)}
            if(compareWhat() == "Details") { updateSelectizeInput(session, "focus", choices = get_domain_set(get_all_reports_with_details(reports())), server=TRUE)}
            if(compareWhat() == "none" && showsheet() == "Details") { updateSelectizeInput(session, "focus", choices = c("all", get_domain_set(get_all_reports_with_details(reports()))), server=TRUE)}
        }
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
