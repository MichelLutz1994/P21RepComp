#tools for the P21RepComp
#Author: Michel Lutz
#Date: 12.04.2022

#table styling
table_options <- function(){
  list(
    pageLength = min(nrow(df),50), 
    search = list(regex=TRUE, caseInsensitiv = FALSE),
    scroller = TRUE,
    edittable = TRUE,
    lengthChange = TRUE,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#517fb9', 'color': '#fff'});",
      "}"
    )
    
  )
}


#loads all excel files given in the file list
loadReports <- function(file_list){
  reports <- list()
  number <- 1
  for(path in file_list){
    report <- list()
    for(sheet in excel_sheets(path)){
      report[[sheet]] <- read_excel(path, sheet=sheet)
    }
    reports[[paste(sep = "", "report", number)]] <- report
    number <- number + 1
    rm(report)
  }
  return(reports)
}

# finds a substring in a tibble 
find_text_filter <- function(df, tt){
  df %>%
    filter(if_any(where(is.character), ~str_detect(.x, tt)))
  
}

#get the VersionNummbers, that maches the str as list
get_Vers_Nr <- function(reports, str){
  versNrList <-c()
  for(report in reports){
    finding <- report[["Validation Summary"]][,1] %>% find_text_filter(str)
    if(nrow(finding)==0){
      versNrList <- c(versNrList, "-NA-")
    } else {
      versNrList <- c(versNrList, finding)
    }
  }
  return(versNrList)
}

#return true if all version numbers are equal
compare_Vers_Nr <- function(list){
  every(list, function(s)(list[[1]] == s))
}

getCompareOutput <- function(reports, str){
  list <- get_Vers_Nr(reports, str)
  if(compare_Vers_Nr(list)){
    return(paste("Equal: ", "all Reports have the same ", str,  "Version Number\n", 
                 toString(list[[1]])))
  } else {
    s <- "\n"
    for(i in 1:length(list)){
      s <- paste(s, "Report", i, " ", toString(list[i]), "\n")
    }
    return(paste("Not Equal ", "following Version-Numbers occure" , s))
  }
}

#returns a dataframe with the version nummer summary
get_Vers_Summary <- function(reports){
  configuration <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Configuration")))
  define <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Define.xml:")))
  generated <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Generated")))
  cdisc <- c(compare_Vers_Nr(get_Vers_Nr(reports, "CDISC")))
  medDRA <- c(compare_Vers_Nr(get_Vers_Nr(reports, "MedDRA")))
  standard <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Standard")))
  unii <- c(compare_Vers_Nr(get_Vers_Nr(reports, "UNII")))
  medrt <- c(compare_Vers_Nr(get_Vers_Nr(reports, "MED-RT")))
  engine <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Validation Engine Version")))
  version <- c(compare_Vers_Nr(get_Vers_Nr(reports, "Software Version")))
  for (i in 1:length(reports)){
    configuration <- c(configuration, lapply(get_Vers_Nr(reports, "Configuration"), function(s) (str_replace_all(s , ".*(\\\\)", "" )))[[i]])
    if(str_detect(lapply(get_Vers_Nr(reports, "Define.xml:"), function(s) (str_replace_all(s , ".*(\\\\)", "" )))[[i]], "Not provided")){
      define <- c(define , "Not provided")
    } else {
      define <- c(define ,lapply(get_Vers_Nr(reports, "Define.xml:"), function(s) (str_replace_all(s , ".*(\\\\)", "" )))[[i]])
    }
    generated <- c(generated, lapply(get_Vers_Nr(reports, "Generated"), function(s) (str_replace_all(s , "Generated: ", "" )))[[i]])
    cdisc <- c(cdisc, lapply(get_Vers_Nr(reports, "CDISC"), function(s) (str_replace_all(s , "[a-z]|[A-Z]|[ :]", "" )))[[i]])
    medDRA <- c(medDRA, lapply(get_Vers_Nr(reports, "MedDRA"), function(s) (str_replace_all(s , "MedDRA: ", "" )))[[i]])
    standard <- c(standard, lapply(get_Vers_Nr(reports, "Standard"), function(s) (str_replace_all(s , "Standard: ", "" )))[[i]])
    unii <- c(unii, lapply(get_Vers_Nr(reports, "UNII"), function(s) (str_replace_all(s , "UNII: ", "" )))[[i]])
    medrt <- c(medrt, lapply(get_Vers_Nr(reports, "MED-RT"), function(s) (str_replace_all(s , "MED-RT: ", "" )))[[i]])
    engine <- c(engine, lapply(get_Vers_Nr(reports, "Validation Engine Version"), function(s) (str_replace_all(s , "[a-z]|[A-Z]|[ :]", "" )))[[i]])
    version <- c(version, lapply(get_Vers_Nr(reports, "Software Version"), function(s) (str_replace_all(s , "Software Version: ", "" )))[[i]])
  }
  
  
  df <- data.frame(configuration, define, generated,cdisc,medDRA,standard,
                   unii,medrt, engine, version)
  colnames <- c("All Equal ?")
  for(i in 1:length(reports)){
    colnames <- c(colnames, paste("Report", i, sep=""))
  }
  row.names(df) <- colnames
  colnames(df) <- c("Configuration", "Define.xml","Generated", "CDISC", "MedDRA", "Standart", "UNII", "MED-RT",
                    "Validation Engine Version", "Software Version")
  return(t(df))
}

################################################################################
#functions for Issue Summary df
get_all_IssueSources <- function(report){
  totalSources <- c()
  for(rn in 1:length(report)){
    issueSum <- report[[rn]][["Issue Summary"]]
    for(i in 1:length(issueSum[[1]])){
      if(!is.na(issueSum[[i,1]])){
        totalSources <- union(totalSources, issueSum[[i,1]])
      }
    }
  }
  totalSources <- totalSources[3:length(totalSources)]
  sourceList <- list()
  for(i in 1:length(totalSources)){
    sourceList <- append(sourceList, list(totalSources[[i]]))
  }
  return(sourceList)
}

get_IssuSummary <- function(reportNummer, report){
  issueSum <- report[[reportNummer]][["Issue Summary"]]
  sources <- c()
  startSources <- c()
  for(i in 1:length(issueSum[[1]])){
    if(!is.na(issueSum[[i,1]])){
      sources <- c(sources, issueSum[[i,1]])
      startSources <- c(startSources, i)
    }
  }
  startSources <- c(startSources,length(issueSum[[1]])+1) 
  sources <- sources[3:length(sources)]
  startSources <- startSources[3:length(startSources)]
  
  list <- list()
  df <- data.frame()
  for(sourceNr in 1:length(sources)){
    id <- c()
    message <- c()
    severity <- c()
    found <- c()
    for(i in (startSources[sourceNr] +1):(startSources[sourceNr+1] - 1)){
      id <- c(id, issueSum[[i,2]])
      message <- c(message, issueSum[[i,3]])
      severity <- c(severity, issueSum[[i,4]])
      found <- c(found, issueSum[[i,5]])
    }
    domain <- rep(sources[sourceNr], length(id))
    list[[sourceNr]] <- data.frame(domain, id, message, severity, found)
    #df <- rbind(df, data.frame(id, message, severity, found))
  }
  names(list) <- sources
  return(list)
}

#returns a list of lists for each report, containing a dataframe for reach domain
get_reportList <- function(reports){
  reportList <- c()
  for(i in 1:length(reports)){
    reportList[[i]] <- get_IssuSummary(i, reports)
  }
  return(reportList)
}

#combins the domain id and messege col into one sperated by | to make it possible 
#to create a union frame
combine_id_and_message <- function(reportList){
  newList <- list()
  for(i in 1:length(reportList)){
    rep <- list()
    for(sources in 1:length(reportList[[i]])){
      idmessages <- c()
      for(entry in 1:length(reportList[[i]][[sources]][[1]])){
        idmessages <- c(idmessages, paste(reportList[[i]][[sources]][[1]][[entry]], reportList[[i]][[sources]][[2]][[entry]], reportList[[i]][[sources]][[3]][[entry]], sep=" - "))
      }
      #print(idmessages)
      rep[[sources]] <- data.frame(idmessages, reportList[[i]][[sources]][[4]], reportList[[i]][[sources]][[5]])
    }
    names(rep) <- names(reportList[[i]])
    newList[[i]] <- rep
  }
  return(newList)
}

get_compare_Issue_df <- function(Source, reportList){
  unionSourceIssues <- c()
  for(i in 1:length(reportList)){
    if(Source %in%  names(reportList[[i]])){
      unionSourceIssues <- union(unionSourceIssues, reportList[[i]][[Source]][[1]])
    }
  }
  unionSourceIssues
  
  df <- data.frame(id=unionSourceIssues)
  colnames(df) <- c(Source)
  for(i in 1:length(reportList)){
    if(Source %in%  names(reportList[[i]])){
      inrep <-  unionSourceIssues %in% reportList[[i]][[Source]][[1]]
      #get the number of occurence instead of just true and false
      issueOccurenc <- c()
      for(n in 1:length(unionSourceIssues)){
        if(inrep[n]){
          issueOccurenc <- c(issueOccurenc, reportList[[i]][[Source]][[3]][n])
        } else {
          issueOccurenc <- c(issueOccurenc, "")
        }
      }
    } else {
      issueOccurenc <- rep("", length(unionSourceIssues))
    }
    df <- cbind(df, issueOccurenc)
    colnames(df) <- c(colnames(df)[1:(ncol(df)-1)], paste("Rep", i, sep=""))
  }
  return(df)
}

get_Issue_Summary_df <- function(report, reportList){
  df <- data.frame()
  colnames <- c("ID-Message")
  for(i in 1:length(report)){
    colnames <- c(colnames, paste("Rep", i, sep=""))
  }
  issueSourceList <- get_all_IssueSources(report)
  for(issue in 1:length(issueSourceList)){
    df <- rbind(df ,c(issueSourceList[[issue]], rep("", length(report))))
    colnames(df) <- colnames
    issuedf <- get_compare_Issue_df(issueSourceList[[issue]], reportList)
    colnames(issuedf) <- colnames
    df <- rbind(df, issuedf)
    df <- rbind(df ,c(rep("", length(report)+1)))
  }
  return(df)
}

flattentibbleslist <- function(list){
  if(is_empty(list)){
    return(c("na"))
  }
  l <- c()
  for(i in 1:length(list)){
    l <- c(l , list[[i]])
  }
  return(l)
}
################################################################################
#functions for DatasetSummary
get_all_reports_with_dataset <- function(reports){
  list <- c()
  for(i in 1:length(reports)){
    if(!is.null(reports[[i]][["Dataset Summary"]])){
      list <- c(list , i)
    }
  }
  reportsList <- c()
  for(i in 1:length(list)){
    reportsList[[i]] <- reports[[list[i]]][["Dataset Summary"]]
  }
  return(reportsList)
}

get_row_processed <- function (df){
  begin_processed <- 0
  for(i in 1:nrow(df[[1]])){
    if(!is.na(df[[1]][[1]][i]) && df[[1]][[1]][i] == "Processed Sources" ){
      begin_processed <- i
    }
  }
  begin_processed
}

get_row_unprocessed <- function (df){
  begin_unprocessed <- 0
  for(i in 1:nrow(df[[1]])){
    if(!is.na(df[[1]][[1]][i]) && df[[1]][[1]][i] == "Unprocessed Sources" ){
      unbegin_processed <- i
    }
  }
  unbegin_processed
}

get_processed_df <- function(report){
  return(filter(report[[1]], between(row_number(), get_row_processed(report)+1 , get_row_unprocessed(report)-2)))
}

get_unprocessed_df <- function(report){
  return(filter(report[[1]], between(row_number(), get_row_unprocessed(report)+1 , nrow(report[[1]])-2)))
}

get_set_processed_sources <- function(list){
  processed_sources_set <- c()
  for(i in 1:length(list)){
    processed_sources_set <- union(processed_sources_set, get_processed_df(list[i])[[1]])
  }
  return(processed_sources_set)
} 

get_set_unprocessed_sources <- function(list){
  unprocessed_sources_set <- c()
  for(i in 1:length(list)){
    unprocessed_sources_set <- union(unprocessed_sources_set, get_unprocessed_df(list[i])[[1]])
  }
  return(unprocessed_sources_set)
} 

get_processed_dataset_df_rep <- function(list, repNum){
  processed_sources_set <- get_set_processed_sources(list)
  processed_df <- get_processed_df(list[repNum])
  Source <- c()
  Records <- c()
  Rejects <- c()
  for(i in 1:length(processed_sources_set)){
    if (processed_sources_set[i] %in% processed_df[[1]]) {
      Source  <- c(Source,   processed_df[[4]][i])
      Records <- c(Records,  processed_df[[5]][i])
      Rejects <- c(Rejects,  processed_df[[6]][i])
    } else {
      Source  <- c(Source,  "")
      Records <- c(Records, "")
      Rejects <- c(Rejects, "")
    }
  }
  Source <- Source[2:length(Source)]
  Records <- Records[2:length(Records)]
  Rejects <- Rejects[2:length(Rejects)]
  Domain <- processed_sources_set[2:length(processed_sources_set)]
  return(data.frame(Domain, Source, Records, Rejects))
}

get_unprocessed_dataset_df_rep <- function(list, repNum){
  unprocessed_sources_set <- get_set_unprocessed_sources(list)
  unprocessed_df <- get_unprocessed_df(list[repNum])
  Source <- c()
  Records <- c()
  Rejects <- c()
  for(i in 1:length(unprocessed_sources_set)){
    if (unprocessed_sources_set[i] %in% unprocessed_df[[1]]) {
      Source  <- c(Source,   unprocessed_df[[4]][i])
      Records <- c(Records,  unprocessed_df[[5]][i])
      Rejects <- c(Rejects,  unprocessed_df[[6]][i])
    } else {
      Source  <- c(Source,  "")
      Records <- c(Records, "")
      Rejects <- c(Rejects, "")
    }
  }
  Source <- Source[2:length(Source)]
  Records <- Records[2:length(Records)]
  Rejects <- Rejects[2:length(Rejects)]
  Domain <- unprocessed_sources_set[2:length(unprocessed_sources_set)]
  return(data.frame(Domain, Source, Records, Rejects))
}

dataset_processed_first_try <- function(list) {
  result_df <- data.frame(rep("", 5))
  for (i in 1:length(list)) {
    df  <- get_processed_dataset_df_rep(list, i)
    headerdf <- data.frame(c(""), c(""), c(paste("Report", i)), c(""))
    colnames(headerdf) <- colnames(df)
    df <- rbind(headerdf, df)
    result_df <- cbind(result_df, df[2:ncol(df)])
  }
  result_df <- cbind(rbind(c(""), get_processed_dataset_df_rep(list, i)[1:1]), result_df[2:ncol(result_df)])
  names <- c("Domain")
  for(i in 1:((ncol(result_df)-1) / 3)){
    names <- c(names , paste("Source Rep", i, sep=""), paste("Records Rep", i, sep=""), paste("Rejects Rep", i, sep=""))
  }
  colnames(result_df) <- names
  
  return(result_df)
}

dataset_unprocessed_first_try <- function(list) {
  result_df <- data.frame(rep("", nrow(get_unprocessed_dataset_df_rep(list, 1))+1))
  for (i in 1:length(list)) {
    df  <- get_unprocessed_dataset_df_rep(list, i)
    headerdf <- data.frame(c(""), c(""), c(paste("Report", i)), c(""))
    colnames(headerdf) <- colnames(df)
    df <- rbind(headerdf, df)
    result_df <- cbind(result_df, df[2:ncol(df)])
  }
  result_df <- cbind(rbind(c(""), get_unprocessed_dataset_df_rep(list, i)[1:1]), result_df[2:ncol(result_df)])
  
  names <- c("Domain")
  for(i in 1:((ncol(result_df)-1) / 3)){
    names <- c(names , paste("Source Rep", i, sep=""), paste("Records Rep", i, sep=""), paste("Rejects Rep", i, sep=""))
  }
  colnames(result_df) <- names
  return(result_df)
}

get_union_last_row <- function(list){
  res_row <- filter(list[1][[1]], between(row_number(), nrow(list[1][[1]]), nrow(list[1][[1]])))[c(1,4,5,6)]
  for(i in 2:length(list)){
    res_row <- c(res_row, filter(list[i][[1]], between(row_number(), nrow(list[i][[1]]), nrow(list[i][[1]])))[4:6])
  }
  res <- c()
  for(i in 1:length(res_row)){
    res <- c(res, res_row[[i]])
  }
  return(res)
}

get_all_DatasetsSummary <- function(list){
  df_pro <- dataset_processed_first_try(list)
  df_un <- dataset_unprocessed_first_try(list)
  headercol <- c("processed", rep("", ncol(df_pro)-1))
  middlecol1 <- rep("", ncol(df_pro))
  middlecol2 <- c("unprocessed", rep("", ncol(df_pro)-1))
  lastcol1 <- rep("", ncol(df_pro))
  lastcol2 <- get_union_last_row(list)
  df_res <- rbind(headercol, df_pro, middlecol1, middlecol2, df_un, lastcol1, lastcol2)
  
  names <- c("Domain")
  for(i in 1:((ncol(df_res)-1) / 3)){
    names <- c(names , paste("Source Rep", i, sep=""), paste("Records Rep", i, sep=""), paste("Rejects Rep", i, sep=""))
  }
  colnames(df_res) <- names
  return(df_res)
}

################################################################################
#functions for Details
get_all_reports_with_details <- function(reports){
  list <- c()
  for(i in 1:length(reports)){
    if(!is.null(reports[[i]][["Details"]])){
      list <- c(list , i)
    }
  }
  reportsList <- c()
  for(i in 1:length(list)){
    reportsList[[i]] <- reports[[list[i]]][["Details"]]
  }
  return(reportsList)
}
#returns a list with all domains occuring in the reports
get_domain_set <- function(list){
  domain_set <- c()
  for(i in 1:length(list)){
    domain_set <- union(domain_set, unique(list[i][[1]][[1]]))
  }
  return(domain_set)
} 
#returns a list with an entry for each report of a domain 
get_domain_list <- function(list, domain) {
  df_list <- c()
  for(i in 1:length(list)){
    df <- list[[i]] %>% filter(Domain == domain)
    if(nrow(df) > 0){
      df_1 <- df[, c("Record", "Count","Variables", "Values", "Pinnacle 21 ID")]
      set_list <- c()
      for(r in 1:nrow(df)){
        #add $||$ as separator symbol, should never occour in the report
        set_list <- c(set_list, paste(df[[r,"Variables"]], "###", df[[r,"Values"]],"###", df[[r,"Pinnacle 21 ID"]]) )
      }
      df_2 <- cbind(df_1, set_list)
      df_list[[i]] <- df_2
    }
  }
  return(df_list)
}
#returns the set with all entrys, must be of the same domain
get_entry_set <- function(domain_list){
  entry_set <- c()
  for(i in 1:length(domain_list)){
    entry_set <- union(entry_set, domain_list[[i]][[6]])
  }
  return(entry_set)
}
#function make the entry_set to a df with the default coloms Variables, Values, Pinnacle 21 ID
get_default_details_df <- function(entry_set){
  df <- data.frame()
  for(i in 1:length(entry_set)){
    # there are cases with more then one value in a cell, this are speratet with a , 
    # therefore the regex seperates only such entrys
    new_row <- c((str_split(entry_set[i], "###"))[[1]], entry_set[i])  
    df <- rbind(df, new_row)
  }
  return(df)
}
#retunrs a domain sepcific df !CAVE default_details_domain_ ist wrong
get_domain_comp_df <- function(domain_list){
  entry_set <- get_entry_set(domain_list)
  df <- get_default_details_df(entry_set)
  for(l in 1:length(domain_list)){
    record_list <- c()
    count_list  <- c() 
    for(i in 1:length(entry_set)){
      if(entry_set[i] %in% domain_list[[l]][[6]]){
        row <- domain_list[[l]] %>% filter(set_list == entry_set[i])
        # there could be more than one row in row, when a entry occurs more then once
        # in the report -> this paste(as.l ... ) makes them to one entry mit | as sep
        record_list <- c(record_list, paste(as.list(row[1])[[1]], collapse = " | "))
        count_list  <- c(count_list,  paste(as.list(row[2])[[1]], collapse = " | ")) 
      } else {
        record_list <- c(record_list, "")
        count_list  <- c(count_list, "")
      }
    }
    df_rep <- data.frame(record_list, count_list)
    col_names <- c(paste("Record_Rep", l, sep=""), paste("Count_Rep", l, sep=""))
    colnames(df_rep) <- col_names
    df <- cbind(df, df_rep)
  }
  df <- select(df, -4)
  colname_list <- c("Variables", "Values", "P21 ID")
  for(i in 1:length(domain_list)){
    colname_list <- c(colname_list, paste("Record_Rep", i, sep=""), paste("Count_Rep", i, sep=""))
  }
  colnames(df) <- colname_list
  return(df)
}

################################################################################
#functions for changing the existing Reports
#uses openxlsx Package, some functions need RTools, by now is it not clear if
#this code requires them

#adds the domain value in each first row of a dataset
correct_domain <- function(df) {
  currentdomain <- df[1, 1]
  for (row in 2:nrow(df)) {
    if (is.na(df[row, 1])) {
      df[row, 1] <- currentdomain
    } else {
      currentdomain <- df[row, 1]
    }
  }
  return(df)
}

#get the two issue sheet that will be merged
loadReports_merge <- function(datapath){
  df1 <- read.xlsx(datapath[1], sheet = "Issue Summary", cols=c(1, 2, 3,4,5,6,7))
  df1 <- correct_domain(df1)
  df2 <- read.xlsx(datapath[2], sheet = "Issue Summary", cols=c(1, 2, 3,4,5,6,7))
  df2 <- correct_domain(df2)
  return(reports <- list(df1, df2))
}

#returns the data of creation 
get_creation_data <- function(path){
  date <- read.xlsx(path, sheet = "Validation Summary", cols=c(1), rows = c(5))
  return(parse_datetime(substr(colnames(date),12, nchar(colnames(date)))))
}

#adds a merged col with the col1-col3
add_merge_col <- function(df){
  merge_col <- c()
  for(i in 1:nrow(df)){
    merge_col <- c(merge_col, paste(df[i,1],df[i,2],df[i,3],sep="|"))
  }
  return(df <- cbind(df,merge_col))
}

#returns the younger of two report issue sheets
get_younger_report <- function(df1, df2, datapath){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(df1)
  }else{
    return(df2)
  }
}

#returns the filename of the younger of two reports
get_younger_fileName <- function(df1, df2, datapath, names){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(names[1])
  }else{
    return(names[2])
  }
}

#returns the filename of the older of two reports
get_older_fileName <- function(df1, df2, datapath, names){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(names[2])
  }else{
    return(names[1])
  }
}

#returns the path of the older of two reports
get_younger_path <- function(df1, df2, datapath){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(datapath[1])
  }else{
    return(datapath[2])
  }
}

#returns the path of the older of two reports
get_older_path <- function(df1, df2, datapath){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(datapath[2])
  }else{
    return(datapath[1])
  }
}

#returns the younger of two report issue sheets
get_older_report <- function(df1, df2, datapath){
  if(get_creation_data(datapath[1])>get_creation_data(datapath[2])){
    return(df2)
  }else{
    return(df1)
  }
}

#returns a dataframe with the to merged comment cols
get_merged_comments <- function(dfy, dfo) {
  dfy <- add_merge_col(dfy)
  dfo <- add_merge_col(dfo)
  merged_col1 <- c()
  merged_col2 <- c()
  for (i in 1:nrow(dfy)) {
    #check if the row contains the same entry
    if (dfy$merge_col[i] %in% dfo$merge_col) {
      if (is.na(dfy[i, 6])) {
        #young has no entry -> use the old entry
        merged_col1  <- c(merged_col1, dfo[i, 6])
        merged_col2 <- c(merged_col2, "")
      } else if (is.na(dfo[i, 6])) {
        #old has no entry -> use the young entry
        merged_col1  <- c(merged_col1, dfy[i, 6])
        merged_col2 <- c(merged_col2, "")
      } else {
        #merge
        merged_col1 <- c(merged_col1, dfy[i, 6])
        merged_col2 <- c(merged_col2, dfo[i, 6])
      }
    }
  }
  return(df_merge <- data.frame(merged_col1, merged_col2))
}

#Decapitated: returns a default data frame with just the first 3 cols
get_default_union_df <- function(dfy, dfo) {
  dfy <- add_merge_col(dfy)
  dfo <- add_merge_col(dfo)
  unionrows <- union(dfy$merge_col, dfo$merge_col)
  domain <- c()
  p21ID <- c()
  message <- c()
  for (row in unionrows) {
    split <- str_split(row, "\\|")[[1]]
    domain <- c(domain, split[1])
    p21ID <- c(p21ID, split[2])
    message <- c(message, split[3])
  }
  dfs <- data.frame("Domain" = domain,
                    "P21ID" = p21ID,
                    "Message" = message,
                    "union" = unionrows)
  return(dfs)
}

#add the found values of old to the data.frame
get_default_show_df <- function(dfy, dfo){
  dfy <- add_merge_col(dfy)
  dfo <- add_merge_col(dfo)
  foundOld <- c()
  for (i in 1:nrow(dfy)) {
    if (dfy$merge_col[i] %in% dfo$merge_col) {
      foundOld <- c(foundOld, (dfo %>% filter(merge_col == dfy$merge_col[i]))[[5]])
    } else {
      foundOld <- c(foundOld, "")
    }
  }
  dfs <- cbind(dfy, "Found\nRepOld" = foundOld)[, c(1, 2, 3, 5, 8)]
  return(dfs)
}

#returns the df to show, needs the default_show_df and the merge_df
get_show_df <- function(dfds, merge_df, mode){
  dfs <- cbind(dfds, merge_df)
  df <- dfs[c(3:nrow(dfs)),]
  if(mode == "young"){ 
    colnames(df) <- c("Domain", "P21ID", "Message", "Found\nRepYoung", "Found\nRepOld", "Commentar\nYoung", "Commentar\nOld")
  } else {
    colnames(df) <- c("Domain", "P21ID", "Message", "Found\nRepOld", "Found\nRepYoung", "Commentar\nOld", "Commentar\nYoung")
    }
  
  return(df)
}

#write the new report based on the one with the orginal path to the outputpath
write_new_report <- function(orginalPath, reports_merge, mode ,outputpath, datapath){
  df1 <- reports_merge[[1]]
  df2 <- reports_merge[[2]]
  dfy <- get_younger_report(df1, df2, datapath)
  dfo <- get_older_report(df1, df2, datapath)
  if(mode == "younger"){
    merge_df <- get_merged_comments(dfy, dfo)
  } else {
    merge_df <- get_merged_comments(dfo, dfy)
  }
  wb <- loadWorkbook(orginalPath)
  for(i in 1:length(merge_df[,1])){
    writeData(wb, sheet="Issue Summary", x=merge_df[i,1], startCol=6, startRow = (i+2))
    writeData(wb, sheet="Issue Summary", x=merge_df[i,2], startCol=7, startRow = (i+2))
  }
  saveWorkbook(wb, outputpath, overwrite = TRUE)
}

