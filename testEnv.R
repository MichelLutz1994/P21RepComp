# Test environment for the P21RepComp
# build to skip the shiny application

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, readxl, shiny, data.table, dplyr, stringr, openxlsx, janitor, lubridate)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("tools.R")


#rep_files <- choose.files()
dir_path <- "C:\\Users\\Michel Lutz\\OneDrive\\Documents\\Arbeit\\Mainanalytics\\R\\p21files"
file_list <- sapply(list.files(dir_path), function(s) {paste(sep="", dir_path,"\\" , s)})
reports <- loadReports(file_list)



list <- get_all_reports_with_details(reports)
domain_list <- get_domain_list(list, domain="AE")

entry_set <- get_entry_set(domain_list)
df <- get_domain_comp_df(domain_list) 



issueSources <- get_all_IssueSources(reports)
reportsList <- get_reportList(reports)
df <- combine_id_and_message(reportsList)


matrix("Report", 1, 1)


coln










#test changing a rep
wb <- loadWorkbook("C:\\Users\\Michel Lutz\\OneDrive\\Documents\\Arbeit\\Mainanalytics\\R\\p21files\\test_comment_1.xlsx")
writeData(wb, sheet="Issue Summary", x="Test", startCol=6, startRow = 3)
writeData(wb, sheet="Issue Summary", x="Test2", startCol=6, startRow = 4)
saveWorkbook(wb, "test_1.xlsx", overwrite = TRUE)
openXL("test_1.xlsx")


datapath <- c("C:\\Users\\Michel Lutz\\OneDrive\\Documents\\Arbeit\\Mainanalytics\\R\\p21files\\test_comment_1.xlsx", "C:\\Users\\Michel Lutz\\OneDrive\\Documents\\Arbeit\\Mainanalytics\\R\\p21files\\test_comment_2.xlsx")
reports_merge<- loadReports_merge(datapath)

df1 <- reports_merge[[1]]
df2 <- reports_merge[[2]]
dfy <- get_younger_report(df1, df2, datapath)
dfo <- get_older_report(df1, df2, datapath)
#dfy <- add_merge_col(dfy)
#dfo <- add_merge_col(dfo)


merge_df <- get_merged_comments(dfy, dfo)
dfds <- get_default_show_df(dfy,dfo)
df <- get_show_df(dfds, merge_df, "young")

wb <- loadWorkbook("C:\\Users\\Michel Lutz\\OneDrive\\Documents\\Arbeit\\Mainanalytics\\R\\p21files\\test_comment_1.xlsx")
for(i in 1:length(merge_df[,1])){
  writeData(wb, sheet="Issue Summary", x=merge_df[i,1], startCol=6, startRow = (i+2))
  writeData(wb, sheet="Issue Summary", x=merge_df[i,2], startCol=7, startRow = (i+2))
}
saveWorkbook(wb, "test_1.xlsx", overwrite = TRUE)
openXL("test_1.xlsx")



