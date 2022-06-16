# P21RepComp

Tool to compare multiple P21 reports and to merge comments in the Issue
Summary table. Supports hat least P21 V3.2 and V4.0 reports.

## How to Start

Copy the app.R and the tool.R file in the same folder on your system.
Run the app.R file on your R-Console or on R-Studio. If you use a non
RStudio setting remove the comment in line 13 and commend line 11 out, to set the
working directory to the right folder, if the program does not work.

The merge comments makes use of the openxlsx package, some functions of
this package needs the RTools software, it is not completely clear if this
program requires RTools (<https://cran.r-project.org/bin/windows/Rtools/>).

## How to Use

Show & Compare Mode When the program is started, the compare mode is
automatically selected. In the second menu item "Select P21 Reports",
any number of reports can be selected using browse. The remaining
buttons have the following function.

search Fields. Each column has his own search field. Moreover there is a
general search window, to address all columns. All fields support
standard regEx expressions. If you need help, I recommend
(<https://regexr.com/>).

select Report Choose the report you want to see. Works just in the
"show" mode, that means compare: -none- If you are not completely sure
which report number is which report you can check the first entry in the
Validation Summary in the show mode. This entry is not present in the
original P21 Report, but was added to show the original file name.

select sheet Choose the sheet you want to see. Works just in the "show"
mode, that means compare: -none-

compare: Choose the sheet you want to compare. Shows the comparison
tables on the right side, if a report does not contain this sheet, it
may be that this report is not shown in the table. For many sheet you
can set a focus on specific domains with the focus on button.

###Merge Comments Mode
