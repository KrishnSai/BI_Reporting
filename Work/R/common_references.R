library(crosstalk)
library(ggplot2)
library(RODBCext)
library(shinythemes)
library(RODBC)
library(DT)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(reshape)
library(viridis)
library(wesanderson)
library (shinyBS)


# create the oracle connection 
connHandle <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")
querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
sqlQuery(connHandle, querystring1)

employee <-  sqlQuery(connHandle, "SELECT AGENT_NAME AS USERS FROM DIM_AGENT order by 1 asc")
days_present <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
ind_flag <- 4
