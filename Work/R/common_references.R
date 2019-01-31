
library(svDialogs)
milestone <- dlgInput("Enter number of days to view data: ", 7)$res


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
library (plotly)
library(svDialogs)

# create the oracle connection 
connHandle <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")
metaQuery <- paste("SELECT DA.AGENT_NAME,DW.WEEK_DAY,DS.SHIFT_TYPE,DF.CONTACT_DATE,TOTAL_CONNECTED_IN_HRS,
                                           TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS,
                                           TOTAL_SUMMARY_IN_HRS, TOTAL_CONNECTED_IN_MINS, TOTAL_WAITING_IN_MINS,
                                           TOTAL_PAUSED_IN_MINS, TOTAL_DEASSIGN_IN_MINS, TOTAL_SUMMARY_IN_MINS 
                                      FROM 
                                           DIM_AGENT DA,
                                           DIM_SHIFTS DS,
                                           DIM_WEEKS DW,
                                           DAILY_NOBLE_DATA_FACT DF
                                     WHERE 
                                           DF.AGENT_ID =DA.AGENT_ID
                                       AND DS.SHIFT_ID=DF.SHIFT_ID
                                       AND DW.WEEK_ID = DF.WEEK_ID
                                       AND DF.CONTACT_DATE >= TO_DATE(SYSDATE - ",milestone,", 'MM/DD/YYYY');")

# fset the date format in the database query session
querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
sqlQuery(connHandle, querystring1)

# fetch all data from  daily_noble_data_fact
data_temp  <- sqlQuery(connHandle, metaQuery,stringsAsFactors = F
                        )

employee <-  sqlQuery(connHandle, "SELECT AGENT_NAME AS USERS FROM DIM_AGENT order by 1 asc")
days_present <- unique(data_temp$WEEK_DAY)

# Chanege the date Oracle format to R date
data_temp$CONTACT_DATE <- as.Date(data_temp$CONTACT_DATE, "%m/%d/%Y")

# aggregating the data for the second tab - weekly employee performance
data_left_pgOne <-  data_temp %>%
                      select (.,AGENT_NAME,TOTAL_SUMMARY_IN_HRS,CONTACT_DATE) %>%
                      group_by(AGENT_NAME, CONTACT_DATE) %>% 
                      summarise(WORK_HOURS = sum(TOTAL_SUMMARY_IN_HRS, na.rm = TRUE))

# aggregating the data for the second tab - overall call perfrmance

KPI_perf_overall <- data_temp %>% 
  select(SHIFT_TYPE,WEEK_DAY,AGENT_NAME,TOTAL_CONNECTED_IN_HRS,TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS)  %>%  
  group_by(SHIFT_TYPE,WEEK_DAY,AGENT_NAME)  %>% summarise_all(funs(sum))

ind_flag <- 1
#melted_data <-  reshape2::melt(over_perf) %>% mutate_at(vars(value), funs(./ sum(.)*100))

close(connHandle)