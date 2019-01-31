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
library(fmsb)


# create the oracle connection 
connHandle <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

# fset the date format in the database query session
querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
sqlQuery(connHandle, querystring1)

# fetch all data from  daily_noble_data_fact
data_temp  <- sqlQuery(connHandle, "
                                    SELECT DA.AGENT_NAME,DW.WEEK_DAY,DS.SHIFT_TYPE,DF.CONTACT_DATE,TOTAL_CONNECTED_IN_HRS,
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
                                       AND DF.CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'MM/DD/YYYY');",stringsAsFactors = F
                        )



KPI_perf_overall <- data_temp %>% 
  select(SHIFT_TYPE,WEEK_DAY,AGENT_NAME,TOTAL_CONNECTED_IN_HRS,TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS)  %>%  
  group_by(SHIFT_TYPE,WEEK_DAY,AGENT_NAME)  %>% summarise_all(funs(sum))

filter_var <- list() 
group_var <-  list('SHIFT_TYPE','WEEK_DAY')
filter_val <- list()
values_var <- list('TOTAL_CONNECTED_IN_HRS',
                     'TOTAL_WAITING_IN_HRS', 
                     'TOTAL_PAUSED_IN_HRS', 
                     'TOTAL_DEASSIGN_IN_HRS')
select_var <- c(group_var,values_var)
all_shifts <- c("Morning", "Afternoon", "Night") 


 dynamic_data <- KPI_perf_overall %>% ungroup() %>%
                          select_(.dots = unlist(select_var)) %>%
                          select_(.dots = values_var) %>% 
                          summarise_all(funs(sum))

 dynamic_data <- reshape2::melt(dynamic_data, measure.vars = unlist(values_var))
 dynamic_data <- dynamic_data %>%   mutate_at(vars(value), funs(./sum(value)*100)) 

 individual_data <- KPI_perf_overall %>% ungroup() %>%
                     select_(.dots = unlist(c('AGENT_NAME',select_var))) %>%
                     filter(.,AGENT_NAME == 'Michelle Dcruz') %>%
                     select_(.dots = values_var) %>% 
                     summarise_all(funs(sum))
 
 individual_data <- reshape2::melt(individual_data, measure.vars = unlist(values_var))
 individual_data <- individual_data %>%   mutate_at(vars(value), funs(./sum(value)*100)) 
 # individual_data <- data.frame(t(individual_data))[2,]
 # colnames(individual_data) <- values_var
 # rownames(individual_data) = 1

 radarchart(rbind(rep(1,100), rep(0,100), individual_data[1,]))
 
library ('plotly')
 
ply <- plot_ly(
    type = 'scatterpolar',
    r = individual_data$value,
    theta = individual_data$variable,
    fill = 'toself'
  ) %>%
  layout(
    polar = list(
      radialaxis = list(
        visible = T,
        range = c(0,100)
      )
    ),
    showlegend = F
  )

plotly::config(ply, collaborate = F, doubleClick = F, displayModeBar = F)
