# import the libraries 
library(ggplot2)
library(dplyr)
library(DT)
library(reshape)
library(viridis)
library(wesanderson)



# create the oracle connection 
connHandle <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

# fset the date format in the database query session
querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
sqlQuery(connHandle, querystring1)

# fetch all data from  daily_noble_data_fact
data_temp  = sqlQuery(connHandle, "
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
                                       AND DF.CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'MM/DD/YYYY');",stringsAsFactors = T
                        )

# Chanege the date Oracle format to R date
data_temp$CONTACT_DATE <- as.Date(data_temp$CONTACT_DATE, "%m/%d/%Y")

# aggregating the data for the second tab
data_left_pgOne <-  data_temp %>%
                      select (.,AGENT_NAME,TOTAL_SUMMARY_IN_HRS,CONTACT_DATE) %>%
                      group_by(AGENT_NAME, CONTACT_DATE) %>% 
                      summarise(WORK_HOURS = sum(TOTAL_SUMMARY_IN_HRS, na.rm = TRUE))

data_right_pgOne <-  data_temp %>%
                      select (.,AGENT_NAME,TOTAL_CONNECTED_IN_HRS,
                                TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS,
                                TOTAL_DEASSIGN_IN_HRS,TOTAL_SUMMARY_IN_HRS,CONTACT_DATE) %>%
                      group_by(AGENT_NAME, CONTACT_DATE) %>% 
                      summarise(WORK_HOURS = sum(TOTAL_SUMMARY_IN_HRS, na.rm = TRUE))

# variable for conditional coloring at ggplot
defaulter <- "Red"
Ok <- "blue"

# ggplot to point plot the weekly performance of all the employees
ggplot(data = data_left_pgOne, aes(x = CONTACT_DATE,y = WORK_HOURS)) + 
geom_point(shape = 20,size = 5,aes(color = ifelse( WORK_HOURS <= 8 , Ok, defaulter)),
         show.legend = T
         ) +
scale_color_manual(labels = c("Default", "Success"), values = c("red", "#0072B2")) +
theme_bw(base_size = 15)  +
theme( panel.background = element_rect( linetype = 1,
                                      colour = 'black',
                                      size = 2,
                                      fill = '#e6e8ed')) +
labs(x = '', y = 'WORK_HOURS') + labs(col = "") +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
scale_x_date(date_breaks = 'day', date_labels = '%b %d\n%a')



# ggplot to generate the barplot of employee performance over the week
ggplot(data = data_left_pgOne %>% filter(AGENT_NAME == 'Arnab Kumar Das'),
       aes(x = CONTACT_DATE, y = WORK_HOURS)
       ) +
  geom_bar(fill = '#328770', col = "black", stat = "identity",  width = 1) +
  theme_bw() + theme_minimal(base_size = 15) +   labs(x = '', y = 'Working Hours') +
  labs(col = "Legend") +   geom_smooth(level = .65,se = F, colour = 'red', method = 'loess',formula  = 'y ~ x') +
  theme(panel.background = element_rect(linetype = 1, colour = 'black', size = 2,fill = '#e6e8ed')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = '', y = 'WORK_HOURS') + labs(col = "Legend")  + 
  scale_x_date(date_breaks = 'day', date_labels = '%b %d\n%a')
 
x = data_left_pgOne %>% filter(AGENT_NAME == 'Arnab Kumar Das')

over_perf <- data_temp %>% 
  select(TOTAL_CONNECTED_IN_HRS,TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS)  %>%  
  summarise_all(funs(sum))

m <-  reshape2::melt(over_perf) %>% mutate_at(vars(value), funs(./ sum(.)*100))


pie(m$value,m$variable)


# Create a basic bar
ggplot(m, aes(x="", y=value, fill=variable)) + geom_bar(stat="identity", width=1, col = 'black') +
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(value,0), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual( labels = c("Connected", "Waiting", "Paused", "Deassign"),wes.palette(n=3, name="GrandBudapest"))  +
  labs(x = NULL, y = NULL, fill = NULL, title = "Work Shares") +
  theme_bw() + theme_minimal(base_size = 15) + 
  theme(panel.background = element_rect(linetype = 1, colour = 'black', size = 2)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text=element_blank())


sum(m$value)
install.packages("wesanderson")
