
# import the libraries 
library(shiny)
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

# aggregating the data for the second tab - weekly employee performance
data_left_pgOne <-  data_temp %>%
                      select (.,AGENT_NAME,TOTAL_SUMMARY_IN_HRS,CONTACT_DATE) %>%
                      group_by(AGENT_NAME, CONTACT_DATE) %>% 
                      summarise(WORK_HOURS = sum(TOTAL_SUMMARY_IN_HRS, na.rm = TRUE))

# aggregating the data for the second tab - overall call perfrmance
over_perf <- data_temp %>% 
  select(TOTAL_CONNECTED_IN_HRS,TOTAL_WAITING_IN_HRS, TOTAL_PAUSED_IN_HRS, TOTAL_DEASSIGN_IN_HRS)  %>%  
  summarise_all(funs(sum))


melted_data <-  reshape2::melt(over_perf) %>% mutate_at(vars(value), funs(./ sum(.)*100))


# variable for conditional coloring at ggplot
defaulter <- "Red"
Ok <- "blue"

filter_var <- list() 
group_var <-  list('SHIFT_TYPE','WEEK_DAY')
filter_val <- list()
values_var <- list('TOTAL_CONNECTED_IN_HRS',
                     'TOTAL_WAITING_IN_HRS', 
                     'TOTAL_PAUSED_IN_HRS', 
                     'TOTAL_DEASSIGN_IN_HRS')
select_var <- c(group_var,values_var)
all_days <- c("Sunday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Monday")
all_shifts <- c("Morning", "Afternoon", "Evening", "Night") 


server = function(input, output, session) {
  
  ######################################## START OF TAB TWO #######################################################
  
  # element 1
  output$plot1 <- renderPlot({
    
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
    
                              })

    # element 3
    output$datatable_one <- DT::renderDataTable(

                                              # render the dataframe as data table with preference of 5 results in each pagination
                                              DT::datatable(brushedPoints (data_left_pgOne,
                                                                          input$plot1_brush,
                                                                          xvar = 'CONTACT_DATE',
                                                                          yvar = 'WORK_HOURS'), options = list(paging = T,
                                                                                                                pageLength = 5,
                                                                                                                dom='tip',
                                                                                                                order = list(3,'asc')),
                                                                                                                selection = 'single'
                                                            )
                                              )

  # element 2
  output$plot2 <- renderPlot({  
    
                              data_react = data_left_pgOne %>% filter(AGENT_NAME == (input$datatable_one_cell_clicked)[3])
                              total_rows = nrow(data_react)

                              if (total_rows == 0) {
                                                    
                                                    if (input$daychoice != "All") {

                                                        filter_var <- list('WEEK_DAY')
                                                        filter_val <-  list(c(input$daychoice))

                                                      } else {

                                                                filter_val <-  list()
                                                                filter_var <- list()

                                                              }

                                                      if (input$shiftchoice != "All") {

                                                        filter_var <- c(filter_var,list('SHIFT_TYPE'))
                                                        filter_val <- c(filter_val,list(input$shiftchoice))

                                                      } else {

                                                                filter_val <- filter_val
                                                                filter_var <- filter_var

                                                              }

                                                      if (length(filter_val)==0 && length(filter_var)==0) {

                                                          dynamic_data <- KPI_perf_overall %>% ungroup() %>%
                                                                          select_(.dots = unlist(select_var)) %>%
                                                                          select_(.dots = values_var) %>% 
                                                                          summarise_all(funs(sum))

                                                      } else {

                                                              dynamic_data <- KPI_perf_overall %>% ungroup() %>%
                                                                              select_(.dots = unlist(select_var)) %>%
                                                                              filter_(.,.dots = paste0(unlist(filter_var), " %in% '", unlist(filter_val), "'"))%>%
                                                                              select_(.dots = values_var) %>% 
                                                                              summarise_all(funs(sum))

                                                      }



                                                    dynamic_data <- reshape2::melt(dynamic_data, measure.vars = unlist(values_var))

                                                    # if no rows returned from the filtered selection (on click) display an overall pie chart
                                                    ggplot(dynamic_data, aes(x="", y=value*2, fill=variable)) + 
                                                    geom_bar(stat="identity", width=1, col = 'black') + coord_polar("y", start=0,clip = "on") + 
                                                    geom_text(aes(label = paste0(round(value,0), "%")), position = position_stack(vjust = 0.5)) +
                                                    scale_fill_manual( labels = c("Connected", "Waiting", "Paused", "Deassign"),
                                                                       values = wes_palette(n=4, name="Darjeeling2"))  +       
                                                    theme_bw(base_size = 15)  +                                                    
                                                    labs(x = NULL, y = NULL, fill = NULL) + 
                                                    labs(col = "Legend")  +
                                                    theme(panel.background = element_rect(linetype = 1,
                                                                                          colour = 'black',
                                                                                          size = 2,
                                                                                          fill = '#e6e8ed')) +
                                                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
                                                    theme(axis.text=element_blank()) + 
                                                    theme(legend.position="bottom") 

                                                     }
                                else{

                                      # ggplot to generate the barplot of performcance selected employee over the week
                                      ggplot(data = data_left_pgOne %>% filter(AGENT_NAME == (input$datatable_one_cell_clicked)[3]),
                                           aes(x = CONTACT_DATE, y = WORK_HOURS)) +
                                      geom_bar(fill = '#328770', col = "black", stat = "identity",  width = 0.5) +
                                      geom_smooth(level = .65,se = F, colour = 'red', method = 'loess',formula  = 'y ~ x') +
                                      theme_bw(base_size = 15)  + 
                                      labs(x = '', y = 'Working Hours') +
                                      labs(col = "Legend") + 
                                      theme(panel.background = element_rect(linetype = 1,
                                                                            colour = 'black',
                                                                            size = 2,
                                                                            fill = '#e6e8ed')) +
                                      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                                      labs(x = '', y = 'WORK_HOURS') + labs(col = "Legend")  #+ 
                                     # scale_x_date(date_breaks = 'day', date_labels = '%b %d\n%a')

                                 }
                            })

   
}