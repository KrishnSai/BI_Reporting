# import the libraries 
library(shiny)

# variable for conditional coloring at ggplot
defaulter <- "Red"
Ok <- "blue"

all_days <- days_present

filter_var <- list() 
group_var <-  list('SHIFT_TYPE','WEEK_DAY')
filter_val <- list()
values_var <- list('TOTAL_CONNECTED_IN_HRS',
                     'TOTAL_WAITING_IN_HRS', 
                     'TOTAL_PAUSED_IN_HRS', 
                     'TOTAL_DEASSIGN_IN_HRS')
select_var <- c(group_var,values_var)
all_shifts <- c("Morning", "Afternoon", "Night") 


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
                                    labs(x = '', y = '') + labs(col = "") +
                                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  +
                                    scale_x_date(date_breaks = 'day', date_labels = '%b %d\n%a') +
                                    theme(legend.position="bottom") + 
                                    ggtitle("All Employees Weekly Performance")
    
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
                              total_rows <- reactiveValues()
                              total_rows = nrow(data_react)
                              
                              # observe if no rows in data table or no selection then show pie chart
                              observe(
                                 if (total_rows != 0) {

                                          shinyjs::hideElement(id= "daychoice")
                                          shinyjs::hideElement(id= "shiftchoice")
                                          #shinyjs::showElement(id= "plot3")
                              } else{
                                          # hide element if an user is selected
                                          shinyjs::showElement(id= "daychoice")
                                          shinyjs::showElement(id= "shiftchoice")
                                          #shinyjs::hideElement(id= "plot3")
                              })

                              if (total_rows == 0) {
                                
                                                    ind_flag <<- 4
                                                    
                                                    # encode the day value with actual values
                                                    if (input$daychoice=="Sun") {day_val <- c("Sunday")}
                                                    if (input$daychoice=="Mon") {day_val <- c("Monday")}
                                                    if (input$daychoice=="Tue") {day_val <- c("Tuesday")}
                                                    if (input$daychoice=="Wed"){day_val <- c("Wednesday")}
                                                    if (input$daychoice=="Thu") {day_val <- c("Thursday")}
                                                    if (input$daychoice=="Fri") {day_val <- c("Friday")}
                                                    if (input$daychoice=="Sat") {day_val <- c("Saturday")}
                                                    if (input$daychoice=="All") {day_val <- all_days}
                                                    
                                                    # if a day is selected the filter variable adds Week_day and the value to filter
                                                    if (input$daychoice != "All") {

                                                        filter_var <- list('WEEK_DAY')
                                                        filter_val <-  list(day_val)

                                                      } else {

                                                                # if all the days were selected, no filter clause is put
                                                                filter_val <-  list()
                                                                filter_var <- list()

                                                              }

                                                      # if a shift is selected the filter variable adds SHIFT_TYPE and the value to filter as well
                                                      if (input$shiftchoice != "All") {

                                                        filter_var <- c(filter_var,list('SHIFT_TYPE'))
                                                        filter_val <- c(filter_val,list(input$shiftchoice))

                                                      } else {
                                                                # if all the shifts were selected, no filter clause is put
                                                                filter_val <- filter_val
                                                                filter_var <- filter_var

                                                              }

                                                      # if ALL days and ALL shifts were selected  
                                                      if (length(filter_val)==0 && length(filter_var)==0) {

                                                          dynamic_data <- KPI_perf_overall %>% ungroup() %>%
                                                                          select_(.dots = unlist(select_var)) %>%
                                                                          select_(.dots = values_var) %>% 
                                                                          summarise_all(funs(sum))

                                                      } else {
                                                        
                                                              # if any of the filter selection is not ALL, i.e they have a singular value
                                                              dynamic_data <- KPI_perf_overall %>% ungroup() %>%
                                                                              select_(.dots = unlist(select_var)) %>%
                                                                              filter_(.,.dots = paste0(unlist(filter_var), " %in% '", unlist(filter_val), "'"))%>%
                                                                              select_(.dots = values_var) %>% 
                                                                              summarise_all(funs(sum))

                                                      }


                                                    # melt the data for plotting
                                                    dynamic_data <- reshape2::melt(dynamic_data, measure.vars = unlist(values_var))
                                                    dynamic_data <- dynamic_data %>%   mutate_at(vars(value), funs(./sum(value)*100)) 

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
                                                    theme(axis.text=element_blank()) + labs(x = '', y = '') +
                                                    theme(legend.position="bottom")  + ggtitle("Total Activity Shares")

                                                     }
                                else{
                                  
                                      ind_flag <<- 5
                                      user <- paste0("Hours Worked by ",(input$datatable_one_cell_clicked)[3],' :')

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
                                      labs(x = '', y = '') + labs(col = "Legend")  + 
                                      ggtitle(user)#+ 
                                     # scale_x_date(date_breaks = 'day', date_labels = '%b %d\n%a')

                                }
                              
                            })

# element 3
output$plot3 <- renderPlot({
  

 # prepare the datframe for user related statistics on call KPIs
 individual_data <- KPI_perf_overall %>% ungroup() %>%
                     select_(.dots = unlist(c('AGENT_NAME',select_var))) %>%
                     filter(.,AGENT_NAME == (input$datatable_one_cell_clicked)[3]) %>%
                     select_(.dots = values_var) %>% 
                     summarise_all(funs(sum))
 
 #melt and prepare the data
 individual_data <- reshape2::melt(individual_data, measure.vars = unlist(values_var))
 individual_data <- individual_data %>%   mutate_at(vars(value), funs(./sum(value)*100)) 
 
 if (ind_flag == 5) {
                                
                                user <- paste0("Performance of ",(input$datatable_one_cell_clicked)[3],' :', ind_flag)
                                
                                # if no rows returned from the filtered selection (on click) display an overall pie chart
                                ggplot(individual_data, aes(x="", y=value*2, fill=variable)) + 
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
                                theme(axis.text=element_blank()) + labs(x = '', y = '') +
                                theme(legend.position="bottom") + ggtitle(user)
   
                              }

})
  
}