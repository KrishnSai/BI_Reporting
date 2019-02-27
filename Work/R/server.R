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
  
  
  ORA_DB <- reactive({
    
                      if(is.null(input$range[1]) || is.null(input$range[2])) {
                        dayStrt <- '2019-01-16'
                        dayend <- '2019-02-16'
                      } else {
                        dayStrt <- input$range[1]
                        dayend <- input$range[2]
                      }
                      
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
                       AND TO_DATE(DF.CONTACT_DATE , 'MM/DD/YYYY') BETWEEN TO_DATE('",dayStrt,"', 'YYYY-MM-DD') 
                       AND TO_DATE('",dayend,"', 'YYYY-MM-DD') ;")
    
    # fetch all data from  daily_noble_data_fact
    data_temp  <- sqlQuery(connHandle, metaQuery,stringsAsFactors = F)
    
    print(nrow(data_temp))
    print(input$range)
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
    
    return(list(as.data.frame(data_left_pgOne),as.data.frame(KPI_perf_overall)))  
    
  })
  
  observeEvent(input$range,{
    print(input$range)
  })
  
  observeEvent(input$navtab, {
    shinyjs::toggleClass(selector = "body", class = "one",
                         condition = (input$navtab == "Home"))
    
    shinyjs::toggleClass(selector = "body", class = "two",
                         condition = (input$navtab == "Weekly Performance Report"))
  })
  
  
  # element 1
  output$plot1 <- renderPlot({
    
    # ggplot to point plot the weekly performance of all the employees
    ggplot(data = as.data.frame(ORA_DB()[1]), aes(x = CONTACT_DATE,y = WORK_HOURS)) + 
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
    DT::datatable(brushedPoints (as.data.frame(ORA_DB()[1]),
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
    
    data_react = as.data.frame(ORA_DB()[1]) %>% filter(AGENT_NAME == (input$datatable_one_cell_clicked)[3])
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
        
        dynamic_data <- as.data.frame(ORA_DB()[2]) %>% ungroup() %>%
          select_(.dots = unlist(select_var)) %>%
          select_(.dots = values_var) %>% 
          summarise_all(funs(sum))
        
      } else {
        
        # if any of the filter selection is not ALL, i.e they have a singular value
        dynamic_data <- as.data.frame(ORA_DB()[2]) %>% ungroup() %>%
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
      ggplot(data = as.data.frame(ORA_DB()[1]) %>% filter(AGENT_NAME == (input$datatable_one_cell_clicked)[3]),
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
    individual_data <- as.data.frame(ORA_DB()[2]) %>% ungroup() %>%
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