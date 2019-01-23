library(shiny)
library(ggplot2)
library(dplyr)


connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")
query <- "INSERT INTO RESPONSES (FIRST_NAME, LAST_NAME, IN_TIME, OUT_TIME, SYSTEM) VALUES (?, ?, ?, ?, ?)"

data_temp  = sqlQuery(connHandle, "
                       SELECT DA.AGENT_NAME,
                       SUM(r.total_summary_in_hrs) AS HOUR_DIFF,
                       r.contact_date AS TIMESTAMP
                       FROM DAILY_NOBLE_DATA_FACT R,
                       DIM_AGENT DA
                       WHERE da.agent_id =r.agent_id
                       AND CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'DD-MON-YY')
                       GROUP BY DA.AGENT_NAME, r.contact_date"
)



data_temp$TIMESTAMP <- format(strptime(data_temp$TIMESTAMP, format = "%d/%b/%y"), "%d-%b-%y")

data_temp$DAY <- weekdays(strptime(data_temp$TIMESTAMP, format =  "%d-%b-%y"))


defaulter <- "Red"
Ok <- "blue"


server = function(input, output, session) {
  
    
    ######################################## START OF TAB one #######################################################
    
    
    output$plot1 <- renderPlot({

      ggplot(data = data_temp, aes(y=HOUR_DIFF,x=DAY)) +
        geom_point(shape=15,size = 5,aes(color = ifelse(data_temp$HOUR_DIFF<= 8,Ok,defaulter)), show.legend = T ) + 
        scale_color_manual(labels = c("Default", "Success"), values = c("#D55E00", "#0072B2")) +
        theme_bw(base_size = 15)  + theme(panel.background = element_rect(linetype =1,colour = 'black', size=2,  fill = '#e6e8ed'))+
        labs(x= '', y= 'Work Hours') + labs(col="") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
    })
    
    print( head(data_temp) )
    
    output$brush_info <- renderTable({
      brushedPoints(data_temp, input$plot1_brush, xvar= 'DAY', yvar='HOUR_DIFF')
    })
    
    output$plot2 <- renderPlot({
    
      ggplot(data = data_temp %>% filter(data_temp$AGENT_NAME == input$text), aes(x = TIMESTAMP, y = HOUR_DIFF))+
        geom_bar(fill ='#328770', col = "black",stat = "identity") + 
        theme_bw() +
        theme_minimal(base_size = 15) +
        labs(x= '', y= 'Working Hours') + 
        labs(col="Legend") +
        geom_smooth(level = .65, se= F, colour = 'red') + 
        theme(panel.background = element_rect(linetype =1,colour = 'black', size=2, fill = '#e6e8ed'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x= '', y= 'Work Hours') + 
        labs(col="Legend") 
      
      
      
    })

    
  }