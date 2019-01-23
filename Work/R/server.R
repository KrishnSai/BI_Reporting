library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


connHandle <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

data_temp  = sqlQuery(
  connHandle,
  "
  SELECT * FROM (
  SELECT DA.AGENT_NAME,
  SUM(r.total_summary_in_hrs) AS WORK_HOURS,
  r.contact_date AS TIMESTAMP
  FROM DAILY_NOBLE_DATA_FACT R,
  DIM_AGENT DA
  WHERE da.agent_id =r.agent_id
  AND CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'DD-MON-YY')
  GROUP BY DA.AGENT_NAME, r.contact_date) WHERE WORK_HOURS > 1"
)



data_temp$TIMESTAMP <-
  format(strptime(data_temp$TIMESTAMP, format = "%d/%b/%y"), "%d-%b-%y")

data_temp$DAY <-
  weekdays(strptime(data_temp$TIMESTAMP, format =  "%d-%b-%y"))


defaulter <- "Red"
Ok <- "blue"


server = function(input, output, session) {
  
  ######################################## START OF TAB one #######################################################
  
  
  output$plot1 <- renderPlot({
    
    ggplot(data = data_temp, aes(y = WORK_HOURS, x = DAY)) +
      geom_point(
        shape = 15,
        size = 5,
        aes(color = ifelse(
          data_temp$WORK_HOURS <= 8, Ok, defaulter
        )),
        show.legend = T
      ) +
      scale_color_manual(
        labels = c("Default", "Success"),
        values = c("#D55E00", "#0072B2")
      ) +
      theme_bw(base_size = 15)  + theme(panel.background = element_rect(
        linetype = 1,
        colour = 'black',
        size = 2,
        fill = '#e6e8ed'
      )) +
      labs(x = '', y = 'WORK_HOURS') + labs(col = "") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
  print(head(data_temp))
  
  #   output$brush_info <- renderTable({
  #   brushedPoints(data_temp,
  #                 input$plot1_brush,
  #                 xvar = 'DAY',
  #                 yvar = 'WORK_HOURS')
  # }, options = list(paging = T))
    
    
    output$brush_info <- DT::renderDataTable(
      DT::datatable(brushedPoints(data_temp,
                                  input$plot1_brush,
                                  xvar = 'DAY',
                                  yvar = 'WORK_HOURS'), options = list(paging = T,
                                                                      pageLength = 3,
                                                                      dom='tip',
                                                                      order = list(2,'asc')))
    )
  
  output$plot2 <- renderPlot({
    
    ggplot(
      data = data_temp %>% filter(data_temp$AGENT_NAME == input$text),
      aes(x = TIMESTAMP, y = WORK_HOURS)
    ) +
      geom_bar(fill = '#328770',
               col = "black",
               stat = "identity") +
      theme_bw() +
      theme_minimal(base_size = 15) +
      labs(x = '', y = 'Working Hours') +
      labs(col = "Legend") +
      geom_smooth(level = .65,
                  se = F,
                  colour = 'red') +
      theme(panel.background = element_rect(
        linetype = 1,
        colour = 'black',
        size = 2,
        fill = '#e6e8ed'
      )) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      labs(x = '', y = 'WORK_HOURS') +
      labs(col = "Legend")
    
  })
  
  
}