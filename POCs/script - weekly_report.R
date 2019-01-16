library(RODBC)
library (ggplot2)


connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")

data  = sqlQuery(connHandle, "     SELECT R.FIRST_NAME,
                                          R.LAST_NAME,
                                          SUM(TRUNC((TO_DATE(R.TIMESTAMP||' '||R.out_time,'DD-MM-YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'DD-MM-YYYY HH24:MI')) *1440/60)) AS HOUR_DIFF,
                                          r.timestamp
                                     FROM RESPONSES R 
                                    WHERE r.system = 'NOBEL'
                                      AND R.out_time IS NOT NULL
                                      AND R.IN_time IS NOT NULL
                                      AND TO_DATE(TIMESTAMP,'DD-MM-YYYY') > SYSDATE - 1 
                                 GROUP BY R.FIRST_NAME,R.LAST_NAME,R.TIMESTAMP;"
                 )


data$day <- weekdays(as.Date(data$TIMESTAMP)) 

head(data)

defaulter <- "Red"
Ok <- "blue"

ggplot(data = data, aes(y=data$HOUR_DIFF,x=data$day)) +
      geom_point(shape=15,size = 5,aes(color = ifelse(data$HOUR_DIFF<= 8,Ok,defaulter)), show.legend = T ) + 
      scale_color_manual(labels = c("defaulter", "ok"), values = c("red", "blue")) +
      theme_minimal()  +
      labs(title = "Daily Performance", x= 'Days', y= 'Work Hours') + labs(col="Legend")
