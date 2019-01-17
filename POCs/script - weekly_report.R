library(RODBC)
library (ggplot2)
library (dplyr)


connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")

data  = sqlQuery(connHandle, " SELECT * FROM  (
                               SELECT R.FIRST_NAME,
                                             R.LAST_NAME,
                                             - ROUND(SUM(((TO_DATE(R.TIMESTAMP||' '||R.out_time,'MM/DD/YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'MM/DD/YYYY HH24:MI')) *1440/60)),2) AS HOUR_DIFF,
                                             r.timestamp
                                             FROM RESPONSES R 
                                             WHERE r.system = 'NOBEL'
                                             AND R.out_time IS NOT NULL
                                             AND R.IN_time IS NOT NULL
                                             AND  EXTRACT(YEAR FROM TO_DATE(TIMESTAMP,'MM/DD/YYYY')) = 2018
                                             GROUP BY R.FIRST_NAME,R.LAST_NAME,R.TIMESTAMP) WHERE HOUR_DIFF > 0; "  )


data$day <- weekdays(as.Date(data$TIMESTAMP)) 

head(data)

defaulter <- "Red"
Ok <- "blue"

ggplot(data = data, aes(y=data$HOUR_DIFF,x=data$day)) +
      geom_point(shape=15,size = 5,aes(color = ifelse(data$HOUR_DIFF<= 8,Ok,defaulter)), show.legend = T ) + 
      scale_color_manual(labels = c("Default", "Success"), values = c("red", "GREEN")) +
      theme_minimal(base_size = 22)  +
      labs(title = "Weekly Employee Performance", x= '', y= 'Work Hours') + labs(col="Legend")

z <- list(sqlQuery(connHandle, "SELECT distinct(FIRST_NAME||' '||LAST_NAME) as users FROM responses"))
head(z)
class(z)

u1 <- sqlQuery(connHandle, "SELECT R.FIRST_NAME||' '|| R.LAST_NAME as usernames,
                             - ROUND(((TO_DATE(R.TIMESTAMP||' '||R.out_time,'MM/DD/YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'MM/DD/YYYY HH24:MI')) *1440/60),2) AS HOUR_DIFF,
               r.timestamp
               FROM RESPONSES R 
               WHERE r.system = 'NOBEL'
               AND R.out_time IS NOT NULL
               AND R.IN_time IS NOT NULL
               AND  EXTRACT(YEAR FROM TO_DATE(TIMESTAMP,'MM/DD/YYYY')) = 2018")

u1$TIMESTAMP <- as.Date(u1$TIMESTAMP, "%m/%d/%Y")


ggplot(data = u1 %>% filter(u1$USERNAMES == 'u'), aes(x = u1$TIMESTAMP, y = u1$HOUR_DIFF))+
  geom_bar(fill ='royalblue', col = "black",stat = "identity") + theme_classic() +
  theme_minimal(base_size = 10)  +
  labs(x= '', y= 'Working Hours') + labs(col="Legend")

