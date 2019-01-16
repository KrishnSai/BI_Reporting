library(RODBC)
connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")

data  = sqlQuery(connHandle, "  SELECT R.FIRST_NAME,
         R.LAST_NAME ,
         SUM(TRUNC((TO_DATE(R.TIMESTAMP||' '||R.out_time,'DD-MM-YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'DD-MM-YYYY HH24:MI')) *1440/60)) AS HOUR_DIFF,
         r.timestamp
    FROM RESPONSES R 
    WHERE r.system = 'NOBEL' GROUP BY R.FIRST_NAME,R.LAST_NAME,R.TIMESTAMP;")


library (ggplot2)
