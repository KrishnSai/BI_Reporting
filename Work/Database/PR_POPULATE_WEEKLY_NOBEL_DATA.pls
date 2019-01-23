create or replace PROCEDURE PR_POPULATE_WEEKLY_NOBEL_DATA IS
BEGIN

INSERT INTO DAILY_NOBLE_DATA_TEMP (
                                ID,
                                AGENT_NAME,
                                CODE,
                                CONTACT_DATE,
                                LOGON_TIME,
                                LOGOFF_TIME,
                                CONNECTED,
                                WAITING,
                                PAUSED,
                                DEASSIGN,
                                ACW,
                                TOTAL,
                                FLAG
                                )
                        SELECT
                            ID,
                            AGENT_NAME,
                            CODE,
                            CONTACT_DATE,
                            LOGON_TIME,
                            LOGOFF_TIME,
                            CONNECTED,
                            WAITING,
                            PAUSED,
                            DEASSIGN,
                            ACW,
                            TOTAL,
                            FLAG
                        FROM
                            DAILY_NOBLE_DATA;
                            
     
                            
UPDATE DAILY_NOBLE_DATA_TEMP 
SET     SHIFT = CASE 
                     WHEN TO_NUMBER(REPLACE(LOGON_TIME, ':', '')) BETWEEN 50000 AND 120000  THEN 'Morning'
                     WHEN TO_NUMBER(REPLACE(LOGON_TIME, ':', '')) BETWEEN 120000 AND 190000  THEN 'Afternoon'
                     WHEN TO_NUMBER(REPLACE(LOGON_TIME, ':', '')) BETWEEN 190000 AND 220000 THEN 'Evening'
                     ELSE 'Night'
                END,
                
         FLAG =  CASE  
                    WHEN FLAG = 'CONTINUED' THEN 'TO_ADJUST' 
                    WHEN FLAG = 'TO_ADJUST' THEN 'TO_ADD' 
                    ELSE 'TO_COMPUTE'
                END,
        WEEK_DAY = RTRIM(TO_CHAR(TO_DATE(CONTACT_DATE,'mm/dd/yyyy'), 'Day'))
        WHERE (FLAG <> 'COMPLETE' or FLAG IS NULL);


UPDATE DAILY_NOBLE_DATA_TEMP 
SET     CONTACT_DATE = CONTACT_DATE + 1 WHERE FLAG = 'TO_ADJUST';
        
     
COMMIT;

END;