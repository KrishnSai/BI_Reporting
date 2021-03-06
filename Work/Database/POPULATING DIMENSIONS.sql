MERGE INTO DIM_AGENT DA
    USING (SELECT DISTINCT AGENT_NAME AS AGENTS FROM DAILY_NOBLE_DATA DND WHERE DND.CONTACT_DATE = TO_DATE(SYSDATE - 6, 'DD-MON-YY'))
    ON (AGENTS = DA.AGENT_NAME)
  WHEN NOT MATCHED THEN
    INSERT (AGENT_ID, AGENT_NAME)
    VALUES (AGENT_SEQ.NEXTVAL, AGENTS);
    
    