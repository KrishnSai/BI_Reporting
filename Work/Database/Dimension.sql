-----------------------------------
 -- CREATE THE DIMENSION TABLES --
-----------------------------------

CREATE TABLE DIM_AGENT (
    AGENT_ID VARCHAR2(100) NOT NULL UNIQUE,
    AGENT_NAME VARCHAR2(100) NOT NULL
);

CREATE TABLE DIM_WORK_KPI (
    KPI_ID NUMBER UNIQUE,
    TYPE_DESC VARCHAR2(100) NOT NULL
);

INSERT INTO DIM_WORK_KPI VALUES(1,'LOGON_TIME');
INSERT INTO DIM_WORK_KPI VALUES(2,'LOGOFF_TIME');
INSERT INTO DIM_WORK_KPI VALUES(3,'CONNECTS');
INSERT INTO DIM_WORK_KPI VALUES(4,'WAITING');
INSERT INTO DIM_WORK_KPI VALUES(5,'PAUSED');
INSERT INTO DIM_WORK_KPI VALUES(6,'DEASSIGN');
INSERT INTO DIM_WORK_KPI VALUES(7,'ACW');
INSERT INTO DIM_WORK_KPI VALUES(8,'TOTAL');
COMMIT;

CREATE TABLE DIM_CITY (
    CITY_ID NUMBER UNIQUE,
    CITY_NAME VARCHAR2(100) NOT NULL
);

INSERT INTO DIM_CITY VALUES(1,'Melbourne');
INSERT INTO DIM_CITY VALUES(2,'Kolkata');
INSERT INTO DIM_CITY VALUES(3,'Delhi');
INSERT INTO DIM_CITY VALUES(4,'Mumbai');
COMMIT;


CREATE TABLE DIM_SHIFTS (
    SHIFT_ID NUMBER UNIQUE,
    SHIFT_TYPE VARCHAR2(100) NOT NULL,
    SHIFT_START NUMBER
);

INSERT INTO DIM_SHIFTS VALUES(1,'Morning', 500);
INSERT INTO DIM_SHIFTS VALUES(2,'Afternoon',1200);
INSERT INTO DIM_SHIFTS VALUES(3,'Evening',1900 );
INSERT INTO DIM_SHIFTS VALUES(4,'Night',2200);
COMMIT;

CREATE TABLE DIM_WEEKS (
    WEEK_ID NUMBER UNIQUE,
    WEEK_DAY VARCHAR2(100) NOT NULL
);

INSERT INTO DIM_WEEKS VALUES(1,'Monday');
INSERT INTO DIM_WEEKS VALUES(2,'Tuesday');
INSERT INTO DIM_WEEKS VALUES(3,'Wednesday');
INSERT INTO DIM_WEEKS VALUES(4,'Thursday');
INSERT INTO DIM_WEEKS VALUES(5,'Friday');
INSERT INTO DIM_WEEKS VALUES(6,'Saturday');
INSERT INTO DIM_WEEKS VALUES(7,'Sunday');
commit;

-----------------------------------
 -- CREATE THE STAGING TABLES --
-----------------------------------

CREATE TABLE STAGING_NOBLE (
    AGENT_NAME     VARCHAR2(30),
    CODE           VARCHAR2(30),
    CONTACT_DATE   DATE,
    LOGON_TIME     VARCHAR2(30),
    LOGOFF_TIME    VARCHAR2(30),
    CONNECTED      VARCHAR2(30),
    WAITING        VARCHAR2(30),
    PAUSED         VARCHAR2(30),
    DEASSIGN       VARCHAR2(30),
    ACW            VARCHAR2(30),
    TOTAL          VARCHAR2(30),
    FILE_NAME      VARCHAR2(100),
    LOAD_DATE      TIMESTAMP DEFAULT SYSDATE    
);

CREATE TABLE STAGING_FILES_READ (

    FILE_NAME VARCHAR2(50)

);

-----------------------------------
 -- CREATE THE JOBS TABLES --
-----------------------------------

CREATE TABLE JOBS_SCHEDULE (
ID NUMBER,
DESCRIPTIONS    VARCHAR2(3000),
START_DATE      TIMESTAMP,   
END_DATE        TIMESTAMP,
STATUS CHAR(10)
);



CREATE TABLE DAILY_NOBLE_DATA (
    ID             NUMBER,
    AGENT_NAME     VARCHAR2(30),
    CODE           VARCHAR2(30),
    CONTACT_DATE   DATE,
    LOGON_TIME     VARCHAR2(30),
    LOGOFF_TIME    VARCHAR2(30),
    CONNECTED      VARCHAR2(30),
    WAITING        VARCHAR2(30),
    PAUSED         VARCHAR2(30),
    DEASSIGN       VARCHAR2(30),
    ACW            VARCHAR2(30),
    TOTAL          VARCHAR2(30),
    FLAG            VARCHAR2(30)
    );
    
CREATE TABLE DAILY_NOBLE_DATA_TEMP (
    ID             NUMBER,
    AGENT_NAME     VARCHAR2(30),
    CODE           VARCHAR2(30),
    CONTACT_DATE   DATE,
    LOGON_TIME     VARCHAR2(30),
    LOGOFF_TIME    VARCHAR2(30),
    CONNECTED      VARCHAR2(30),
    WAITING        VARCHAR2(30),
    PAUSED         VARCHAR2(30),
    DEASSIGN       VARCHAR2(30),
    ACW            VARCHAR2(30),
    TOTAL          VARCHAR2(30),
    SHIFT          VARCHAR2(30),
    WEEK_DAY       VARCHAR2(30), 
    FLAG           VARCHAR2(30)
    );



CREATE TABLE DAILY_NOBLE_DATA_FACT (
    AGENT_ID                  VARCHAR2(30),
    CONTACT_DATE              VARCHAR2(30),
    SHIFT_ID                  NUMBER,
    WEEK_ID                   NUMBER,
    TOTAL_CONNECTED_IN_HRS    NUMBER,
    TOTAL_WAITING_IN_HRS    NUMBER,
    TOTAL_PAUSED_IN_HRS       NUMBER,
    TOTAL_DEASSIGN_IN_HRS     NUMBER,
    TOTAL_SUMMARY_IN_HRS      NUMBER,
    TOTAL_CONNECTED_IN_MINS   NUMBER,
    TOTAL_WAITING_IN_MINS     NUMBER,
    TOTAL_PAUSED_IN_MINS      NUMBER,
    TOTAL_DEASSIGN_IN_MINS    NUMBER,
    TOTAL_SUMMARY_IN_MINS     NUMBER
    );


CREATE SEQUENCE  AGENT_SEQ  MINVALUE 1 INCREMENT BY 1 START WITH 1 CACHE 20 NOORDER  NOCYCLE ;
CREATE SEQUENCE  DAILY_SEQ  MINVALUE 1 MAXVALUE 9999999999999999999999999999 INCREMENT BY 1 START WITH 1 CACHE 20 NOORDER  NOCYCLE ;
CREATE SEQUENCE  JOB_SEQ  MINVALUE 1 INCREMENT BY 1 START WITH 1 CACHE 20 NOORDER  NOCYCLE ;

