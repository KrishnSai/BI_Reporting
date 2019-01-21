"""
Description : Reads the NOBEL data from the excel and updates in the database tables 
STAGING_NOBLE, STAGING_FILES_READ, JOBS_SCHEDULE. The log of the script is found in 
./logs and the data is loaded in ./data.

Author : Krishnendu Das

Date : 21-01-2019

Version : 1.0

Html : https://github.com/KDDS/BI_Reporting/blob/master/wORK/Parse_Nobel_Files.py

"""

#importing the libraries
import pandas as pd
import os
import re
import datetime
import cx_Oracle  as ora

status_flag = 'Started'
desc = "PARSE_NOBLE_DATA_JOB"

#oracle connection details
conn = ora.connect('system/1234@localhost:1521/xe')
cursor = conn.cursor()

# insert into the job table
queryjob = 'insert into JOBS_SCHEDULE (id, DESCRIPTIONS,START_DATE,STATUS) values (JOB_SEQ.NEXTVAL,:bdesc,sysdate,:bstatus_flag)'
cursor.execute(queryjob, bdesc=desc,bstatus_flag =status_flag)
cursor.execute('select JOB_SEQ.currval i from dual')
run_id = int(re.sub('[^0-9]+', '', str(cursor.fetchall())))
conn.commit()
               
try:

    #logging script
    log_path = "C:/Users/GTX STATION/Documents/Internship - GTX/BI_Reporting/logs/" 
    now = datetime.datetime.now()
    date = now.strftime("%d_%m_%Y")
    logfile = 'log_'+date+'.txt' 
    logs = os.listdir(log_path)
    
    # create the log file
    if logfile not in logs:
        logging = open(log_path+logfile, 'w')
    else:
        logging = open(log_path+logfile, 'a')

    print("  ",file=logging)
    print("############################################",file=logging)
    print("################### START ##################",file=logging)
    print("############################################",file=logging)
    print("  ",file=logging)

    path = "C:/Users/GTX STATION/Documents/Internship - GTX/BI_Reporting/data/" 
    files = os.listdir(path)

    for f in files:
        print(f,file=logging)

        # query1
        querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
        cursor.execute(querystring1)
        print("Query 1 Executed",file=logging)

        # query2
        querystring2 = "select * from STAGING_FILES_READ"
        cursor.execute(querystring2)
        print("Query 2 Executed",file=logging)
        files_arx = re.sub('[^0-9a-zA-Z._,]+', '', str(cursor.fetchall()))[:-1].split(',,')

        if f.replace(' ','') not in files_arx:
            # read the csv files
            nobel_data = pd.read_csv(path+f, names =("Agent Hours",
                                                     "col1","col2",
                                                     "col3", "col4",
                                                     "col5","col6",
                                                     "col7","col8",
                                                     "col9","col10"))

            # get the index where Agent Name is present
            header_dupes = nobel_data[nobel_data['Agent Hours'] == 'Agent Name'].index.tolist()

            # data cleansing
            nobel_data = nobel_data.iloc[header_dupes[0]:].dropna()
            nobel_data = nobel_data.reset_index(drop=True)
            nobel_data.columns = list(nobel_data.iloc[0])
            nobel_data = nobel_data.drop(index=nobel_data[nobel_data['Agent Name'] == 'Agent Name'].index.tolist())
            nobel_data = nobel_data.reset_index(drop=True)

            # query3
            querystring3 = "insert into STAGING_NOBLE (AGENT_NAME, \
                                                      CODE, \
                                                      CONTACT_DATE, \
                                                      LOGON_TIME, \
                                                      LOGOFF_TIME, \
                                                      CONNECTED, \
                                                      WAITING, \
                                                      PAUSED, \
                                                      DEASSIGN, \
                                                      ACW, \
                                                      TOTAL, \
                                                      FILE_NAME) \
                                        VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')"

            # loop thourhg dataframe 
            for i in range(len(nobel_data)):
                
                # update the job table
                status_flag = "Running"
                queryjob = "UPDATE JOBS_SCHEDULE SET status = :bstatus_flag WHERE id = :brun_id"
                cursor.execute(queryjob, bstatus_flag=status_flag, brun_id=run_id )
                conn.commit()

                # define the bind variables for data entry
                cursor.execute(querystring3 % (nobel_data['Agent Name'][i],
                                               nobel_data['Code'][i],
                                               nobel_data['Contact Date'][i],
                                               nobel_data['Logon Time'][i],
                                               nobel_data['Logoff Time'][i],
                                               nobel_data['Connect'][i],
                                               nobel_data['Waiting'][i],
                                               nobel_data['Paused'][i],
                                               nobel_data['Deassign'][i],
                                               nobel_data['ACW'][i],
                                               nobel_data['Total'][i],
                                               f)
                              )

            print("Query 3 Executed",file=logging) 

            # query4
            querystring4 = "insert into STAGING_FILES_READ values ('%s')"
            cursor.execute(querystring4 % (f))
            print("Query 4 Executed",file=logging)

            #commit after all the data is entered
            conn.commit()
            print("Total",len(nobel_data),'rows were inserted to the table STAGING_NOBLE',file=logging)
            print("             ",file=logging) 

        else:

            print('File:',f,'previously uploaded',file=logging)

    print("############################################",file=logging)
    print("################### END ##################",file=logging)
    print("############################################",file=logging)

    success_flag = True 
    print("SQL connection closed",file=logging)
    
    status_flag = 'Finished'
    
except Exception as e: 
    
    status_flag = 'Error'
    print("\nERROR : ++++++++++++++++++++++++++++++++++++",file=logging)
    print (str(e),file=logging)
    print("++++++++++++++++++++++++++++++++++++++++++\n",file=logging)

#update the job table
queryjob = "UPDATE JOBS_SCHEDULE SET status = :bstatus_flag, end_date = sysdate WHERE id = :brun_id "
cursor.execute(queryjob, bstatus_flag=status_flag, brun_id = run_id )
conn.commit()
    
cursor.close()
conn.close()