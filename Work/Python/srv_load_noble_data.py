"""
Description : Reads the NOBEL data from the excel and updates in the database tables 
STAGING_NOBLE, STAGING_FILES_READ, JOBS_SCHEDULE. The log of the script is found in 
./logs and the data is loaded in ./data.

Author : Krishnendu Das

Date : 21-01-2019

Version : 1.0

Html : TBD

"""

#importing the libraries
import subprocess
import os 
import re
import datetime
import sys
from argparse import ArgumentParser
import pandas as pd
import cx_Oracle  as ora

parser = ArgumentParser(description='Utility script to load daily data into Oracle Database')
parser.add_argument("-l", "--log",
                    help="path to directory containing log files (default: '../../log')")
parser.add_argument("-d", "--data",
                    help="path to directory containing data files (default: '../../data')")
parser.add_argument("--database",
                    help="database address(default: 'system/1234@localhost:1521/xe')")
args = parser.parse_args()


status_flag = 'Started'
desc = "PARSE_NOBLE_DATA_JOB"


#oracle connection details
conn = ora.connect('system/1234@localhost:1521/xe')

db_address = 'system/1234@localhost:1521/xe' if not args.database else args.database
conn = ora.connect(db_address)
cursor = conn.cursor()

# insert into the job table
queryjob = 'insert into JOBS_SCHEDULE (id, DESCRIPTIONS,START_DATE,STATUS) values (JOB_SEQ.NEXTVAL,:bdesc,sysdate,:bstatus_flag)'
cursor.execute(queryjob, bdesc=desc,bstatus_flag =status_flag)
cursor.execute('select JOB_SEQ.currval i from dual')
run_id = int(re.sub('[^0-9]+', '', str(cursor.fetchall())))
conn.commit()
               
try:

    #logging script
    log_path = "../../logs/" if not args.log else args.log
    if not os.path.isdir(log_path):
        os.mkdir(log_path)
    now = datetime.datetime.now()
    date = now.strftime("%d_%m_%Y")
    logfile = 'log_'+date+'.txt' 
    logs = os.listdir(log_path)
    
    # create the log file
    if logfile not in logs:
        logging = open(log_path+logfile, 'w')
    else:
        logging = open(log_path+logfile, 'a')

    logging.write("  \n")
    logging.write("############################################\n")
    logging.write("################### START ##################\n")
    logging.write("############################################\n")

    path = "C:/Users/Jennifer/Documents/GitHub/BI_Reporting1/data/" 
    files = os.listdir(path)
 
    for f in files:
        print(f,file=logging)

        # query1
        querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
        cursor.execute(querystring1)
        logging.write("Query 1 Executed\n")

        # query2
        querystring2 = "select * from STAGING_FILES_READ"
        cursor.execute(querystring2)
        logging.write("Query 2 Executed\n")
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
                                        VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%squerystring3 = "insert into STAGING_NOBLE (AGENT_NAME,                                                       CODE,                                                       CONTACT_DATE,                                                       LOGON_TIME,                                                       LOGOFF_TIME,                                                       CONNECTED,                                                       WAITING,                                                       PAUSED,                                                       DEASSIGN,                                                       ACW,                                                       TOTAL,                                                       FILE_NAME)                                         VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%squerystring3 = "insert into STAGING_NOBLE (AGENT_NAME,                                                       CODE,                                                       CONTACT_DATE,                                                       LOGON_TIME,                                                       LOGOFF_TIME,                                                       CONNECTED,                                                       WAITING,                                                       PAUSED,                                                       DEASSIGN,                                                       ACW,                                                       TOTAL,                                                       FILE_NAME)                                         VALUES ('%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')"


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

            logging.write("Query 3 Executed\n") 

            # query4
            querystring4 = "insert into STAGING_FILES_READ values ('%s')"
            cursor.execute(querystring4 % (f))
            logging.write("Query 4 Executed\n")

            #commit after all the data is entered
            conn.commit()
            logging.write("Total"+str(len(nobel_data))+'rows were inserted to the table STAGING_NOBLE\n')
            logging.write("             \n") 

        else:

            logging.write('File:'+str(f)+'previously uploaded\n')

    logging.write("############################################\n")
    logging.write("################### END ##################\n")
    logging.write("############################################\n")

    success_flag = True 
    logging.write("SQL connection closed")
    
    status_flag = 'Finished'
    
except Exception as e: 
    
    status_flag = 'Error'
    logging.write("\nERROR : ++++++++++++++++++++++++++++++++++++\n")
    logging.write (str(e)+'\n')
    logging.write("++++++++++++++++++++++++++++++++++++++++++\n\n")

#update the job table
queryjob = "UPDATE JOBS_SCHEDULE SET status = :bstatus_flag, end_date = sysdate WHERE id = :brun_id "
cursor.execute(queryjob, bstatus_flag=status_flag, brun_id = run_id )
conn.commit()

querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
cursor.execute(querystring1)

for i in reversed(range(1,30)):
    cursor.callproc("PR_POPULATE_DAILY_NOBEL_DATA",[i])
    cursor.callproc("PR_POPULATE_WEEKLY_NOBEL_DATA")
    cursor.callproc("PR_POPULATE_DAILY_FACT")
    print("Processed sysdate - "+ str(i))
    
cursor.close()
conn.close()
logging.close()
