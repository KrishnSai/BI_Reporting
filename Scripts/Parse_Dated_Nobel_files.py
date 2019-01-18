#!/usr/bin/env python
# coding: utf-8

# In[252]:


#importing the libraries
import pandas as pd
import os
import cx_Oracle  as ora
import re

path = "C:/Users/GTX STATION/Documents/Internship - GTX/BI_Reporting/data/" 
files = os.listdir(path)

for f in files:
    
    print("SQL connection opened")    
    #oracle connection details
    conn = ora.connect('system/1234@localhost:1521/xe')
    cursor = conn.cursor()
    
    # query1
    querystring1 = "alter session set nls_date_format = 'mm/dd/yyyy'"
    cursor.execute(querystring1)
    print("Query 1 Executed")
    
    # query2
    querystring2 = "select * from STAGING_FILES_READ"
    cursor.execute(querystring2)
    print("Query 2 Executed")
    files_arx = re.sub('[^0-9a-zA-Z._,]+', '', str(cursor.fetchall()))[:-1].split(',,')
    
    if f.replace(' ','') not in files_arx:
        # read the csv files
        nobel_data = pd.read_csv(path+f)

        # get the index where Agent Name is present
        header_dupes = nobel_data[nobel_data['Agent Hours'] == 'Agent Name'].index.tolist()

        # data cleansing
        nobel_data = nobel_data.iloc[header_dupes[0]:].dropna()
        nobel_data = nobel_data.reset_index(drop=True)
        nobel_data.columns = list(nobel_data.iloc[0])
        nobel_data = nobel_data.drop(index=nobel_data[nobel_data['Agent Name'] == 'Agent Name'].index.tolist())
        nobel_data = nobel_data.reset_index(drop=True)

        # query3
        querystring3 = "insert into STAGING_NOBLE (AGENT_NAME,                                                   CODE,                                                   CONTACT_DATE,                                                   LOGON_TIME,                                                   LOGOFF_TIME,                                                   CONNECTED,                                                   WAITING,                                                   PAUSED,                                                   DEASSIGN,                                                   ACW,                                                   TOTAL,                                                   FILE_NAME)                                     VALUES ('%s',%d,'%s','%s','%s','%s','%s','%s','%s','%s','%s','%s')"

        # loop thourhg dataframe 
        for i in range(len(nobel_data)):

            # define the bind variables for data entry
            cursor.execute(querystring3 % (nobel_data['Agent Name'][i],
                                           int(nobel_data['Code'][i]),
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
            
        print("Query 3 Executed")    
        
        # query4
        querystring4 = "insert into STAGING_FILES_READ values ('%s')"
        cursor.execute(querystring4 % (f))
        print("Query 4 Executed")
        
        #commit after all the data is entered
        conn.commit()
        cursor.close()
        conn.close()

        print("Total",len(nobel_data),'rows were inserted to the table STAGING_NOBLE')
        
    else:
        
        print('File:',f,'previously uploaded')
        
    print("SQL connection closed")


# In[ ]:




