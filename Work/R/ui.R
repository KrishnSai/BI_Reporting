# import the libraries 
library(shiny)
library(RODBCext)
library(shinythemes)
library(RODBC)
library(DT)

# create the oracle connection 
connHandle_1 <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

# fetch the unique employees from the database
employee <-  sqlQuery(connHandle_1, "SELECT AGENT_NAME AS USERS FROM DIM_AGENT order by 1 asc")


ui = 
  
  # list the navigation bar with a shiny theme
  navbarPage(    
    theme = shinytheme("spacelab"),

    # first tab displaying the Title and the Date
    tabPanel("Navbar 0", "Performance Dashboard",
             paste0('- ', format(Sys.Date(), "%Y/%m/%d"))
             ),

    # second tab displaying the weekly performance
    tabPanel(
      "Weekly Performance Report",
      br(),

      # row 1 : (5/1-5/1)
      fluidRow(
                
                # point plot to dislay the overall working hours of all the employees for each day
                column(
                        5,
                        h4("All Employees Weekly Performance"),
                        br(),
                        # element 1
                        plotOutput("plot1", brush = brushOpts(id = "plot1_brush")),
                        style = 'margin-bottom:30px;border:3px double; padding: 10px;',
                        offset = 1
                        ),

                # bar plot to display the employee wirking hours based on the selecion from the drop down
                column(
                        4,    
                        h4("Individual Performance"),
                        br(),
                        # element 2
                        plotOutput("plot2"),
                        style = 'margin-bottom:30px;border:3px double; padding: 10px;',
                        offset = 1
                      )
                

              ),
      
      # row 2 : (5/1-5/1)
      fluidRow(

                # display the brush info from the point plots in a structured data table
                column(
                        width = 5,
                        # element 3
                        DT::dataTableOutput("datatable_one"),
                        offset = 1
                      ),
                
                # Search box to input employee name for bar plot generation
                column(
                        width = 5,
                        selectInput( "daychoice",
                                     "Choose Day",
                                      choices =  c( "Sunday",
                                                    "Monday",
                                                    "Tuesday",
                                                    "Wednesday",
                                                    "Thursday",
                                                    "Friday",
                                                    "Saturday",
                                                    "All"), 
                                      selected = 'All', 
                                      multiple = FALSE),
                        
                        selectInput( "shiftchoice",
                                     "Choose Shift",
                                      choices =  c( "Morning",
                                                    "Afternoon",
                                                    "Evening",
                                                    "Night",
                                                    "All"), 
                                      selected = 'All', 
                                      multiple = FALSE),
                        
                        offset = 1

                      )
                )

              ),
    
    tabPanel("Search Employee", "TBD")

  )
