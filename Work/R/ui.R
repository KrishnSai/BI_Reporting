library(shiny)
library(RODBCext) #https://stackoverflow.com/questions/44502558/insert-multiple-rows-from-r-dataframe-into-oracle-database
library(shinythemes)
library(RODBC)
library(DT)

print("strt")
connHandle_1 <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")
employee <-
  sqlQuery(connHandle_1,
           "SELECT AGENT_NAME AS USERS FROM DIM_AGENT order by 1 asc")

ui = 
  
  navbarPage(
    
    theme = shinytheme("spacelab"),
    tabPanel("Navbar 0", "Performance Dashboard",
             paste0('- ', format(
               Sys.Date(), "%Y/%m/%d"
             ))),
    tabPanel(
      "Reports",
      br(),
      fluidRow(
        column(
          5,
          h4("All Employees Weekly Performance"),
          br(),
          plotOutput("plot1",
                     brush = brushOpts(id = "plot1_brush")),
          style = 'margin-bottom:30px;border:3px double; padding: 10px;',
          offset = 1
        ),
        column(
          4,
          h4("Individual Performance"),
          br(),
          plotOutput("plot2"),
          style = 'margin-bottom:30px;border:3px double; padding: 10px;',
          offset = 1
        )
      ),
      fluidRow(
        column(
          width = 5,
          DT::dataTableOutput("brush_info"),
          offset = 1
        ),
        column(
          width = 5,
          selectInput("text", label = "Search employee", choices = as.list(employee$USERS)),
          offset = 1
        )
      )
    ),
    
    tabPanel("Visualisation", "Data Visualisation")
  )