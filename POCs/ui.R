library(shiny)
library(RODBCext) #https://stackoverflow.com/questions/44502558/insert-multiple-rows-from-r-dataframe-into-oracle-database
library(shinythemes)
library(RODBC)

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

print("strt")
connHandle_1 <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")
employee <- sqlQuery(connHandle_1, "SELECT distinct(FIRST_NAME||' '||LAST_NAME) as users FROM responses")
head(employee$USERS)
print(employee$USERS)
print("closed")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"

  ui = #fluidPage(theme = shinytheme("sandstone"),
                 
                 navbarPage(theme = shinytheme("sandstone"),
                   tabPanel("Navbar 0", "Performance Dashboard"),
                   tabPanel(
                     "Data Entry",
                     shinyjs::useShinyjs(),
                     shinyjs::inlineCSS(appCSS),
                     fluidRow(column(1),
                              column(6,
                                     h2(
                                       'Employee Login Details'
                                     ))),
                     
                     fluidRow(
                       br(),
                       column(1),
                       column(
                         4,
                         br(),
                         div(
                           id = "form",
                           textInput("First_Name", labelMandatory("First Name"), ""),
                           textInput("Last_Name", labelMandatory("Last Name")),
                           textInput("In_Time", labelMandatory("IN - Time"), ""),
                           textInput("Out_Time", labelMandatory("Out - Time"), ""),
                           radioButtons(
                             "System",
                             "System2",
                             choices = c('COSEC', 'NOBEL'),
                             inline = T
                           ),
                           #checkboxInput("COSEC", "Door Data", F),
                           actionButton("submit", "Submit", class = "btn-primary"),
                           
                           shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                                           div(id = "error",
                                               div(
                                                 br(), tags$b("Error: "), span(id = "error_msg")
                                               )))
                         ),
                         
                         shinyjs::hidden(div(
                           id = "thankyou_msg",
                           h3("Thanks, your response was submitted successfully!"),
                           actionLink("submit_another", "Submit another response")
                         ))
                       ),
                       column(3,
                              uiOutput("adminPanelContainer"))
                     )
                   ),
                   tabPanel("Reports",
                            br(),
                            fluidRow(column(5,
                                            h4("All Employees Weekly Performance"),
                                            br(),
                                            plotOutput("plot1",   
                                                       brush = brushOpts(id = "plot1_brush")),
                                            style='margin-bottom:30px;border:3px double; padding: 10px;',
                                            offset = 1),
                                     column(4,
                                            h4("Individual Performance"),
                                            br(),
                                            plotOutput("plot2"),
                                            style='margin-bottom:30px;border:3px double; padding: 10px;',
                                            offset = 1
                                       )
                                     ),
                            hr(),
                            fluidRow(column(width = 5,
                                            br(),
                                            verbatimTextOutput("brush_info"),
                                            offset = 1),
                                     column(width = 5,
                                            
                                            selectInput("text", label = "Search employee", choices = as.list(employee$USERS)),
                                            offset = 1))
                            ),

                   tabPanel("Visualisation", "Data Visualisation")
                 )