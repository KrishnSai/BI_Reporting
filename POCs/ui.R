library(shiny)
library(RODBCext) #https://stackoverflow.com/questions/44502558/insert-multiple-rows-from-r-dataframe-into-oracle-database
library(shinythemes)
library(RODBC)

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"


  ui = fluidPage(theme = shinytheme("sandstone"),
                 
                 navbarPage(
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
                   tabPanel("Reports", "Performance Reports"),
                   tabPanel("Visualisation", "Data Visualisation")
                 ))