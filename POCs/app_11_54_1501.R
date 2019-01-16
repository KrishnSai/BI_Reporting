library(shiny)
library(RODBCext) #https://stackoverflow.com/questions/44502558/insert-multiple-rows-from-r-dataframe-into-oracle-database
library(shinythemes)
library(RODBC)

connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")
query <- "INSERT INTO RESPONSES (FIRST_NAME, LAST_NAME, IN_TIME, OUT_TIME, SYSTEM) VALUES (?, ?, ?, ?, ?)"

# which fields get saved
fieldsAll <-  c("First_Name", "Last_Name", "In_Time", "Out_Time", "System")

# which fields are mandatory
fieldsMandatory <-
  c("First_Name", "Last_Name", "In_Time", "Out_Time")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(label,
          span("*", class = "mandatory_star"))
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  
  sqlExecute(connHandle, query, data)
  
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
.shiny-input-container { margin-top: 25px; }
#submit_msg { margin-left: 15px; }
#error { color: red; }
body { background: #fcfcfc; }
#header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
"


shinyApp(
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
  )),
  
  
  server = function(input, output, session) {
    # Enable the Submit button when all mandatory fields are filled out
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x)
        input[[x]])
      #data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error",
                      anim = TRUE,
                      animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
    # render the admin panel
    output$adminPanelContainer <- renderUI({
      if (!isAdmin())
        return()
      
      div(
        id = "adminPanel",
        br(),
        br(),
        DT::dataTableOutput("responsesTable"),
        br(),
        downloadButton("downloadBtn", "Download Attendance")
      )
    })
    
    # determine if current user is admin
    isAdmin <- reactive({
      is.null(session$user) || session$user %in% adminUsers
    })
    
    # if (input$submit) {
    #   output$responsesTable <- DT::renderDataTable(
    #     DT::datatable(sqlQuery(channel, "SELECT * FROM responses"), options = list(searching = F)))
    # }
    
    # Show the responses in the admin table
    output$responsesTable <- DT::renderDataTable(
      DT::datatable(sqlQuery(connHandle, "SELECT * FROM responses"), options = list(searching = F)))
    
    # Allow user to download responses
    output$downloadBtn <- downloadHandler(
      filename = function() {
        sprintf("responses.csv", humanTime())
      },
      content = function(file) {
        write.csv(loadData(), file, row.names = FALSE)
      }
    )
  }
)