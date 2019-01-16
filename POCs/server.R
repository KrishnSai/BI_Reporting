#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")
query <- "INSERT INTO RESPONSES (FIRST_NAME, LAST_NAME, IN_TIME, OUT_TIME, SYSTEM) VALUES (?, ?, ?, ?, ?)"

# which fields get saved
fieldsAll <-  c("First_Name", "Last_Name", "In_Time", "Out_Time", "System")

# which fields are mandatory
fieldsMandatory <-
  c("First_Name", "Last_Name", "In_Time", "Out_Time")



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