
library(shiny)
library(ggplot2)

connHandle <- odbcConnect("ORA_XE", uid="SYSTEM", pwd="1234")
query <- "INSERT INTO RESPONSES (FIRST_NAME, LAST_NAME, IN_TIME, OUT_TIME, SYSTEM) VALUES (?, ?, ?, ?, ?)"

# which fields get saved
fieldsAll <-  c("First_Name", "Last_Name", "In_Time", "Out_Time", "System")

# which fields are mandatory
fieldsMandatory <-
  c("First_Name", "Last_Name", "In_Time", "Out_Time")

defaulter <- "Red"
Ok <- "blue"


# get a formatted string of the timestamp (exclude colons as they are invalid # characters in Windows filenames)
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
    
    ######################################## START OF TAB TWO #######################################################
    
    
    output$plot1 <- renderPlot({
      
      data  = sqlQuery(connHandle, " SELECT * FROM  (
                       SELECT R.FIRST_NAME,
                       R.LAST_NAME,
                       - ROUND(SUM(((TO_DATE(R.TIMESTAMP||' '||R.out_time,'MM/DD/YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'MM/DD/YYYY HH24:MI')) *1440/60)),2) AS HOUR_DIFF,
                       r.timestamp
                       FROM RESPONSES R 
                       WHERE r.system = 'NOBEL'
                       AND R.out_time IS NOT NULL
                       AND R.IN_time IS NOT NULL
                       AND  EXTRACT(YEAR FROM TO_DATE(TIMESTAMP,'MM/DD/YYYY')) = 2018
                       GROUP BY R.FIRST_NAME,R.LAST_NAME,R.TIMESTAMP) WHERE HOUR_DIFF > 0; "  )
      
      data$day <- weekdays(as.Date(data$TIMESTAMP)) 
      
      
      ggplot(data = data, aes(y=HOUR_DIFF,x=day)) +
        geom_point(shape=15,size = 5,aes(color = ifelse(data$HOUR_DIFF<= 8,Ok,defaulter)), show.legend = T ) + 
        scale_color_manual(labels = c("Default", "Success"), values = c("red", "GREEN")) +
        theme_minimal(base_size = 15)  + theme(panel.background = element_rect(linetype =1,colour = 'black', size=2))+
        labs(x= '', y= 'Work Hours') + labs(col="Legend") 
      
    })
    
    output$brush_info <- renderPrint({
      brushedPoints(data, input$plot1_brush, xvar= 'day', yvar='HOUR_DIFF')
    })
    
    output$plot2 <- renderPlot({
      
      individuals <- sqlQuery(connHandle, "SELECT R.FIRST_NAME||' '|| R.LAST_NAME as usernames,
                             - ROUND(((TO_DATE(R.TIMESTAMP||' '||R.out_time,'MM/DD/YYYY HH24:MI') -  TO_DATE(R.TIMESTAMP||' '||R.in_time,'MM/DD/YYYY HH24:MI')) *1440/60),2) AS HOUR_DIFF,
                              r.timestamp
                              FROM RESPONSES R 
                              WHERE r.system = 'NOBEL'
                              AND R.out_time IS NOT NULL
                              AND R.IN_time IS NOT NULL
                              AND  EXTRACT(YEAR FROM TO_DATE(TIMESTAMP,'MM/DD/YYYY')) = 2018")
      
      individuals$TIMESTAMP <- as.Date(individuals$TIMESTAMP, "%m/%d/%Y")
      
      print(input$text)
      
      ggplot(data = individuals %>% filter(individuals$USERNAMES == input$text), aes(x = TIMESTAMP, y = HOUR_DIFF))+
        geom_bar(fill ='royalblue', col = "black",stat = "identity") + 
        theme_classic() +
        theme_minimal(base_size = 15) +
        labs(x= '', y= 'Working Hours') + 
        labs(col="Legend") +
        geom_smooth(level = .65, se= F, colour = 'red') + 
        theme(panel.background = element_rect(linetype =1,colour = 'black', size=2))+
        labs(x= '', y= 'Work Hours') + 
        labs(col="Legend") 
      
      
      
    })

    
  }