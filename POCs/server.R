
library(shiny)
library(ggplot2)
library(dplyr)


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
      
      data  = sqlQuery(connHandle, "
                       SELECT DA.AGENT_NAME,
                       SUM(r.total_summary_in_hrs) AS HOUR_DIFF,
                       r.contact_date AS TIMESTAMP
                       FROM DAILY_NOBLE_DATA_FACT R,
                       DIM_AGENT DA
                       WHERE da.agent_id =r.agent_id
                       AND CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'DD-MON-YY')
                       GROUP BY DA.AGENT_NAME, r.contact_date"
      )
      
      data$TIMESTAMP <- format(strptime(data$TIMESTAMP, format = "%d/%b/%y"), "%d-%b-%y")
      
      data$DAY <- weekdays(strptime(data$TIMESTAMP, format =  "%d-%b-%y"))
      

      ggplot(data = data, aes(y=HOUR_DIFF,x=DAY)) +
        geom_point(shape=15,size = 5,aes(color = ifelse(data$HOUR_DIFF<= 8,Ok,defaulter)), show.legend = T ) + 
        scale_color_manual(labels = c("Default", "Success"), values = c("#D55E00", "#0072B2")) +
        theme_bw(base_size = 15)  + theme(panel.background = element_rect(linetype =1,colour = 'black', size=2,  fill = '#e6e8ed'))+
        labs(x= '', y= 'Work Hours') + labs(col="") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
    })
    
    output$brush_info <- renderTable({
      brushedPoints(data, input$plot1_brush, xvar= 'DAY', yvar='HOUR_DIFF')
    })
    
    output$plot2 <- renderPlot({
      
      individuals <-  sqlQuery(connHandle, "
                       SELECT DA.AGENT_NAME,
                               SUM(r.total_summary_in_hrs) AS HOUR_DIFF,
                               r.contact_date AS TIMESTAMP
                               FROM DAILY_NOBLE_DATA_FACT R,
                               DIM_AGENT DA
                               WHERE da.agent_id =r.agent_id
                               AND CONTACT_DATE >= TO_DATE(SYSDATE - 7, 'DD-MON-YY')
                               GROUP BY DA.AGENT_NAME, r.contact_date"
      )
      
      data$TIMESTAMP <- format(strptime(data$TIMESTAMP, format = "%d/%b/%y"), "%d-%b-%y")
      
      data$DAY <- weekdays(strptime(data$TIMESTAMP, format =  "%d-%b-%y"))
      
      print(input$text)
      
      ggplot(data = individuals %>% filter(individuals$AGENT_NAME == input$text), aes(x = TIMESTAMP, y = HOUR_DIFF))+
        geom_bar(fill ='#328770', col = "black",stat = "identity") + 
        theme_bw() +
        theme_minimal(base_size = 15) +
        labs(x= '', y= 'Working Hours') + 
        labs(col="Legend") +
        geom_smooth(level = .65, se= F, colour = 'red') + 
        theme(panel.background = element_rect(linetype =1,colour = 'black', size=2, fill = '#e6e8ed'))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x= '', y= 'Work Hours') + 
        labs(col="Legend") 
      
      
      
    })

    
  }