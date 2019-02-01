#https://rstudio-pubs-static.s3.amazonaws.com/27777_55697c3a476640caa0ad2099fe914ae5.html#/6

# import the libraries 
library(shiny)

# load the global variables
source('common_references.R', local= F)

# create the oracle connection 
connHandle_1 <- odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

ui = 
  
  tags$style(type="text/css",
         ".shiny-output-error { visibility: hidden; }",
         ".shiny-output-error:before { visibility: hidden; }")
  
  # list the navigation bar with a shiny theme
  navbarPage(

    useShinyjs(),
    theme = shinytheme("spacelab"),

    # Tab displaying the weekly performance
    tabPanel(
      #HTML(paste("This text is ", tags$span(style="color:red", "red"), sep = "")),
      style = 'margin-bottom:30px;border:3px double; padding: 10px;',
      "Weekly Performance Report",
      dateRangeInput("daterange3", "Enter Date Range:",
                 start  = "2001-01-01",
                 end    = "2010-12-31",
                 min    = "2001-01-01",
                 max    = "2012-12-21",
                 format = "mm/dd/yy",
                 separator = icon(class="far fa-arrow-alt-circle-right", "fa-1x")),
      br(),


      # row 1 : (5/1-5/1)
      fluidRow(
                
                # point plot to dislay the overall working hours of all the employees for each day
                column(
                        5,
                        align="center",
                        # element 1
                        plotOutput("plot1", brush = brushOpts(id = "plot1_brush")),
                        offset = 1
                        ),

                # bar plot to display the employee wirking hours based on the selecion from the drop down
                column(
                        4,    
                        align=align2_flag,
                        # element 2
                        plotOutput("plot2"),
                        offset = 1
                      )
              ),
      
      # row 2 : (5/1-5/1)
      fluidRow(

                # display the brush info from the point plots in a structured data table
                column(
                        width = 5,
                        align="center",
                        # element 3
                        DT::dataTableOutput("datatable_one"),
                        offset = 1
                      ),
                
                # Search box to input employee name for bar plot generation
                column(
                        width = ind_flag,
                        align="center",
                        sliderTextInput(
                                        inputId = "daychoice",
                                          label = "",
                                        choices = substr(c(days_present,"All"),1,3),   
                                        selected = "All",
                                        grid = T,
                                        animationOptions(interval = 1000, 
                                                         loop = FALSE, 
                                                         playButton = icon("fas fa-angle-double-right", "fa-1x"), 
                                                         pauseButton = icon("fas fa-pause", "fa-1x"))
                                        ),
                        
                        selectInput(  inputId = "shiftchoice",
                                        label = "Choose Shifts",
                                      choices =  c( "Morning",
                                                    "Afternoon",
                                                    "Night",
                                                    "All"), 
                                      selected = 'All', 
                                      multiple = FALSE),
                        
                        plotOutput("plot3" ),
                        
                        offset = 1

                      ) 
                )

              )

  )
