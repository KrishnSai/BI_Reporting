#https://rstudio-pubs-static.s3.amazonaws.com/27777_55697c3a476640caa0ad2099fe914ae5.html#/6
#http://rstudio.github.io/shiny/tutorial/#html-ui
#https://shiny.rstudio.com/articles/html-tags.html

# import the libraries 
library(shiny)

# load the global variables
source('common_references.R', local= F)

# create the oracle connection 
connHandle_1 <- RODBC::odbcConnect("ORA_XE", uid = "SYSTEM", pwd = "1234")

ui = 
  

  
  # list the navigation bar with a shiny theme
  navbarPage(

    useShinyjs(),
    theme = shinytheme("united"),

    # Tab displaying the weekly performance
    tabPanel(
      #HTML(paste("This text is ", tags$span(style="color:red", "red"), sep = "")),
      style = 'margin-bottom:30px;border:3px double; padding: 10px;text-align: Left',      
      "Weekly Performance Report",
      dateRangeInput("range", "",
                 start  = "2019-01-15",
                 end    = "2019-01-31",
                 min    = "2005-01-01",
                 max    = "3000-12-21",
                 format = "mm/dd/yy",
                 separator = icon(class="far fa-arrow-alt-circle-right", "fa-1x")),align="center",
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
                        align=ind_flag,
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
                
                # Dropdown and pie chart for each employee 
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
