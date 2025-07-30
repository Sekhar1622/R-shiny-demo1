# # # install.packages("shiny")
# # library(shiny)
# # library(plotly)
# # library(DT)
# # # ui <- fluidPage(
# # #   titlePanel("Shiny Elements Example"),
# # #   sidebarLayout(
# # #     sidebarPanel(
# # #       numericInput("numInput", "Enter a number:", value = 0),
# # #       sliderInput("numSlider", "Select a value:", min = 0, max = 100, value = 50),
# # #       sliderInput("numSlider", "Select a value:", min = 0, max = 100, value = c(50,60)),
# # #       actionButton("actionBtn", "Click Me"),
# # #       checkboxInput("singleCheckbox", "Check me"),
# # #       checkboxGroupInput("multiCheckbox", "Select options:",
# # #                          choices = c("Option 1", "Option 2", "Option 3")),
# # #       radioButtons("rb", "Choose one:",
# # #                    choiceNames = list(
# # #                      icon("angry"),
# # #                      icon("smile"),
# # #                      icon("sad-tear")
# # #                    ),
# # #                    choiceValues = list("angry", "happy", "sad")
# # #       ),
# # #       radioButtons("radioBtn", "Choose one:",
# # #                    choices = c("Option A", "Option B", "Option C"), selected = NULL),
# # #       selectInput("dropdownMenu", "Choose one:",
# # #                   choices = c("Option 1", "Option 2", "Option 3"), selected = NULL),
# # #       selectInput("dropdownMenu", "Choose one:",
# # #                   choices = c("Option 1", "Option 2", "Option 3"), selected = NULL,multiple = TRUE),
# # #       textInput("txtInput", "Enter text:"),
# # #       textAreaInput("story", "Tell me about yourself", rows = 3),
# # #       dateInput("dob", "When were you born?"),
# # #       dateRangeInput("holiday", "When do you want to go on vacation next?"),
# # #       fileInput("upload", "Upload csv file", accept = ".csv")
# # #     ),
# # #     mainPanel(
# # #     )
# # #   )
# # # )
# # 
# # ui <- fluidPage(
# #   textOutput("text"),
# #   verbatimTextOutput("code"),
# #   tableOutput("static"),
# #   dataTableOutput("dynamic"),
# #   plotOutput("plot", width = "400px"),
# #   plotlyOutput("plotly")
# # )
# # 
# # server <- function(input, output, session) {
# #   output$text <- renderText({
# #     "Hello friend!"
# #   })
# #   output$code <- renderPrint({
# #     summary(1:10)
# #   })
# #   output$static <- renderTable(head(mtcars))
# #   output$dynamic <- renderDT(mtcars, options = list(pageLength = 5))
# #   output$plotly <- renderPlotly({
# #     plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter", mode = "markers")
# #   })
# #   output$plot <- renderPlot(plot(1:5), res = 96)
# # }
# # shinyApp(ui, server)
# # 
# 
# 
# 
# 
# library(shiny)
# ui <- fluidPage(navbarPage(
#   "My Shiny App",
#   tabPanel("tab 1",
#            sidebarPanel(
#              fileInput("upload", "Upload csv file", accept = ".csv"),
#               numericInput("numInput", "Enter a number:", value = 0)
#            ),
#            mainPanel(
#              tableOutput("head")
#              # dataTableOutput("head")
#            )
#   )
# ))
# server <- function(input, output, session) {
#   data <- reactive({
#     req(input$upload)
#     read.csv(input$upload$datapath)
#   })
#    observeEvent(input$upload, {
#         showNotification("File uploaded", duration = 10)
#    
#    })
#   output$head <- renderTable({
#     if(input$numInput<5){validate("
#        enter number more than 5"
#      )}
#      head(data(), input$numInput)
#     head(data(), 10)
#   })
#   
#   # output$head <- renderDT(data(), options = list(pageLength = 5))
#   # output$head <- renderDT(data(), options = list(pageLength = 5))
# }
# shinyApp(ui, server)






library(shiny)
# library(shinythemes)
library(plotly)
ui <- fluidPage(#theme= shinytheme("united"),
  navbarPage(
    "My Shiny App",
    tabPanel("Data",
             sidebarPanel(
               fileInput("upload", "Upload csv file", accept = ".csv")
             ),
             mainPanel(
               tableOutput("head")
             )
    ),
    tabPanel("Table",
             sidebarPanel(
               selectInput("x", "X", choices = NULL),
               selectInput("y", "Y", choices = NULL),
               selectInput("color", "color", choices = NULL),
               actionButton("simulate", "plot",class = "btn-success")
             ),
             mainPanel(
               plotlyOutput("plot")
             )
    )
  ))
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    read.csv(input$upload$datapath)
  })
  
  output$head <- renderTable({
    head(data(), 10)
  })
  
  observeEvent(data(), {
    choices <- names(data())
    updateSelectInput(inputId = "x", choices = choices)
    updateSelectInput(inputId = "y", choices = choices)
    updateSelectInput(inputId = "color", choices = choices)
  })
  
  output$plot <- renderPlotly({
    if(input$simulate>0){
      plot_ly(data = data(), x = ~get(input$x), y = ~get(input$y), color = ~get(input$color), type = "scatter", mode = "markers")%>%
        layout(
          title = "Customized Scatter Plot",
          xaxis = list(title = input$x),
          yaxis = list(title = input$y)
        )
    }})
}
shinyApp(ui, server)


# 
# 
# 


# Load necessary libraries for the Shiny app and data manipulation
library(shiny)
library(formatters)  # For formatting data
library(DT)          # For rendering DataTables
library(shinythemes) # For applying themes to the UI
library(dplyr)       # For data manipulation
library(plotly)      # For interactive plots
# Define the user interface (UI) of the Shiny app
ui <- navbarPage(theme = shinytheme("united"), "MSx123",
                 tabPanel("Data",                           # Create a tab for data
                          sidebarPanel(
                            selectInput("usubjid", "USUBJID", choices = NULL),  # Dropdown for USUBJID
                            # Uncomment the following lines to add more dropdowns for ARM and SEX
                            # selectInput("arm", "ARM", choices = NULL),
                            # selectInput("sex", "SEX", choices = NULL),
                            actionButton("simulate", "Submit", class = "btn-success")  # Submit button
                          ),
                          mainPanel(
                            tabsetPanel(  # Create a set of tabs in the main panel
                              tabPanel("Subjects data",
                                       mainPanel(
                                         dataTableOutput("adsl")  # Output table for subjects data
                                       )),
                              tabPanel("Adverse Events",
                                       mainPanel(
                                         dataTableOutput("adae")  # Output table for adverse events
                                       )),
                              tabPanel("Lab Plot",
                                       mainPanel(
                                         plotlyOutput("plot")  # Output for interactive lab plot
                                       ))
                            )
                          )
                 ))
# Define the server logic for the Shiny app
server <- function(input, output, session) {
  
  # Observe changes in ex_adsl and update the USUBJID dropdown choices
  observeEvent(ex_adsl, {
    updateSelectInput(inputId = "usubjid", choices = ex_adsl$USUBJID)  # Update choices for USUBJID
    # Uncomment the following lines to update choices for ARM and SEX
    # updateSelectInput(inputId = "arm", choices = unique(ex_adsl$ARMCD))
    # updateSelectInput(inputId = "sex", choices = unique(ex_adsl$SEX))
  })
  
  # Render the subjects data table based on selected USUBJID
  output$adsl <- renderDT({
    if (input$simulate > 0) {  # Check if the submit button has been pressed
      adsl <- ex_adsl %>% filter(USUBJID == input$usubjid)  # Filter data for selected USUBJID
      adsl  # Return the filtered data
    } else {
      ex_adsl  # Return the full dataset if not simulated
    }
  })
  
  # Render the adverse events data table based on selected USUBJID
  output$adae <- renderDT({
    if (input$simulate > 0) {  # Check if the submit button has been pressed
      adae <- ex_adae %>% filter(USUBJID == input$usubjid)  # Filter data for selected USUBJID
      adae  # Return the filtered data
    } else {
      ex_adae  # Return the full dataset if not simulated
    }
  })
  
  # Render the lab plot based on selected USUBJID
  output$plot <- renderPlotly({
    if (input$simulate > 0) {  # Check if the submit button has been pressed
      adlb <- ex_adlb %>% filter(USUBJID == input$usubjid & PARAMCD == "ALT")  # Filter for ALT parameter
      plot_ly(data = adlb, x = ~ADY, y = ~AVAL, color = ~USUBJID, type = "scatter", mode = "lines") %>%
        add_lines() %>%
        layout(
          title = "Lab Plot",  # Title of the plot
          xaxis = list(title = "Time since treatment"),  # X-axis label
          yaxis = list(title = "ALT"),  # Y-axis label
          height = 800, width = 1200  # Dimensions of the plot
        )
    } else {
      adlb <- ex_adlb %>% filter(PARAMCD == "ALT")  # Filter all data for ALT parameter
      plot_ly(data = adlb, x = ~ADY, y = ~AVAL, color = ~USUBJID, type = "scatter", mode = "lines") %>%
        add_lines() %>%
        layout(
          title = "Lab Plot",  # Title of the plot
          xaxis = list(title = "Time since treatment"),  # X-axis label
          yaxis = list(title = "ALT"),  # Y-axis label
          height = 800, width = 1200  # Dimensions of the plot
        )
    }
  })
}
# Run the Shiny app
shinyApp(ui, server)




library(rmarkdown)

library(readxl)

library(haven)

library(knitr)

library(shiny)

library(formatters)

library(gt)

library(tidyverse)

library(rlistings)

library(shinythemes)

library(DT)
install.packages("shiny")
install.packages("formatters")
install.packages("gt")
install.packages("tidyverse")
install.packages("rlistings")
install.packages("shinythemes")
install.packages("DT")








