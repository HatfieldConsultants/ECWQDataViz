#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(testApp)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Visualizations of EC Freshwater Quality Monitoring and Surveillance Online Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all",
                               Figure = "figure"),
                   selected = "head"),
      # Horizontal line ----
      tags$hr(),
      
      uiOutput('uiParameter')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput('result')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataset <- reactive(
    {req(input$file1)
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
      tryCatch(
        {
          df <- read.csv(input$file1$datapath)
          
          dataLong <- format_wq_data(df)
          
          return(dataLong)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    })
  
  output$uiParameter <- renderUI({
    data <- dataShow()
    selectInput("selectParameter",
                label = "Select parameter:",
                choices = c(unique(data$Analyte), ""),
                selected = "")
  })
  
  dataShow <- reactive(
    {req(input$file1)
    if(is.null(input$selectParameter) ||
       input$selectParameter == ""){
      dataOut <- dataset() } else {
        dataOut <- dataset() %>% filter(input$selectParameter)}
    }
    return(dataOut)
  )

  output$result <- renderUI(
    { switch(input$disp,
             head=tableOutput('head'),
             all=tableOutput('all'),
             figure=plotOutput('figure'))
    }
  )
  
  output$head <- renderTable({
    dataOut <- dataShow()
    return(head(dataOut))})
  
  output$all <- renderTable({
    req(input$file1)
    dataOut <- dataShow()
    return((dataOut))})
  
  output$figure <- renderPlot({
    req(input$file1)
    dataOut <- dataShow()
    plot_wq_data(dataOut)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
