#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- function(){
  #' @export
  fluidPage(
  
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
      
      uiOutput('uiParameter'),
      actionButton("filterButton", "Filter Parameters")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(selected = "Data/Plot",
                  tabPanel("Data/Plot",
                           br(),
                           uiOutput('result')),
                  tabPanel("User guide",
                           br(),
                           includeHTML(system.file("userGuide.html", package = "ecviz"))
                          )
                  )
 
    )
  )
)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
#' @export
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
          
          dataLong <- ecviz::format_wq_data(df)
          
          return(dataLong)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )
    })
  
  output$uiParameter <- renderUI({
    data <- dataset()
    selectizeInput("selectParameter",
                label = "Select parameter:",
                choices = c(unique(data$Analyte), ""),
                selected = "",
                multiple=T)
  })
  

  dataShow <- reactive(
    {req(input$file1)
    if(is.null(input$selectParameter) ||input$selectParameter == ""|| input$filterButton == 0){
      dataOut <- dataset() } else {
        dataOut <- dataset()
         dataOut <- dataOut %>% filter(Analyte %in% input$selectParameter)
      }
      return(dataOut)
    }
    
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
    ecviz::plot_wq_data(dataOut)
  })
  
}


