library(shiny)
library(tidyverse)
ui <- fluidPage(
  fileInput("a", "Upload csv file"),
  uiOutput("b"),
  uiOutput("c"),
  tableOutput("x")
)

server <- function(input, output) {
  r <- reactive({
    read.csv(input$a$datapath)
  })
  
  output$b <- renderUI({
    selectInput('cols', 'Select Column', names(r()))
  })
  
  r2<-reactive({
    r()[input$cols]
  })
  
  output$c <- renderUI({
    selectInput("factor", "Select factor", unique(r2()))
  })
  
  r3<-reactive({
    filter(r2(),!!sym(input$cols) %in% input$factor)
  })

  output$x<-renderTable({
    r3()
  })

}


shinyApp(ui, server)