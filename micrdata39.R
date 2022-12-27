library(shiny)
library(tidyverse)
ui <- fluidPage(
  fileInput("a", "Upload csv file"),
   uiOutput("b"),
  uiOutput("c"),
  tableOutput("x")
)

server <- function(input, output) {
  options(shiny.maxRequestSize = 3000*1024^2)
  r <- reactive({
    a<-read.csv(input$a$datapath,header = F)
    a<-fee(a)
  })
  
  output$b <- renderUI({
    selectInput('cols', 'Select Column', names(r()))
  })

  r2<-reactive({
    b<-r()[input$cols] |> table() |> prop.table() |> data.frame()
    
  })

  output$c <- renderUI({
    selectInput("factor", "Select factor", unique(r2()[1]))
  })

  # r3<-reactive({
  #   filter(r2(),!!sym(input$cols) %in% input$factor)
  # 
  # })


  output$x<-renderTable({
    r2()
  })
  
  
options(scipen=500,max.print = 100)
  
  
}


shinyApp(ui, server)