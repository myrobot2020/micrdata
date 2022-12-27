library(shiny)
library(tidyverse)
cars<-tibble(mtcars[1:11])
ui <- fluidPage(
  selectInput("a","selection1",names(cars)),
  uiOutput("b"),
  tableOutput("x")
)

server <- function(input, output, session) {
  
  output$b <- renderUI({selectInput("b", "selection 2", choices =  unique(cars[,input$a]))})
  
  r<-reactive({
    s<- filter(cars,!!sym(req(input$a)) %in% req(input$b))
  })
  
  

  
  output$x<-renderTable({r()})
}

shinyApp(ui, server)