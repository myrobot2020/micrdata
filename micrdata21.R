cars<-mtcars[8:11]


library(shiny)

ui <- fluidPage(
  selectInput("a","select variable for standard error",names(cars)),
  uiOutput("b"),
  tableOutput("x")
  
)

server <- function(input, output, session) {
  
  output$b <- renderUI({
    selectInput("ba", "select 2", choices =  unique(cars[,input$a]))     #   as.character(dat5[dat5$email==input$Select]))
  })
  
  r<-reactive({
    s<-cars[cars[[input$b]],input$a]
  })

  
  output$x<-renderTable({r()})
}

shinyApp(ui, server)