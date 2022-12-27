library(shiny)

ui <- fluidPage(
  selectInput("a","variable1",choices = names(mtcars)),
  selectInput("b","variable2",choices = names(mtcars)),
  tableOutput("x")
)

server <- function(input, output, session) {
  r<-reactive({
    r1<-table(mtcars[[input$a]],mtcars[[input$b]])
    
  })
  
  output$x<-renderTable({r()})
  
}

shinyApp(ui, server)



b<-paste("mtcars","$carb",sep = "")
