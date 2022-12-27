library(shiny)


library(shiny)

ui <- fluidPage(
  selectInput("a","stratum",names(df)),
  uiOutput("b"),
  uiOutput("c"),
  tableOutput("x")
)

server <- function(input, output, session) {
  
  output$b <- renderUI({selectInput("b", "factor", choices =  unique(df[,input$a]))})
  
  r<-reactive({
    s<- filter(df,!!sym(req(input$a)) %in% req(input$b))
  })
  
  output$x<-renderTable({r()})
  
}

shinyApp(ui, server)