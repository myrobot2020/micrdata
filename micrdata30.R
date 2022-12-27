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
  
  
  output$c<-renderUI({selectInput("c", "samp error var", choices =  names(r())) })

  # r1<-reactive({
  #
  # })
  #
  # r3<-reactive({
  #    s<-data.frame(intersect(r(), r1()))
  #  })
  
   output$x<-renderTable({r()})
}

shinyApp(ui, server)