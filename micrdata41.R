library(shiny)

ui <- fluidPage(
  fileInput("a","s1"),
  fileInput("b","s2"),
  
  tableOutput("z")
)

server <- function(input, output, session) {
  r1<-reactive({
    read.csv(input$a$datapath,header = F) |> fee()
  })
  
  r2<-reactive({
    read.csv(input$b$datapath,header = F) |> fee()
  })
  
  r3<-reactive({
    list(r1(),r2())
  })
  output$z<-renderTable( )
  
}

shinyApp(ui, server)