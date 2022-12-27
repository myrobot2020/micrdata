o1<-names(code)

library(shiny)
library(purrr)
ui <- fluidPage(
  fileInput("file1", "File 1"),
  fileInput("file2", "File 2"),
  selectInput("a","Select option:",choices = names(code)),
  selectInput("b", "Select sub option:", choices = NULL),
  tableOutput("x")
)

server <- function(input, output,session) {
  
  observeEvent(input$a, {
    updateSelectInput(session, "b", choices = code[[input$a]])
  })


r1 <- reactive({
    as.list(c(input$file1$datapath,input$file2$datapath)) |> map(~read.csv(.,header = F)) |> map(fee) |> map(head)
})
  output$x<-renderTable({
     r1()
  })
}
shinyApp(ui = ui, server = server)
