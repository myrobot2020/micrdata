library(shiny)

# Define the user interface
ui <- fluidPage(
  # Add 5 file input widgets
  fileInput("file1", "File 1"),
   fileInput("file2", "File 2"),
  tableOutput("x")
)

# Define the server logic``
server <- function(input, output) {
  
  # Create a reactive expression that reads the uploaded files and saves them as a list of dataframes
  r1 <- reactive({
    
      as.list(input$file1$datapath,input$file2$datapath) |> map(~read.csv(.,header = F)) |> map(fee) |> map(head)
      #|> map(fee)
      #a <- read.csv(input$file1$datapath,header = F)
      
#      file2 = read.csv(input$file2$datapath)) #|> 
  })
  
  # Create an observer that displays the head of the list of dataframes when the submit button is clicked
    output$x<-renderTable({
       r1()
      #data.frame(map(data(),fee))
      #data() |> map(fee) |> head()
      
    }) 
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
