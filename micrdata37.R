library(shiny)
ui <- fluidPage(
  fileInput("upload", "Upload a file"),
  selectInput("a","select stratum",choices = names(df),selected = "roof"),
  uiOutput("b"),
  selectInput("c","select variable",choices = names(df),selected = "housetype"),
  tableOutput("x")
  
)
server <- function(input, output, session) {
  r<-reactive({
    
    df<-read.csv(file = input$upload$datapath,header = F)
    fee<-function(x){
      x$urbanity<-substr(x[1],5,5) |> stringi::stri_replace_all_regex(pattern=seq(1:2),replacement= code$urbanity,vectorize_all = F)
      x$ownership<-substr(x[1],6,6) |> stringi::stri_replace_all_regex(pattern=seq(1:3),replacement= code$ownership,vectorize_all = F)
      x$housetype<-substr(x[1],7,7) |> stringi::stri_replace_all_regex(pattern=seq(1:4),replacement= na.omit(code$house_type),vectorize_all = F)
      x$electricity<-substr(x[1],8,8) |> stringi::stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$electricity),vectorize_all = F)
      x$drinkingwater<-substr(x[1],9,9)|> stringi::stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$drinking),vectorize_all = F)
      x$serialnumber<-substr(x[1],10,10)
      x$floor<-substr(x[1],11,11) |> stringi::stri_replace_all_regex(pattern=seq(1:7),replacement= na.omit(code$floor),vectorize_all = F)
      x$wall<-substr(x[1],12,12) |> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$wall),vectorize_all = F)
      x$roof<-substr(x[1],13,13) |> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$roof),vectorize_all = F)
      x$purposes<-substr(x[1],14,14) |> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$purpose),vectorize_all = F)
      x$condition<-substr(x[1],15,15)|> stringi::stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$condition),vectorize_all = F)
      x$caste<-substr(x[1],16,16) |> stringi::stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$caste),vectorize_all = F)
      x$dwellingrooms<-substr(x[1],17,18) |> stringi::stri_replace_all_regex(pattern= sprintf('%0.2d', 6:99),replacement= "6 or more rooms",vectorize_all = F)
      x$couples<-substr(x[1],19,20)|> stringi::stri_replace_all_regex(pattern= sprintf('%0.2d', 5:99),replacement= "5 or more couples",vectorize_all = F)
      x$watersource<-substr(x[1],21,21)|> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$watersource),vectorize_all = F)
      x$waterpremises<-substr(x[1],22,22)|> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$watersource),vectorize_all = F)
      x$lightsource<-substr(x[1],23,23)|> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6),replacement= na.omit(code$lightsource),vectorize_all = F)
      x$latrine<-substr(x[1],24,24)|> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$latrine),vectorize_all = F)
      x$wastewater<-substr(x[1],25,25)|> stringi::stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$wastewater),vectorize_all = F)
      x$bathroom<-substr(x[1],26,26)|> stringi::stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$bathroom),vectorize_all = F)
      x$kitchen<-substr(x[1],27,27) |> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5),replacement= na.omit(code$kitchen),vectorize_all = F)
      x$fuel<-substr(x[1],28,28) |> stringi::stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$fuel),vectorize_all = F)
      x$radiotransisor<-substr(x[1],29,29) |> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$radiotransistor),vectorize_all = F)
      x$tv<-substr(x[1],30,30) |> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$tv),vectorize_all = F)
      x$computerinternet<-substr(x[1],31,31)|> stringi::stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$computerinternet),vectorize_all = F)
      x$phone<-substr(x[1],32,32)|> stringi::stri_replace_all_regex(pattern=c(1,2,3,4),replacement= na.omit(code$phone),vectorize_all = F)
      x$cycle<-substr(x[1],33,33)|> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$cycle),vectorize_all = F)
      x$twowheeler<-substr(x[1],34,34)|> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$twowheeler),vectorize_all = F)
      x$fourwheeler<-substr(x[1],35,35)|> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$fourwheeler),vectorize_all = F)
      x$bank<-substr(x[1],36,36)|> stringi::stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$bank),vectorize_all = F)
      x$weight<-substr(x[1],37,39)
      x$samplingrate<-substr(x[1],40,42)
      x[1]<-NULL
      x$serialnumber<-NULL
      x$samplingrate<-NULL
      x$weight<-NULL
      x$dwellingrooms<-NULL
      x<-print(x)
    }
    df |> fee() |> as.data.frame.matrix()
    # s<-filter(df,!!sym(req(input$a)) %in% req(input$b))
    # s<-s[,input$c]
    # n<-nrow(s)
    # s<-prop.table(table(s)) |> data.frame()#|> as.data.frame.matrix()
    # colnames(s)<-c("var","F")
    # s$p<-1-s$F
    # s$pp<-s$p*s$F
    # s$frac<-s$pp/n
    # s$sq<-sqrt(s$frac)
    # s$z<-s$sq*1.96
    # s$`upper confidence interval`<-s$F+s$z
    # s$`lower confidence interval`<-s$F-s$z
    # print(s)
  })
  
  output$x<-renderTable({
    r()
  },rownames = F,
  digits = 5)
}
shinyApp(ui, server)
