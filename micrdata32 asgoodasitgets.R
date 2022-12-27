  library(shiny)
  library(stringi)
  library(purrr)
  library(readxl)
  library(dplyr)
  library(ggplot2)
  
  options(max.print = 15)
  
  df <- read_excel("C:/Users/HP/Desktop/India/india4.xlsx")
  code<-list("urbanity"=c("rural","urban"),
                   "ownership"=c("owned","rented","others"),
                   "house_type"=c("Permananent","Temporary","Semi_permanent","others"),
                   "electricity"=c("light","no_light"),
                   "drinking"=c("good","other"),
                   "floor"=c("Mud","Wood/Bamboo","Burnt_Brick","Stone","Cement","Mosaic/FloorTiles","Any_other_materials"),
                   "wall"=c("Grass/Thatch/Bamboo","Plastic/Polythene","Mud/Unburnt_brick","Wood","Stone_not_packed_with_Mortar",
                            "Stone_packed_with_Mortar","G.I./Metal/Asbestos","Burnt_Brick","Concrete","Any_other_materials"),
                   "roof"=c("Grass/Thatch/Bamboo/Wood/Mud_etc.","Plastic/Polythene","Hand_made_Tiles","Machine_made_Tiles",
                            "Burnt_Brick","Stone","Slate","G.I./Metal/Asbestos","Concrete","Any_other_materials"),
                   "purpose"=c("only_residence","residence/other"),
                   "condition"=c("Good","Livable","Dilapidated"),
                   "caste"=c("SC","ST","Others"),
                   "watersource"=c("Tap_water_from_treated_source","Tap_water_from_un_treated_source","Covered_well",
                                   "Un-covered_well","Hand_pump","Tubewell/Borehole","Spring","River/Canal","Tank/Pond/Lake","Other_source"),
                   "lightsource"=c("Electricity","Kerosene","Solar","Other_oil","Any_other","No_lighting"),
                   "latrine"=c("Pile_sewer_system","Septic_tank","Other_system","Pit_latrine_with_slab/ventilated_improved_pit",
                               "Pit_latrine_without_slab/open_pit","Night_soil_disposed_into_open_drain","Night_soil_removed_by_human",
                               "Night_soil_serviced_by_animals","Public_latrine","Open"),
                   "wastewater"=c("Closed_Drainage","Open_Drainage","No_Drainage"),
                   "bathroom"=c('Yes',"Enclosure without roof","No"),
                   "kitchen"=c("Cooking_inside_house_has_kitchen","Cooking_inside_house_does_not_have_kitchen",
                               "Cooking_outside_house_has_kitchen","Cooking_outside_house_does_not_have_kitchen","No_cooking"),
                   "fuel"=c('Firewood',"Crop_residue","Cowdung_cake","Coal/Lignite/Charcoal","Kerosene","LPG/PNG","Electricity",
                            "Bio-gas","Any_other","No_cooking"),
                   "radiotransistor"=c("Yes","No"),
                   "tv"=c("Yes","No"),
                   "computerinternet"=c("Yes_with_internet","Yes_without_internet","No"),
                   "phone"=c("Yes_Landline_only","Yes_Mobile_only","Both","None"),
                   "cycle"=c("Yes","No"),
                   "twowheeler"=c("Yes","No"),
                   "fourwheeler"=c("Yes","No"),
                   "bank"=c("Yes","No")
  )
  
  
  df$urbanity<-substr(df$V1,5,5) %>% stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$urbanity),vectorize_all = F)
  df$ownership<-substr(df$V1,6,6) %>% stri_replace_all_regex(pattern=seq(1:3),replacement= na.omit(code$ownership),vectorize_all = F)
  df$housetype<-substr(df$V1,7,7) %>% stri_replace_all_regex(pattern=seq(1:4),replacement= na.omit(code$house_type),vectorize_all = F)
  df$electricity<-substr(df$V1,8,8) %>% stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$electricity),vectorize_all = F)
  df$drinkingwater<-substr(df$V1,9,9)%>% stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$drinking),vectorize_all = F)
  df$serialnumber<-substr(df$V1,10,10)
  df$floor<-substr(df$V1,11,11) %>% stri_replace_all_regex(pattern=seq(1:7),replacement= na.omit(code$floor),vectorize_all = F)
  df$wall<-substr(df$V1,12,12) %>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$wall),vectorize_all = F)
  df$roof<-substr(df$V1,13,13) %>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$roof),vectorize_all = F)
  df$purposes<-substr(df$V1,14,14) %>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$purpose),vectorize_all = F)
  df$condition<-substr(df$V1,15,15)%>% stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$condition),vectorize_all = F)
  df$caste<-substr(df$V1,16,16) %>% stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$caste),vectorize_all = F)
  df$dwellingrooms<-substr(df$V1,17,18) %>% stri_replace_all_regex(pattern= sprintf('%0.2d', 6:99),replacement= "6 or more rooms",vectorize_all = F)
  df$couples<-substr(df$V1,19,20)%>% stri_replace_all_regex(pattern= sprintf('%0.2d', 5:99),replacement= "5 or more couples",vectorize_all = F)
  df$watersource<-substr(df$V1,21,21)%>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$watersource),vectorize_all = F)
  df$waterpremises<-substr(df$V1,22,22)%>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$watersource),vectorize_all = F)
  df$lightsource<-substr(df$V1,23,23)%>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6),replacement= na.omit(code$lightsource),vectorize_all = F)
  df$latrine<-substr(df$V1,24,24)%>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$latrine),vectorize_all = F) 
  df$wastewater<-substr(df$V1,25,25)%>% stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$wastewater),vectorize_all = F)
  df$bathroom<-substr(df$V1,26,26)%>% stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$bathroom),vectorize_all = F)
  df$kitchen<-substr(df$V1,27,27) %>% stri_replace_all_regex(pattern=c(1,2,3,4,5),replacement= na.omit(code$kitchen),vectorize_all = F)
  df$fuel<-substr(df$V1,28,28) %>% stri_replace_all_regex(pattern=c(1,2,3,4,5,6,7,8,9,0),replacement= na.omit(code$fuel),vectorize_all = F)
  df$radiotransisor<-substr(df$V1,29,29) %>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$radiotransistor),vectorize_all = F) 
  df$tv<-substr(df$V1,30,30) %>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$tv),vectorize_all = F)
  df$computerinternet<-substr(df$V1,31,31)%>% stri_replace_all_regex(pattern=c(1,2,3),replacement= na.omit(code$computerinternet),vectorize_all = F)
  df$phone<-substr(df$V1,32,32)%>% stri_replace_all_regex(pattern=c(1,2,3,4),replacement= na.omit(code$phone),vectorize_all = F)
  df$cycle<-substr(df$V1,33,33)%>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$cycle),vectorize_all = F)
  df$twowheeler<-substr(df$V1,34,34)%>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$twowheeler),vectorize_all = F)
  df$fourwheeler<-substr(df$V1,35,35)%>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$fourwheeler),vectorize_all = F)
  df$bank<-substr(df$V1,36,36)%>% stri_replace_all_regex(pattern=c(1,2),replacement= na.omit(code$bank),vectorize_all = F)
  df$weight<-substr(df$V1,37,39)
  df$samplingrate<-substr(df$V1,40,42)
  df$V1<-NULL
  df$serialnumber<-NULL
  df$samplingrate<-NULL
  df$weight<-NULL
  df$dwellingrooms<-NULL
  
  
  
  library(shiny)
  ui <- fluidPage(
    selectInput("a","select stratum",choices = names(df),selected = "roof"),
    uiOutput("b"),
    selectInput("c","select variable",choices = names(df),selected = "housetype"),
    plotOutput("d"),
    tableOutput("x")
  )
  server <- function(input, output, session) {
    output$b<-renderUI({
      selectInput("b","select factor",choices = unique(df[,input$a]))
    })
   
     r<-reactive({
      s<-filter(df,!!sym(req(input$a)) %in% req(input$b))
      s<-s[,input$c]
      n<-nrow(s)
      s<-prop.table(table(s)) |> data.frame()#|> as.data.frame.matrix()
      colnames(s)<-c(input$c,"F")
      s$p<-1-s$F
      s$pp<-s$p*s$F
      s$frac<-s$pp/n
      s$sq<-sqrt(s$frac)
      s$z<-s$sq*1.96
      s$LCI<-s$F-s$z
      s$UCI<-s$F+s$z
      s<-s[,c(1,2,8,9)]
      print(s)
      })

     
    output$x<-renderTable({
      r()
    },rownames = F,
    digits = 5)
    
    output$d<-renderPlot({
      ggplot(r(), aes(x = input$c, y = F)) +
        geom_point() +
        geom_errorbar(aes(ymin = LCI, ymax = UCI))
    })
    
  }
  shinyApp(ui, server)
