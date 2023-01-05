a<-1
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


o1<-names(code)

library(shiny)
library(purrr)
ui <- fluidPage(
  fileInput("file1", "File 1"),
  fileInput("file2", "File 2"),
  selectInput("a","Variable of Stratum",choices = names(code)),
  selectInput("b", "Factor of Stratum", choices = NULL),
  selectInput("c","Variable for Estimate:",choices = names(code)),
  selectInput("d", "Factor to Estimate", choices = NULL),
  tableOutput("x")
)

server <- function(input, output,session) {
  
  observeEvent(input$a, {
    updateSelectInput(session, "b", choices = code[[input$a]])
  })
  
  observeEvent(input$c, {
    updateSelectInput(session, "d", choices = code[[input$c]])
  })
  
  
  
  r1 <- reactive({
    as.list(c(input$file1$datapath,input$file2$datapath)) |> map(~read.csv(.,header = F)) |> map(fee) |> map(head)
  })
  output$x<-renderTable({
    r1()
  })
}
shinyApp(ui = ui, server = server)
