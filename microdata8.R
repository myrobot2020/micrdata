library(stringi)
library(purrr)
library(readxl)
library(dplyr)

options(max.print = 10)

df0 <- data.table::fread("C:/Users/HP/Desktop/India/sample data/SAMP_IND.txt", quote="\"") |> sample_n(size = 9999) %>% write_xlsx("india4.xlsx")
#df0 <- data.table::fread("C:/Users/HP/Desktop/India/sample data/india2.csv", quote="\"")
india2 <- read.csv("C:/Users/HP/Desktop/India/india2", header=FALSE)
india2$V1<-NULL
colnames(india2)<-c("V1")


df <- read_excel("india4.xlsx")


df$state<-substr(df$V1,1,2)
df$district<-substr(df$V1,3,4)
df$urbanity<-substr(df$V1,5,5)
df$ownership<-substr(df$V1,6,6)
df$housetype<-substr(df$V1,7,7)
df$electricity<-substr(df$V1,8,8)
df$drinkingwater<-substr(df$V1,9,9)
df$serialnumber<-substr(df$V1,10,10)
df$floor<-substr(df$V1,11,11)
df$wall<-substr(df$V1,12,12)
df$roof<-substr(df$V1,13,13)
df$purposes<-substr(df$V1,14,14)
df$condition<-substr(df$V1,15,15)
df$caste<-substr(df$V1,16,16)
df$dwellingrooms<-substr(df$V1,17,18)
df$couples<-substr(df$V1,19,20)
df$watersource<-substr(df$V1,21,21)
df$waterpremises<-substr(df$V1,22,22)
df$lightsource<-substr(df$V1,23,23)
df$latrine<-substr(df$V1,24,24)
df$wastewater<-substr(df$V1,25,25)
df$bathroom<-substr(df$V1,26,26)
df$kitchekn<-substr(df$V1,27,27)
df$fuel<-substr(df$V1,28,28)
df$radiotransisor<-substr(df$V1,29,29)
df$tv<-substr(df$V1,30,30)
df$computerinternet<-substr(df$V1,31,31)
df$phone<-substr(df$V1,32,32)
df$cycle<-substr(df$V1,33,33)
df$scooter<-substr(df$V1,34,34)
df$car<-substr(df$V1,35,35)
df$bank<-substr(df$V1,36,36)
df$weight<-substr(df$V1,37,39)
df$samplingrate<-substr(df$V1,40,42)
df$V1<-NULL
code<-  read_xls("C:/Users/HP/Desktop/India/annexure.xls")

fee<-function(x){
  x<-as.numeric(x)
  x<-data.frame(x)
  print(x)
}


df4<-map(df,fee)
