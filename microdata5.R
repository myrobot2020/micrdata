library(stringi)
library(purrr)
library(readxl)

options(max.print = 10)

df<-read.table("C:/Users/HP/Desktop/India/sample data/SAMP_IND.txt", quote="\"", comment.char="")
code<-read_xls("C:/Users/HP/Desktop/India/annexure.xls")

df1<-list(
  state=table(substr(df$V1,1,2)),
  district=table(substr(df$V1,3,4)),
  urbanity=setNames(table(substr(df$V1,5,5)),c("R","U")),
  ownership=setNames(table(substr(df$V1,6,6)),c("Owned","Rented","Others")),
  housetype=setNames(table(substr(df$V1,7,7)),c("Permananent","Temporary","Semi_permanent","others")),
  
  )
