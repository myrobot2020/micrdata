fee<-function(x){
  library(stringi)
  x$urbanity<-substr(df$V1,5,5) |>  stri_replace_all_regex(pattern=seq(1:2),replacement= na.omit(code$urbanity),vectorize_all = F)
  x$ownership<-substr(df$V1,6,6) |>  stri_replace_all_regex(pattern=seq(1:3),replacement= na.omit(code$ownership),vectorize_all = F)
  print(x)
  
}


a<-fee(df)
