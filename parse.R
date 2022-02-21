library(jsonlite)
data1 <- read.csv("metrics.csv")
data2 <- split(data1, f=list(data1$qc_program,data1$qc_metric))
list_json <- list()
for (i in 1:length(data2)){
  df <- data.frame(data2[i])
  lojf <- fromJSON(paste0('[',toString(df[,6]),']'))
  if (typeof(lojf)=="list"){
    df_j <- do.call(rbind, lojf)
  }else{
    df_j <- data.frame()
    for (l in 1:nrow(df)){
      df_l<-as.data.frame(fromJSON(fromJSON(paste0('[',toString(df[l,6]),']'))))
      df_j = rbind(df_j, df_l)
    }
  }
  list_json = append(list_json, list(df_j))
  names(list_json)[length(list_json)]=names(data2)[i]
  }

