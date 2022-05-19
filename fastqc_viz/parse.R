library(jsonlite)
library ("purrr")

data1 <- read.csv("metrics.csv")
data2 <- split(data1, f=list(data1$qc_program,data1$qc_metric))
list_json <- list()
for (i in 1:length(data2)){
  df <- data.frame(data2[i])
  lojf <- fromJSON(paste0('[',toString(df[,6]),']'))
  if (typeof(lojf)=="list"){
    if (!is.na(df[,2][1])){
      lojf_new=list()
      for (ii in 1:length(lojf)){
        df_f=data.frame(lojf[ii])
        if(!is_empty(df_f)){
          df_f$db_id=df[,2][ii]
          df_f$read_type=df[,5][ii]
          lojf_new = append(lojf_new,list(df_f))
        }
      }
      lojf = lojf_new
    } 
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

N_count=list_json$fastqc.perbaseNcontent



data_meta <- read.csv("samplemeta.csv")
name="library_name"

data_meta[data_meta["library_strategy"]=="OTHER",]$db_id
#data1=data[data$read_type ==1, ]
data1[data1$db_id == "SRS114766_SRX026690", ]
data_s[data_s$db_id=="SRS114766_SRX026690",]
data_meta[data_meta$db_id=="SRS114766_SRX026690",]$library_layout


