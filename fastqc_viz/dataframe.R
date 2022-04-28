library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tidyr)
library(plotly)
library("plyr")

source("parse.R")

#----------------------------------------
data=list_json$fastqc.perbaseNcontent
data$n <- as.numeric(data$n)
for (i in 1:nrow(data)){
  if(grepl("-", data[i,]$b)){
    twonum <- unlist(str_split(data[i,]$b, "-"))
    data[i,]$b=twonum[1]
    de <- list(b = twonum[2], n = data[i,]$n, db_id=data[i,]$db_id, read_type=data[i,]$read_type)
    data = rbind(data,de, stringsAsFactors=FALSE)
  }
}
data$b <- as.numeric(data$b)

#----------------------------------------
data_s=list_json$fastqc.perbaseseqcontent
data_s$a <- as.numeric(data_s$a)
data_s$c <- as.numeric(data_s$c)
data_s$g <- as.numeric(data_s$g)
data_s$t <- as.numeric(data_s$t)
for (i in 1:nrow(data_s)){
  if(grepl("-", data_s[i,]$b)){
    twonum <- unlist(str_split(data_s[i,]$b, "-"))
    data_s[i,]$b=twonum[1]
    de <- list(b = twonum[2], a = data_s[i,]$a, c = data_s[i,]$c,
               g = data_s[i,]$g, t = data_s[i,]$t, db_id=data_s[i,]$db_id,read_type=data[i,]$read_type)
    data_s = rbind(data_s,de, stringsAsFactors=FALSE)
  }
}
data_s$b <- as.numeric(data_s$b)

#----------------------------------------
data_g=list_json$fastqc.gccontent
data_g$c <- as.numeric(data_g$c)
data_g$g <- as.numeric(data_g$g)

#----------------------------------------
data_sq=list_json$fastqc.seqqual
data_sq$c <- as.numeric(data_sq$c)
data_sq$q <- as.numeric(data_sq$q)

#----------------------------------------
data_q=list_json$fastqc.basequal
data_q$m <- as.numeric(data_q$m)
for (i in 1:nrow(data_q)){
  if(grepl("-", data_q[i,]$b)){
    twonum <- unlist(str_split(data_q[i,]$b, "-"))
    data_q[i,]$b=twonum[1]
    for (n in (as.numeric(twonum[1])+1):as.numeric(twonum[2])){
      de <- list(b = n, l = data_q[i,]$l, m = data_q[i,]$m,
                 t = data_q[i,]$t,u = data_q[i,]$u, m1 = data_q[i,]$m1,
                 tp = data_q[i,]$tp,db_id=data_q[i,]$db_id,read_type=data[i,]$read_type)
      data_q = rbind(data_q,de, stringsAsFactors=FALSE)
    }
    
  }
}
data_q$b <- as.numeric(data_q$b)



