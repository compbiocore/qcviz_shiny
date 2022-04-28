
#---------------------------------------------------------------
#N-content: 1 main sample, compare it with others (point + boxplot)
#GC-content: similar as N-content
#QS: group 11 samples, one against others, one into the band of others.
#---------------------------------------------------------------


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

source("parse.R")

source("dataframe.R")

source("ui.R")

source("server.R")

shinyApp(ui, server)