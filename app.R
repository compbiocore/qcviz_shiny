#June 8, 2021
#Find out about building apps here:
#http://shiny.rstudio.com/
#---------------------------------------
# New info:
#  You can include a lot of components in a dashboard body
#  including: boxes (colored, collapsible, tabs), Histograms, slide inputs,
#  infoBoxes (display numeric or text values)
#  You can also organize boxes (column-order, row-order, or mixed)
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

# CSV with qc metrics, stored as JSON entries
# Contains: Sample_ids, experiment_ids, qc_program,
# and qc_metrics
qc_metrics <- read.csv("metrics.csv")

# Holds shortened names for qc_metrics  
reference <- read.csv("reference.csv")

# Holds seuqencing tech, sequencing centers, organism
# tissue type, scientific name, job_id, library selection
# much more 
metrics <- read.csv("metrics.csv")

source("ui.R")

source("server.R")

shinyApp(ui = ui, server = server)