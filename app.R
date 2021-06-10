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
# PLANNING
# 1) Identify CSV file we're working with
# 2) Load Data
# 3) How are I going to parse data? (Look at https://github.com/brown-ccv/hpc-runs/blob/main/shiny/app.R)
# 4) Organize components using Boxes and FluidRow
#     - Encapsulate boxes in fluidrows to deal with resizing
# 5) Change drop-down menus (program, var1, var2, num_obs) to be reactive
#     - define input and output objects using updateSelectInput() to dynamically set menus, var selection
# 6) Define reactive graphs (stacked barchart)
#     - Format data, also using reactive ()
#     - use ggplot's geom_bar with stat = identity
#       ggplot(data, aes(GRAPH_TITLE, y = var1 , fill = var2)) +
#       geom_bar(position = "stack", stat = "identity")
# Problem: Making sense of data (JSOn objects?)
#   - This might be helpful https://www.geeksforgeeks.org/working-with-json-files-in-r-programming/
#     Possibly convert JSON object in 'data' into a Dataframe (as.data.frame())
#     import library(json) in that case
#

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