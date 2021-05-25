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


#To-Do:
#Change static selectInputs to a reactive output object, this will be set up in the ui 
#and we will create the output$selectInputs object in the server 
#We will create reactive output objects for both variables 

#pivot_wider, might want to go wide to long

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "hpc_runs",
            h2(titlePanel("HPC Runs"),
              fluidRow(
              box(
                title = "Select Parameters", solidHeader = TRUE, background = "black",
                collapsible = TRUE,
                selectInput(inputId = "program_type", label = "Select a Program:", "Names"),
                selectInput(inputId = "var1", label = "Select Variable 1:", "Variables"),
                selectInput(inputId = "var2", label = "Select Variable 2:", "Variables2"),
                numericInput(inputId = "num_obs", label = "Number of Observations to view:", value = 5, min = 1, max = 20)),
              box(
                title = "Stacked Bar Chart"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("testPlot", height = "300px")
              )
              )
           )
    ),
    tabItem(tabName = "new_tab",
            h2(titlePanel("Empty Tab")))
  )
)


# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "QCDB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("HPC Runs", tabName = "hpc_runs", icon = icon("chart-bar")),
      menuItem("New Tab", tabName = "new_tab", icon = icon("grin"))
      
    )
  ),
  body
)
# I have to make a stacked barchart :|

server <- function(input, output, session) {
  #Test data used only for placeholder
  df <- read.table(text = "
Enrolment Applications Accepted Students Enrolled 
                 3 2017 30 25 5 20 
                 2 2016 24 21 3 20 
                 1 2015 22 20 2 17") 
  #TODO: Define datasetInput and plotInput below using reactive function

  #jobids <- worth while to look into lapply, lambda function ?
  
  #TODO: Implement reactive function for variables 1 and 2 below
  
  
  #Create list of programs (Picard and fastqc are the only two)
  observe({
    updateSelectInput(session, "program_type", choices = reference$qc_program)
  })
  
  #Create list of Variables (are these variables correct?)
  observe({
    updateSelectInput(session, "var1", choices = reference$qc_metric)
  })
  
  observe({
    updateSelectInput(session, "var2", choices = reference$field_name)
  })
  
  #TODO: Implement renderTable function below
  
  #TODO: Implement renderPlot function 
  output$testPlot <- renderPlot({
    df$Enrolment <- factor(df$Enrolment)
    df$Not_Accepted <- df$Applications - df$Accepted
    df$Not_Enrolled <- df$Accepted - df$Enrolled
    df2 <- melt(df[, c("Enrolment", "Enrolled", "Not_Enrolled", "Not_Accepted")])
    
    ggplot(df2, aes(Enrolment, y = value, fill = variable)) +
      geom_bar(stat = "identity")
  })
  
  
}

shinyApp(ui = ui, server = server)
