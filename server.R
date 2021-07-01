
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
metrics <- read.csv("metrics.csv")

# Holds shortened names for qc_metrics  
reference <- read.csv("reference.csv")

# Holds seuqencing tech, sequencing centers, organism
# tissue type, scientific name, job_id, library selection
# much more 
server <- function(input, output, session) {
  #Test data used only for placeholder
  df_dummy_stacked_bar <- read.table(text = "
  Enrollment Applications Accepted Students Enrolled 
                 3 2017 30 25 5 20 
                 2 2016 24 21 3 20 
                 1 2015 22 20 2 17") 
linePlotData <- read.csv("lineGraphData.csv") 

heatMapData <- read.csv("heatMapData.csv")

multiLineData <- read.csv("multiLineData.csv")

  
  
  
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
    df_dummy_stacked_bar$Enrollment <- factor(df_dummy_stacked_bar$Enrollment)
    df_dummy_stacked_bar$Not_Accepted <- df_dummy_stacked_bar$Applications - df_dummy_stacked_bar$Accepted
    df_dummy_stacked_bar$Not_Enrolled <- df_dummy_stacked_bar$Accepted - df_dummy_stacked_bar$Enrolled
    df2 <- melt(df_dummy_stacked_bar[, c("Enrollment", "Enrolled", "Not_Enrolled", "Not_Accepted")])
    
    ggplot(df2, aes(Enrollment, y = value, fill = variable)) +
      geom_bar(stat = "identity")
  })
  
  
  output$lineGraph <- renderPlot({ ggplot(data=linePlotData, aes(x=Month, y=Depth, group=1)) +
    geom_line()})
  
  output$heatMap <- renderPlot({ggplot(data = heatMapData, aes(x=Month, y=Station, fill=Temp)) + 
      geom_tile()})
  
  output$multilineGraph <- renderPlot({ggplot(data=multiLineData, aes(x=Month, y=A, group=Station)) +
      geom_line()})
  
  
  
}