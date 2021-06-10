
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
server <- function(input, output, session) {
  #Test data used only for placeholder
  df <- read.table(text = "
Enrolment Applications Accepted Students Enrolled 
                 3 2017 30 25 5 20 
                 2 2016 24 21 3 20 
                 1 2015 22 20 2 17") 
  
  
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