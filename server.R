
library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(reshape2)
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
  df_dummy_boxplot <- read.table(text = "
  Education_Level Income  
  Doctorate 150050 
  Masters 100000 
  Bachelors 120000
  Doctorate 245678 
  Masters 235049 
  Bachelors 357609
  Doctorate 505200 
  Masters 50000
  Bachelors 75000
  Doctorate 120000 
  Masters 175275 
  Bachelors 124050
  Doctorate 9050 
  Masters 16700 
  Bachelors 12000
                   ") 
  
  
  
  
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
  
  
  #Create Box Plot
  output$box_plot<-renderPlot({
    ggplot(df_dummy_boxplot, aes(x=df_dummy_stacked_bar$Education_Level, y=df_dummy_stacked_bar$Income, color=dose)) +
    geom_boxplot()})
  
 # Attempting to convert matric's data column from a column of JSON objects to a separate dataframe 
 #json_df <- do.call(rbind.data.frame,lapply(metrics$data, FUN=function(x){ as.list(fromJSON(x))}))
 #ref_preview <- renderTable({head(ref_preview, 10)})
 #write.csv(json_df,"C:\\Users\\smari\\OneDrive\\Documents\\qcviz_shiny\\MyData.csv", row.names = FALSE)
 #Unsuccessful attempt: Error in (function (..., deparse.level = 1, make.row.names = TRUE, stringsAsFactors = default.stringsAsFactors(),  : 
 #numbers of columns of arguments do not match
}