
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
library(shinythemes)
library(sparkline)
library(timevis)
library(DT)
library(shinycssloaders)
library(fontawesome)

source("parse.R")
source("dataframe.R")
#----------------------------------------------------------
ui <- shinyUI(fluidPage(
  theme = shinytheme("lumen"),
  
  titlePanel("FastQC"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 selectInput(inputId="main",
                             label= strong("Choose a main sample:"),
                             choices=unique(c(data$db_id,data_s$db_id,
                                              data_g$db_id,data_sq$db_id,
                                              data_q$db_id))
                             ),
                 actionButton("bt_samples", "Add 10 samples"),
                 actionButton("bt_clear", "Clear"),
                 br(),
                 checkboxGroupInput(inputId = "db_id",
                             label = strong("Compare with:"),
                             choices = unique(c(data$db_id,data_s$db_id,
                                                   data_g$db_id,data_sq$db_id,
                                                   data_q$db_id))
                              )
    ),
    mainPanel(width = 8,
              tabsetPanel(
                tabPanel(div("N content"),
                                 wellPanel(h3("FastQC: Per Base N Content"),
                                           fluidRow(
                                             #plotlyOutput("N_1"),
                                             plotlyOutput("N_1.5"),
                                             plotlyOutput("N_2.5")
                                             #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("N_1"), plotlyOutput("N_2"))
                                           )
                                 )
                                
              ),
              tabPanel(div("Sequence Content"),
                       wellPanel(h3("FastQC: Per Base Sequence Content"),
                                 fluidRow(
                                   plotlyOutput("SC_1"), 
                                   plotlyOutput("SC_2")
                                   #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("SC_1"), plotlyOutput("SC_2"))
                                 )
                                 )
                       ),
              tabPanel(div("GC Content"),
                       wellPanel(h3("FastQC: Per Sequence GC Content"),
                                 fluidRow(
                                   #plotlyOutput("GC_1"), 
                                   plotlyOutput("GC_1.5"), 
                                   plotlyOutput("GC_2.5"), 
                                   #plotlyOutput("GC_2")
                                   #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("GC_1"), plotlyOutput("GC_2"))
                                 )
                       )
              ),
              tabPanel(div("Quality Scores"),
                       wellPanel(h3("FastQC: Per Sequence Quality Scores"),
                                 fluidRow(
                                   #plotlyOutput("QS_1"), 
                                   plotlyOutput("QS_1.5"),
                                   plotlyOutput("QS_2.5"),
                                   #plotlyOutput("QS_2")
                                   #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("QS_1"), plotlyOutput("QS_2"))
                                 )
                       )
              ),
              tabPanel(div("Mean Quality Scores"),
                       wellPanel(h3("FastQC: Mean Quality Scores"),
                                 fluidRow(
                                   #plotlyOutput("MQS_1"), 
                                   plotlyOutput("MQS_1.5"),
                                   plotlyOutput("MQS_2.5"),
                                   #plotlyOutput("MQS_2")
                                   #splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("MQS_1"), plotlyOutput("MQS_2"))
                                 )
                       )
              )
              )
              )
    )
  )
)





