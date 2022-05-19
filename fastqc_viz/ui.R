
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
library(shinyjs)
source("parse.R")
source("dataframe.R")
#----------------------------------------------------------
ui <- shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("lumen"),
  
  titlePanel("FastQC"),
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 radioButtons(inputId = "layout",
                              label = strong("library_layout:"),
                              choices = c("SINGLE","PAIRED"),
                              selected = "SINGLE"
                              ),
                 
                 uiOutput("pull1"),
                 uiOutput("radio1"),
                 
                 uiOutput("pull2"),
                 uiOutput("radio2"),
                 
                 uiOutput("pull3"),
                 uiOutput("radio3"),
                 
                 uiOutput("pull4"),
                 uiOutput("radio4"),
                 
                 uiOutput("pull5"),
                 uiOutput("radio5"),
                 
                 uiOutput("pull6"),
                 uiOutput("radio6"),
                 
                 uiOutput("pull7"),
                 uiOutput("radio7"),
                 
                 actionButton("button1", "Add filter"),
                 hidden(actionButton("button2", "Add filter")),
                 hidden(actionButton("button3", "Add filter")),
                 hidden(actionButton("button4", "Add filter")),
                 hidden(actionButton("button5", "Add filter")),
                 hidden(actionButton("button6", "Add filter")),
                 hidden(actionButton("button7", "Add filter")),
                 hidden(actionButton("filter", "Add filter")),
                 actionButton("remove", "Remove filter"),
                 
                 hr(),
                 
                 uiOutput("mian_sample"),
                 
                 actionButton("bt_samples", "Add 5 samples"),
                 
                 actionButton("bt_clear", "Clear"),
                 
                 uiOutput("choice_sample"),
                 
                 
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





