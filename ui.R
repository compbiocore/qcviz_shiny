
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



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "graph_tab",
            h2(titlePanel("Graph Tab"),
               fluidRow(
                 box(
                   title = "Stacked Bar Chart"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("testPlot", height = "400px")
                 ),
                 box(
                   title = "Line Plot Chart"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("lineGraph", height = "400px")
                 ),
                 box(
                   title = "Heat Map"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("heatMap", height = "400px")
                 ),
                 box(
                   title = "Multi-line Graph"
                   ,status = "primary"
                   ,solidHeader = TRUE 
                   ,collapsible = TRUE 
                   ,plotOutput("multilineGraph", height = "400px")
                 ),
               )
            ),
            
    ),
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
               )
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "QCDB"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Graph Tab", tabName = "graph_tab", icon = icon("chart-bar")),
      menuItem("HPC Runs", tabName = "hpc_runs", icon = icon("chart-bar"))
    )
  ),
  body
)