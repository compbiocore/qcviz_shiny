
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
                 ),
               )
            )
    ),
    tabItem(tabName = "new_tab",
            h2(titlePanel("Empty Tab")),
            fluidRow(
              box(
                title = "Box Plot Preview"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,tableOutput("box_plot")
              ),
            
            ),
            
            )
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