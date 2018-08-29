#!/usr/bin/env Rscript
library(shiny)
library(shinydashboard)

source("server.R")

ui <- dashboardPage(dashboardHeader(disable = T),
              dashboardSidebar(disable = T),
              dashboardBody(uiOutput("MainBody")
                            
                            )
              
)

app <- shinyApp(ui = ui, server = server)
options(shiny.autoreload=TRUE)
runApp(app, port=8080)
