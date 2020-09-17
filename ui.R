#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
# Define UI for application that draws a Marimekko chart
fluidPage(
    titlePanel("Marimekko Chart Generator 2.0"),
    sidebarPanel(
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")), 
            radioButtons("caltype", "Choose one:",selected = "This", 
                         choices = c(
                             "This" = "This",
                             "That" = "That")),
    ),
    mainPanel(
        plotOutput("MosaicPlot",width="100%") 
    )
) 
