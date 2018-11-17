library(shiny)
library(ggplot2)
library(markdown)

navbarPage("Mayor Pete Dashboard",
           tabPanel("Dan",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                                     )
                        ),
                      mainPanel(
                        plotOutput("plot")
                        )
                      )
                    ),
           tabPanel("Gerard",
                    verbatimTextOutput("summary")
                    ),
           tabPanel("Mike",
                    DT::dataTableOutput("table")
                    ),
           tabPanel("Tony",
                    verbatimTextOutput("structure")
           )
)