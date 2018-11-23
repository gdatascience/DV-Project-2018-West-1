library(shiny)
library(ggplot2)
library(markdown)

server = function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(cars)
  })
  
  output$structure <- renderPrint({
    str(cars)
  })
}

ui = navbarPage("Mayor Pete Dashboard",
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


shinyApp(ui = ui, server = server)