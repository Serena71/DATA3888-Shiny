#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Discipline project 2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput("fs", "Select the number of Principal Components", 
                         value = 5, min = 1, max = 88),
            checkboxGroupInput("metric", 
                        "Select Performance Metrics",
                        choices = c("Accuracy", 
                                    "F1 Score",
                                    "Balanced Accuracy")
                        ),
            actionButton("plot", "Generate Plot"),
            
            
            
            actionButton("CM", "Show Confusion Matrix")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            conditionalPanel(
                h4("Boxplot for Performance Evaluation"),
                br(),
                condition = "input.plot",
                plotOutput("plot")
                
                
            ),
            conditionalPanel(
                h4("Confusion Matrix for 1 repeat CV"),
                br(),
                condition = "input.CM",
                verbatimTextOutput("table")
                
                
            )
            
            
        )
    )
))
