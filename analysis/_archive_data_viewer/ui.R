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
    titlePanel("Looking at one persons data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "select_ID",
                        label = "Choose an ID:",
                        choices = unique(df$ID)
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("smstPlot")
        )
    )
))
