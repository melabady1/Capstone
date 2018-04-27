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
  titlePanel("Capstone Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("text", label = "Enter phrase:"),
      sliderInput("n", label = "Max. Number of suggestions:", value = 12,
                   max = 50, min = 1, step = 1),
      helpText("Enter a phrase and maximum number of suggeted words you want 
                to appear. 
                and you will get this number of suggested words that may complete 
                this phrase."),
      helpText("The output is a table contains columns of suggested words 
                that resulted from four prediction models (news, blogs, twitter 
                and a combined model from the three previous models).")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("result")
    )
  )
))
