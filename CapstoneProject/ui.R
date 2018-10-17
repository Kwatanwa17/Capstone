#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)
library(dplyr)
library(stringr)
library(shinymaterial)

# Define UI for application that draws a histogram


shinyUI(fluidPage(
  titlePanel("Predict next word"),
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "pred_text",
                label = "Your text here:",
                value = "I predict the next word"),
      
      actionButton(inputId = "pred_button",
                   label = "See the next word")
      
    ),
    
    mainPanel(
      
      verbatimTextOutput(outputId = "pred_text")
      
      
    )
  )
))