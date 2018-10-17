#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  InputText <- eventReactive(input$pred_button, {
    
    predNextWordUpdated(input$pred_text)


  })
  
  output$pred_word <- renderText({
    
    print(InputText())
    
    })

  
})

 