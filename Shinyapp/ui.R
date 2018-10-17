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


shinyUI(material_page(title = "Predict the next word",
                      tags$br(),
                      font_color = "cyan darken-5",
                      nav_bar_color = "cyan darken-5",
                      
                      material_row(
                        tags$br(),
                        div(p('Thank you for using my shinyapp!! You can put any frase in the box and click "See the next word"'),
                            p("To see the code of this shinyapp, please visit my Github reppo"),
                            a(href="https://github.com/Kwatanwa17/Capstone", "Repository link here"),
                            align = "center")
                      ),
                      
                      material_row( # ---------------
                                    
                                    material_column(width = 4, 
                                                    offset = 2,
                                                    material_text_box(
                                                      input_id = "pred_text",
                                                      label = "Your text here:",
                                                      color = "#ef5350"), 
                                                    align = "center"
                                    ),
                                                    
                                    material_column(width = 4,
                                                    material_button(
                                                      input_id = "pred_button",
                                                      label = "See the next word",
                                                      depth = 5,
                                                      color = "deep-orange"), 
                                                    align = "center"
                                    )
                      ),
                      

                        material_row(material_column(width = 8, 
                                                     offset = 2,
                                                     material_card(
                                                       title = "The next word is: ",
                                                       depth = 3,
                                                       textOutput("pred_word")),
                                                     align = "center"
                        )
                        )                            
                                    
                      
  
))
  
  
  
  
  
