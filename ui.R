#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram


shinyUI(navbarPage(
    theme = shinytheme("spacelab"),
    "Capstone Project",
                   
                   ############################### ~~~~~~~~1~~~~~~~~ ##############################  
                   ## Tab 1 - App
                   
                   tabPanel("Next Word Predictor App",
                            
                            fluidRow(
                                
                                column(3),
                                column(6,
                                       tags$div(textInput("phrase", "Phrase", placeholder = "Enter text"),
                                                br(),
                                                tags$hr(),
                                                h4("Predicted next word:"),
                                                tags$span(style="color:lightblue",
                                                          tags$strong(tags$h3(textOutput("prediction")))),
                                                br(),
                                                align="center")
                                ),
                                column(3)
                            )
                   ),
                   
                   ############################### ~~~~~~~~2~~~~~~~~ ##############################
                   ## Tab 2 - About 
                   
                   tabPanel("About",
                            fluidRow(
                                column(2,
                                       p("")),
                                column(8,
                                       includeMarkdown("about.md")),
                                column(2,
                                       p(""))
                            )
                   ),
                   
                   ############################### ~~~~~~~~F~~~~~~~~ ##############################
                   
                   ## Footer
                   
                   tags$hr(),
                   
                   tags$br(),
                   
                   tags$span(style="color:grey", 
                             tags$footer(("© 2021 - "), 
                                         tags$a(
                                             
                                             href="https://www.linkedin.com/in/silvana-avramska-lukarska/",
                                             target="_blank",
                                             "Silvana Avramska-Lukarska."), 
                                         tags$br(),
                                         ("Built using"), tags$a(
                                             href="http://www.r-project.org/",
                                             target="_blank",
                                             "R"),
                                         ("&"), tags$a(
                                             href="http://shiny.rstudio.com",
                                             target="_blank",
                                             "Shiny."),
                                         
                                         align = "center"),
                             
                             tags$br()
                   )
)
)





