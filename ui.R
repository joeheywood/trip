
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source("trip.R")

library(shiny)
library(DT)

shinyUI(fluidPage(
    titlePanel("Joe's tripadvisor scrape project"),
    sidebarLayout(
        sidebarPanel(
            textInput("addResto", label = "Add restaurant"),
            selectizeInput("restoList", choices = NULL, label = "Resto"),
            selectizeInput("restoScore", label = "Score",
                           choices = list(None = -1, Average = 3, Good = 4, 
                                          Excellent = 5)),
            shiny::textOutput("resMessage"),
            p(actionLink("addRest", "Add rating"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Restos",
                    p(textOutput("res")),
                    dataTableOutput("restTable"),
                    p(actionLink("getData", "Get data"), " | ",
                      actionLink("removeRev", "Remove")),
                    p(textOutput("selectedRev"))
                ),
                tabPanel(
                    "Collected Data",
                    dataTableOutput("revsTable")
                ),
                tabPanel(
                    "LDA analysis",
                    p(actionLink("load_lda", "Load latest lda")),
                    h5(textOutput("k")),
                    # sliderInput("totScore", "Tot score:", min = 0.0654, 
                    #             max = 142, value = 0.5),
                    selectizeInput("topicList",  label = "Topic:",
                                   choices = paste0("Topic ", 1:k)),
                    dataTableOutput("topWords"),
                    dataTableOutput("topSents")
                )
            )
            
        )
    )
) 
)
