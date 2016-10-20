
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
            h1("Some results.."),
            p(textOutput("res")),
            dataTableOutput("restTable"),
            p(actionLink("showRest", "Show restaurants")),
            p(textOutput("selectedRev"))
        )
    )
) 
)
