source("trip.R")
library(shiny)

shinyServer(function(input, output, session) {
    updateSelectizeInput(session, "restoList", choices = 1:3, server = TRUE)
    
    output$resMessage <- renderText({
        if(nchar(input$addResto) > 3) {
            dd <- findRest(input$addResto)
            opts <- as.list(dd$url)
            names(opts) <- dd$name
            updateSelectizeInput(session, "restoList", 
                                 choices = opts)
            paste0("Looking for ", input$addResto)
        } else {
            "Not enough characters"
        }
    })
    
    output$res <- renderText({
        if(input$restoScore > -1) {
            addRev(input$restList, input$restoScore)
        }
        addRev(input$restList, input$restoScore)
        input$restoList
    })
  

})
