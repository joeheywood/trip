source("trip.R")
library(shiny)

shinyServer(function(input, output, session) {
    # remove?
    updateSelectizeInput(session, "restoList", choices = 1:3, server = TRUE)
    
    observeEvent(input$addRest, {
        output$restTable <- renderDataTable({
            if(input$restoScore > -1) {
                addRev(input$restoList, input$restoScore)
            } else {
                NULL
            }
        }, escape = FALSE)
    })
    
    observeEvent(input$getData, {
        output$selectedRev <- renderText({
            rv <- getRev() 
            selected <- input$restTable_rows_selected
            getDataForRest(rv$name[selected], rv$url[selected])
        })
    })
    
    output$restTable <- renderDataTable({getRev()}, escape = FALSE)
    
    output$resMessage <- renderText({
        if(nchar(input$addResto) > 3) {
            dd <- findRest(input$addResto)
            opts <- as.list(paste0(dd$url, "***", dd$name))
            names(opts) <- dd$name
            updateSelectizeInput(session, "restoList", choices = opts)
            "Choose option..."
        } else {
            "Not enough characters"
        }
    })
    
    output$selectedRev <- renderText({
        rv <- getRev() 
        rv$url[input$restTable_rows_selected]
    })

})
