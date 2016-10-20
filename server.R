source("trip.R")
library(shiny)

shinyServer(function(input, output, session) {
    updateSelectizeInput(session, "restoList", choices = 1:3, server = TRUE)
    
    observeEvent(input$addRest, {
        output$restTable <- renderDataTable({
            if(input$restoScore > -1) {
                rv <- addRev(input$restoList, input$restoScore)
                rv$Select <- paste0('<input type="checkbox" name="row" value="', 
                                    rv$url, '">')
                rv
            } else {
                NULL
            }
        }, escape = FALSE)
    })
    
    observeEvent(input$showRest, {
        output$restTable <- renderDataTable({
            rv <- getRev()
            rowId <- 1:nrow(rv)
            rv$Select <- paste0('<input type="checkbox" name="row', rowId, 
                                '" value="', rv$url, '">')
            rv
        }, escape = FALSE)
    })
    
    output$resMessage <- renderText({
        if(nchar(input$addResto) > 3) {
            dd <- findRest(input$addResto)
            opts <- as.list(dd$url)
            names(opts) <- dd$name
            updateSelectizeInput(session, "restoList", choices = opts)
            "blah"
        } else {
            "Not enough characters"
        }
    })
    
    output$selectedRev <- renderText({
        out <- ""
        # for(i in 1:5) {
        #     if(input[[paste0("row", i)]]) {
        #         out <- paste0(out, " ", input[[paste0("row", i)]])
        #     }
        # }
        out
    })
    
    
  

})
