source("trip.R")
source("clustering.R")
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
        print("Starting...")
        output$revsTable <- renderDataTable({
            print("getting rev..")
            rv <- getRev() 
            print("Getting selection")
            selected <- input$restTable_rows_selected
            print("here...")
            getDataForRest(rv$name[selected], rv$url[selected])
        })
    })
    
    observeEvent(input$removeRev, {
        output$selectedRev <- renderText({
            rv <- getRev() 
            selected <- input$restTable_rows_selected
            removeRev(rv$url[selected])
        })
    })
    
    observeEvent(input$load_lda, {
        l <- loadData()
        tpc <- as.numeric(gsub("Topic ", "", input$topicList))
        output$k <- renderText({paste0("Number of topics: ", l$k, " | ", tpc)})
        output$topWords <- renderDataTable({
            stt <- which(colnames(wds) == "tot")
            stp <- which(colnames(wds) == "topic6pc")
            wds$tot <- round(wds$tot*1000, 4)
            wds <- wds[which(wds[[paste0("topic", tpc, "pc")]] > 0.35),]
            wds[, c(1, stt:stp)] %>% arrange(-tot)
        })
        output$topSents <- renderDataTable({
            ss <- sents %>% select(sid, sentence) %>% 
                left_join(sentScores ) 
            tpcVar <- paste0("topic", tpc, "_pc")
            ss[[tpcVar]] <- round(ss[[tpcVar]]*100, 1)
            ss[which(ss[[tpcVar]] > 35),c("tot", "sentence", tpcVar) ] %>%
                arrange(-tot)
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
