source("text_processing.R")
library(topicmodels)
library(tidyr)
fls <- dir("../csv/restos/", full.names = T)

getDataInSentences <- function(f) {
    prepRestDF(f) %>% unnest_tokens(sentence, content, token = "sentences") %>% 
        mutate(sid = row_number())
}

getDTMFromData <- function(bb) {
    bb <- bb %>% unnest_tokens(word, sentence) %>% 
        anti_join(stop_words)
    bb <- bb[which(is.na(as.numeric(bb$word))),]
    save(bb, file = "../csv/snt.Rda")
    bb %>% count(sid, word) %>% cast_dtm(sid, word, n)
}

getClusterInfo <- function(x, wc) {
    wcsid <- wc[which(wc$sid == 1),]
    a <- as.data.frame(table(wcsid$cluster), stringsAsFactors = F) %>% arrange(-Freq)
    a$Var1[1]
}

getHighestScore <- function(x, mx) {
    r <- mx[x,]
    mxScore <- max(r)
    data.frame(ldaTopic = which(r == max(r))[1], beta = mxScore, stringsAsFactors = F)
}

getLDAModel <- function(dtm, k) {
    LDA(dtm, k = k, control = list(seed = 1234))
}

tidyLDAobject <- function(lda) {
    tidytext:::tidy.LDA(lda)
}

flattenTidyObject <- function(obj) {
    obj$topic <- paste0("topic", obj$topic)
    gg <- spread(obj, key = topic, value = beta)
    numTopics <- length(colnames(gg)) - 1
    gg$tot <- rowSums(gg[, -1])
    gg$tp <- unlist(apply(gg[, -1], 1, which.max))
    
    for(i in 1:numTopics) {
        gg[[paste0("topic", i, "pc")]] <- round(gg[[paste0("topic", i)]] / gg$tot, 3)
    }
    gg
}

getSentenceLDA <- function(sid, bb, wds) {
    sdf <- bb[which(bb$sid == sid),]
    wdf <- inner_join(wds, sdf, by = c("term" = "word"))
    f <- colSums(wdf[, 2:(k+1)])
    g <-sum(f)
    ff <- f / g
    names(ff) <- paste0(names(ff), "_pc")
    out <- cbind(as.data.frame(t(f)), as.data.frame(t(ff)))
    out$sid <- sid
    out
}

saveTidyLookup <- function(k) {
    sents <- do.call(rbind, lapply(fls, getDataInSentences)) 
    sents$sid <- 1:nrow(sents)
    dtm <- sents %>% getDTMFromData() 
    wds <- dtm %>% getLDAModel(k) %>% tidyLDAobject() %>% 
        flattenTidyObject()
    save(k, wds, sents, dtm, file = "../csv/wds_snts.Rda")
    TRUE
}


