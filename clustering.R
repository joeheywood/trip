source("text_processing.R")
library(topicmodels)
library(tidyr)
fls <- dir("../csv/restos/", full.names = T)

getDataInSentences <- function(f) {
    prepRestDF(f) %>% unnest_tokens(sentence, content, token = "sentences") %>%
        mutate(sid = row_number())
}

getDTMFromData <- function(sents) {
    sents_w <- sents %>% unnest_tokens(word, sentence) %>%
        anti_join(stop_words)
    sents_w <- sents_w[which(is.na(as.numeric(sents_w$word))),]
    save(sents_w, file = "../csv/sents_w.Rda")
    sents_w %>% count(sid, word) %>% cast_dtm(sid, word, n)
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
    if(sid %% 100 == 0) print(sid)
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

getSentenceScores <- function() {
    load("../csv/wds_snts.Rda")
    load("../csv/sents_w.Rda")
    sentScores <- do.call(rbind, lapply(unique(sents_w$sid), getSentenceLDA,
                          bb = sents_w, wds = wds))
    save(sentScores, file = "../csv/sentScores.Rda")
}

loadData <- function() {
    load("../csv/wds_snts.Rda")
    load("../csv/sents_w.Rda")
    load("../csv/sentScores.Rda")
    list(sents = sents, sents_w = sents_w, sentScores = sentScores, k=k)
}




