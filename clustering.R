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
    bb %>% count(sid, word) %>% cast_dtm(sid, word, n)
}


# d <- dist(t(wc_d), method="euclidian")
# fit <- hclust(d=d, method="ward.D")
# groups <- cutree(fit, k=5)
# bb$cluster <- groups[bb$word]
# wc$cluster <- groups[wc$word]

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

getSentenceLDA <- function(sid, df, k = 4) {
    sidDf <- df[which(df$sid == sid), ]
    kScores <- rep(0, k)
    for(r in 1:nrow(sidDf)) {
        rowTopic <- sidDf$ldaTopic[r]
        rowBeta <- sidDf$beta[r]
        kScores[rowTopic] <- kScores[rowTopic] + rowBeta
    }
    which(kScores == max(kScores))[1]
}
