library(tidytext)
library(dplyr)
# http://nlp.stanford.edu/courses/cs224n/2010/reports/dpreston-rmelnick.pdf

getRev <- function(x, sents) {
    user <- sents$reviewer[which(sents$link == x)]
    user <- sents[which(sents$reviewer == user[1]),]
    sents <- sents[which(sents$link == x),]
    rating = mean(sents$rating)
    list(numSents = 0, rating = 0, mnRating = 0, snt = 0, mnSnt = 0,
         user = "", usrSent, sents = data.frame())
}

sentimentForSent <- function(x, sntD) {
    wds <- x %>% unnest_tokens(word, sentence)
    scrs <- left_join(wds, sntD)
    scrs$score[is.na(scrs$score)] <- 0
    scrs
}

buildSentimentDict <- function(sents, min) {
    vv <- sents %>% group_by(reviewer) %>% summarise(mnr = mean(rating))
    sents <- left_join(sents, vv)
    sents$rating <- sents$rating - sents$mnr
    uu <- unnest_tokens(sents[, c("rating", "reviewer", "link", "sid", "sentence")],
                        word, sentence)
    dct <- uu %>% group_by(word) %>% summarise(score = mean(rating), n = n())
    dct[which(dct$n > min), ]
}
