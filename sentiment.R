library(tidytext)
library(dplyr)
# http://nlp.stanford.edu/courses/cs224n/2010/reports/dpreston-rmelnick.pdf
source("clustering.R")
sents <- do.call(rbind, lapply(fls, getDataInSentences))
nn <- sents %>% group_by(restName, reviewer) %>% summarise(n = n())
nn$rrid <- 1:nrow(nn)
sents <- left_join(sents, nn %>% select(-n))
dct <- read_csv("../csv/dct.csv")
dct <- dct[which(is.na(dct$keep)),]


getRevSnt <- function(x) {
    usr <- sents$reviewer[which(sents$rrid == x)]
    user <- sents[which(sents$reviewer == usr[1]),]
    sentsa <- sents[which(sents$rrid == x),]
    sentim <-  sentimentForSent(sentsa[, c("sid", "sentence")], dct)
    mr <- round(sentsa$rating[1] - mean(user$rating), 2)
    general = data.frame(a = c("Number of Sents", "Rating", "Adjusted Rating", "Restaurant", "City"),
                         b = c(nrow(sentsa), sentsa$rating[1], mr, sentsa$restName[1], sentsa$city[1]))
    scrs <- sentim %>% group_by(sid) %>% summarise(score = sum(score)) %>%
        right_join(sentsa %>% select(sid, sentence))
    list(general = general, sents = scrs,
         sentim = split(sentim, as.factor(sentim$sid)))
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
