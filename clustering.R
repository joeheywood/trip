source("text_processing.R")
fls <- dir("../csv/restos/", full.names = T)
aa <- prepRestDF(fls[1])
bb <- aa %>% unnest_tokens(sentence, content, token = "sentences") %>% 
    mutate(sid = row_number())
    
save(bb, file = "../csv/bb.Rda")

bb <- bb %>% unnest_tokens(word, sentence) %>% 
    anti_join(stop_words)

bb <- bb[which(is.na(as.numeric(bb$word))),]

wc <- bb %>% count(sid, word)
wc_d <- cast_dtm(wc, sid, word, n)

d <- dist(t(wc_d), method="euclidian")
fit <- hclust(d=d, method="ward.D")
groups <- cutree(fit, k=5)
bb$cluster <- groups[bb$word]
wc$cluster <- groups[wc$word]

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
