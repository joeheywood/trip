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
