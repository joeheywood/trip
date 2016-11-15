library(tidytext)
library(dplyr)
library(readr)
#library(topicmodels)


# rt <- read_csv("../csv/restos/Roti_King.csv")
# rt$rating <- as.numeric(gsub(" of 5 bubbles", "", rt$rating))

wordFreq <- function(x) {
    x %>% unnest_tokens(word, content) %>% 
        anti_join(stop_words) %>% count(word, sort = TRUE)
}

intoSentences <- function(row) {
    x <- row['content']
    id <- row['id']
    xx <- str_split(str_trim(x), "[!.?]")[[1]]
    str_trim(xx[which(nchar(xx) > 0)])
    data.frame(id = id, sent = xx[which(nchar(xx) > 0)])
}

prepRestDF <- function(x) {
    ty <- read_csv(x)
    ty$rating <- as.numeric(gsub(" of 5 bubbles", "", ty$rating))
    ty
}





