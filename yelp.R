library(readr)
y <- "https://www.yelp.com/search_suggest/v1/prefetch?loc=London&loc_name_param=find_loc&prefix=%s"

u <- "https://www.yelp.com/user_details_reviews_self?userid=%s&rec_pagestart=0"

findRestYlp <- function(x) {
    x <- gsub(" ", "+", x)
    typ <- fromJSON(sprintf(y, x))
    ddf <- lapply(typ$response$suggestions, function(x) {
        data.frame(name = x$title[1], url = x$redirect_url[1],
                   stringsAsFactors = FALSE)
    })
    do.call(rbind, ddf)
}

getYelpPage <- function(lk) {
    lk <- sprintf("https://www.yelp.com%s", lk)
    s <- html_session(lk)
    revs <- html_nodes(s, ".review-list")
    users <- html_nodes(revs, ".user-passport-info")
}

getReviewer <- function(bl) {
    nd <- html_node(bl, ".user-display-name") # %>% html_attr("href")
    usrid <- gsub(".*userid=([^\\s]+).*", "\\1", nd %>% html_attr("href"))
    usrName <- nd %>% html_text()
}

getReviewerReviewsPage <- function(usrid, uNm) {
    s <- html_session(sprintf(u, usrid))
    revs <- html_nodes(s, ".user-details_reviews") %>% html_nodes("li")
    rr <- lapply(revs, getSingleReview, uNm = uNm)
    do.call(rbind, rr)
}

getSingleReview <- function(rev, uNm) {
    data.frame(content = rev %>% html_nodes(".review-content") %>% 
                   html_nodes(".review-content") %>% html_node("p") %>% 
                   html_text(),
               restName = rev %>% html_nodes("a.biz-name") %>% html_text(),
               rating = rev %>% html_nodes(".star-img") %>% html_attr("title"),
               city = "yelp",
               link = rev %>% html_nodes("a.biz-name") %>% html_attr("href"),
               reviewer = uNm, stringsAsFactors = FALSE  )
}

