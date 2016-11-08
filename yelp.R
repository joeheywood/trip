library(readr)
library(rvest)
library(jsonlite)
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
    nxt <- TRUE
    rvs = list()
    while(nxt == TRUE) {
        s <- html_session(lk)
        revs <- html_nodes(s, ".review-list")
        users <- html_nodes(revs, ".user-passport-info") 
        # find .next button
        rvs <- c(rvs, lapply(users, getReviewer))
    }
    rvs
}

getReviewer <- function(bl) {
    nd <- html_node(bl, ".user-display-name") # %>% html_attr("href")
    usrid <- gsub(".*userid=(.*)", "\\1", nd %>% html_attr("href"))
    usrName <- nd %>% html_text()
    tryCatch({getReviewerReviewsPage(usrid, usrName)}, error = function(e) {
        print(e)
        print(paste0("NO DATA FOR ", usrName))
        data.frame()
    })
}

getReviewerReviewsPage <- function(usrid, uNm) {
    lkh <- sprintf(u, usrid)
    print(lkh)
    s <- html_session(lkh)
    if(httr::status_code(s) == 404) {
        return(data.frame())
    }
    revs <- html_nodes(s, ".user-details_reviews") %>% html_nodes(".review")
    rr <- lapply(revs, getSingleReview, uNm = uNm)
    do.call(rbind, rr)
}

getSingleReview <- function(rev, uNm) {
    cnt <- rev %>% html_nodes(".review-content") %>% 
        html_nodes(".review-content") %>% html_node("p") %>% 
        html_text()
    if(length(cnt) != 1) {
        print("THIS AIN'T RIGHT content:")
        print(cnt)
        cnt <- c(cnt, "NOWT")[1]
    }
    rnm <- rev %>% html_nodes("a.biz-name") %>% html_text()
    if(length(rnm) != 1) {
        print("THIS AIN'T RIGHT bizname:")
        print(rnm)
        rnm <- c(rnm, "NOWT")[1]
    }
        
    data.frame(content = cnt,
               restName = rev %>% html_nodes("a.biz-name") %>% html_text(),
               rating = rev %>% html_nodes(".star-img") %>% html_attr("title"),
               city = "yelp",
               link = rev %>% html_nodes("a.biz-name") %>% html_attr("href"),
               reviewer = uNm, stringsAsFactors = FALSE  )
}




