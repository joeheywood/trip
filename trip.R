library(dplyr)
library(rvest)
library(stringr)
library(jsonlite)
#load("tripAdvisor.Rda")




getReviewsPage <- function(lk, n = 100, minReviews = 5) {
    # get data about hotel - name, location, website? etc
    # get reviews divs
    out <- data.frame()
    nextLink <- TRUE
    orVal <- 10
    while(nextLink == TRUE) {
        s <- html_session(lk)
        revs <- html_node(s, "#REVIEWS") %>% html_nodes(".basic_review")
        rx <- do.call(rbind, lapply(revs, getReview))
        out <- rbind(out, rx)
        nextDis <- "disabled" %in% strsplit(html_nodes(s, "a.next") %>% 
                                                 html_attr("class"), " ")[[1]]
        if(nextDis == TRUE | nrow(out) >= n ) {
            nextLink <- FALSE
        } else {
            orVal <- orVal + 10
            lk <- gsub("-Reviews-", paste0("-Reviews-or", orVal, "-"), lk)
            print("*************************************************************")
        }
    }
    out

}


getReview <- function(rv) {
    # get name of reviewer, call getAddReviewer()
    contr <- html_node(rv, ".memberOverlayLink") %>% html_attr(name = "id")
    contr <- gsub("UID_", "uid=", contr)
    contr <- gsub("-SRC_", "&src=", contr)
    html_session(paste0("https://www.tripadvisor.co.uk/MemberOverlay?Mode=owa&",
                        contr)) %>% 
        html_node("a") %>% 
        html_attr("href") %>%
        getAddReviewer()
}

getAddReviewer <- function(nm) {
    # TO ADD: ratings, name of restaurant, city etc.
    print(paste0("Getting user page... ", nm))
    s <- html_session(paste0("https://www.tripadvisor.co.uk", nm))
    revs <- html_nodes(s, "li.cs-review")
    print("Getting links..")
    links <- vapply(revs, getReviewLink, "")
    links <- paste0("https://www.tripadvisor.co.uk",links[which(nchar(links) > 0)])
    #content <- vapply(links, getReviewContent, "")
    # change reviewer passed as argument
    #data.frame(reviewer = nm, links = links, content = content)
    do.call(rbind, lapply(links, getReviewContent, rev = nm))
}

getReviewLink <- function(r) {
    type <- gsub("cs-type-hint sprite-feed", "", 
                 html_node(r, ".cs-type-hint") %>% html_attr("class") )
    if(type == "Restaurant") {
        html_node(r, ".cs-review-title") %>% html_attr("href")
    } else {
        ""
    }
}

getReviewContent <- function(link, rev) {
    tryCatch({
        print(paste0("Getting review from ", link))
        revNum <- gsub(".*-r(\\d+)-.*", "\\1", link)
        s <- html_session(link)
        data.frame(content = html_node(s, paste0("p#review_", revNum)) %>% 
                       html_text(),
                   restName = str_trim(html_nodes(s, "a.HEADING") %>% 
                                           html_text()),
                   rating = html_node(s, paste0("div#review_", revNum)) %>% 
                       html_node("img.sprite-rating_s_fill") %>% 
                       html_attr("alt"),
                   link = link,
                   reviewer = rev, stringsAsFactors = FALSE  )
    }, error = function(e){
        print(e)
        data.frame()
    })
    # should return data frame.. with rating and link?
}

getTargetRating <- function(x, df, t) {
    target <- df$rating[which(df$restName == t & df$reviewer == x)][1]
    rep(target, length(which(df$reviewer == x)))
} 

findRest <- function(x) {
    findNm <- "https://www.tripadvisor.co.uk/TypeAheadJson?types=eat&query=%s&action=API&uiOrigin=MASTHEAD&source=MASTHEAD&startTime=1476457447850"
    x <- gsub(" ", "%20", x)
    typ <- fromJSON(sprintf(findNm, x))
    data.frame(name = typ$results$name, url = typ$results$url)
}

addRev <- function(lk, sc) {
    print(getwd())
    load("blah.Rda")
    ff <- rbind(ff, data.frame(url = lk, score = sc))
    save(ff, file = "blah.Rda")
}