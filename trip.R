library(dplyr)
library(rvest)
library(stringr)
library(jsonlite)
library(readr)

source("manageRevs.R")

getReviewsPage <- function(lk, n = 100, minReviews = 5) {
    out <- data.frame()
    nextLink <- TRUE
    orVal <- 10
    restName <- "unknown"
    while(nextLink == TRUE) {
        s <- html_session(lk)
        restName <- html_node(s, "h1#HEADING") %>% html_text() %>% str_trim()
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
    write_csv(out, "../csv/", str_replace(restName, " ", "_"))
    out
}

getReview <- function(rv) {
    # get the rating 
    thisRat <- html_nodes(rv, ".rating_s_fill") %>% html_attr("alt") %>% 
    str_extract("\\d") %>% as.numeric()
    contr <- html_node(rv, ".memberOverlayLink") %>% html_attr(name = "id")
    contr <- gsub("UID_", "uid=", contr)
    contr <- gsub("-SRC_", "&src=", contr)
    html_session(paste0("https://www.tripadvisor.co.uk/MemberOverlay?Mode=owa&",
                        contr)) %>% 
        html_node("a") %>% html_attr("href") %>% getAddReviewer()
}

getAddReviewer <- function(nm) {
    print(paste0("Getting user page... ", nm))
    s <- html_session(paste0("https://www.tripadvisor.co.uk", nm))
    revs <- html_nodes(s, "li.cs-review")
    print("Getting links..")
    links <- vapply(revs, getReviewLink, "")
    links <- paste0("https://www.tripadvisor.co.uk",links[which(nchar(links) > 0)])
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
                   city = html_node(s, "div.slim_ranking a") %>% html_text(),
                   link = link,
                   reviewer = rev, stringsAsFactors = FALSE  )
    }, error = function(e){
        print(e)
        data.frame()
    })
    # should return data frame.. with rating and link?
}

getDataForRest <- function(nm, lk) {
    # placeholder for just get reviews...
    paste0("Get associated reviews for ", nm, " at link: ", lk)
}

findRest <- function(x) {
    findNm <- "https://www.tripadvisor.co.uk/TypeAheadJson?types=eat&query=%s&action=API&uiOrigin=MASTHEAD&source=MASTHEAD&startTime=1476457447850"
    x <- gsub(" ", "%20", x)
    typ <- fromJSON(sprintf(findNm, x))
    data.frame(name = typ$results$name, url = typ$results$url, 
               stringsAsFactors = FALSE)
}


