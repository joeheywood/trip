

if(!dir.exists("../csv/")){
    dir.create("../csv")
}

if(!dir.exists("../csv/restos")){
    dir.create("../csv/restos")
}

addRev <- function(lk, sc) {
    save(lk, sc, file = "debug.Rda")
    if(file.exists("../csv/savedRest.csv")) {
        ff <- read_csv("../csv/savedRest.csv") 
    } else {
        ff <- data.frame(name = c(), url = c(), score = c())
    }
    lu <- str_split(lk, "\\*\\*\\*")[[1]]
    lk <- lu[1]
    nm <- lu[2]
    ff <- rbind(ff, data.frame(name = nm, url = lk, score = sc, 
                               stringsAsFactors = F))
    write_csv(ff, "../csv/savedRest.csv")
    ff
}

getRev <- function() {
    if(file.exists("../csv/savedRest.csv")) {
        read_csv("../csv/savedRest.csv")
    } else {
        data.frame()
    }
}

removeRev <- function(x) {
    rv <- getRev()
    delRow <- which(rv$url == x)
    rv <- rv[-delRow, ]
    write_csv(ff, "../csv/savedRest.csv")
    paste0("Row with link ", x, " deleted.")
}