addRev <- function(lk, sc) {
    if(!dir.exists("../csv/")){
        dir.create("../csv")
    }
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