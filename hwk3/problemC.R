boardgame <- function(nreps,inipos){
    nwins <- 0
    pos <- inipos
    winning <- 0
    for (rep in 1:nreps){
        roll <- sample(1:6,1)
        pos <- pos + roll
        if (pos > 7){
            pos <- pos - 8 
            nwins <- nwins + 1
            winning <- winning + pos + 1
        }
    }
    print(nreps/nwins)
    print(winning/nreps)
}
boardgame(20000,0)


boardexpected <- function(nreps,inipos){
    wins <- vector(length = nreps)
    for (rep in 1:nreps){
        pos <- inipos
        won <- 0
        rolls <- 0
        while (won == 0){
            roll <- sample (1:6,1)
            pos <- pos + roll
            rolls <- rolls + 1
            if (pos > 7){
                won <- won + 1
            }
        }
        wins[rep] <- rolls
    }
    print(mean(wins))
}
boardexpected(20000,0)
boardexpected(20000,1)
boardexpected(20000,2)
boardexpected(20000,3)
boardexpected(20000,4)
boardexpected(20000,5)
boardexpected(20000,6)
boardexpected(20000,7)
