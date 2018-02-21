
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
    print("The long-run average number of rolls between wins:")
    print(nreps/nwins)
    print("The long-run value of total winnings per turn.")
    print(winning/nreps)
}
boardgame(50000,0)


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
                break
            }
        }
        wins[rep] <- rolls
    }
    print(paste0("ET",inipos))
    #print(inipos)
    print(mean(wins))
}

boardexpected(50000,0)
boardexpected(50000,1)
boardexpected(50000,2)
boardexpected(50000,3)
boardexpected(50000,4)
boardexpected(50000,5)
boardexpected(50000,6)
boardexpected(50000,7)


