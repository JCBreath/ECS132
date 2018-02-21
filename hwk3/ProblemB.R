#skewness = E[(N - EN)^3 / stdvN^3] 
#r = 3, p = 0.4



skew <- function(nreps,r,p){
    
    Result_V <- vector(length = nreps)
    SK_V <- vector(length = nreps)
    Var_V <- vector(length = nreps)
    
    #simulate the game, and put results in Result_V
    for (rep in 1:nreps){
        total <- 0
        number <- 0
        while (total < r){
            get = sample(0:1,1,prob = c((1-p),p),replace = TRUE)
            total = total + get
            number = number + 1
        }
        Result_V[rep] = number
    }
    
    EN <- mean(Result_V)
    print(EN)
    
    for (rep_1 in 1:nreps){
        Var_V[rep_1] = (Result_V[rep_1] - EN)^2
    }
    Var <- mean(Var_V)           
    stdV <- sqrt(Var)
    
    print(Var)
    
    for (rep_2 in 1:nreps){
        SK_V[rep_2] = ((Result_V[rep_2] - EN)^3 /(stdV^3))
    }
    
    result <- mean(SK_V)
    print(result)
}

skew(10000,3,0.4)
