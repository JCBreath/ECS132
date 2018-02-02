boardsim <- function(nreps) {
   varb <- 0
   eb <- 0
   for (i in 1:nreps) {
      start <- 0
      position <- start + sample(1:6,1)
      bonus <- 0
      if (position == 3) {
         bonus <- sample(1:6,1)
         position <- (position + bonus) %% 8
      }
      varb <- varb + bonus^2
      eb <- eb + bonus
   }
    eb <- eb / nreps
    varb <- varb / nreps - eb^2
   c(varb, eb)
}

set.seed(99999)
cat("Var(B) =",boardsim(1000000)[1],"\n")
cat("EB =",boardsim(1000000)[2],"\n")