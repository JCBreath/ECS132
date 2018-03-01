simProbB <- function(nreps,lambda,alpha,beta) {
	E_D <- mean(rpois(nreps, lambda)*rbeta(nreps,alpha,beta,ncp=0))
	E_D_2 <- mean(rpois(nreps, lambda)^2*rbeta(nreps,alpha,beta,ncp=0))
	Var_D <- E_D_2 - E_D^2
	cat("Mean:",E_D,'\n')
	cat("Variance:",Var_D,'\n')
}

#simProbB(10000,1,0.2,0.2)