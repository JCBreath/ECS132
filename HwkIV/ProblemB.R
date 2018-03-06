simProbB <- function(nreps,lambda,alpha,beta) {
	N <- rpois(nreps, lambda)
	p <- rbeta(nreps,alpha,beta,ncp=0)
	E_D <- mean(N*p)
	Var_D <- mean((N*p-E_D)^2)
	cat("Mean:",E_D,'\n')
	cat("Variance:",Var_D,'\n')
}
simProbB(1000000,1,0.2,0.2)
