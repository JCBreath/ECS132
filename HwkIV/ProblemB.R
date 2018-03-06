simProbB <- function(nreps,lambda,alpha,beta) {
	nums <- rpois(nreps,lambda)
	defrat <- rbeta(nreps,alpha,beta,ncp=0)
	defNum <- sum(runif(nums[1])<defrat[1])
	for (i in 2:nreps){
		s <- sum (runif(nums[i])<defrat[i])
		defNum <- c(defNum,s)
	}	
	mean = mean(defNum)
	var = sum((defNum-mean)^2)/nreps
	cat("Mean:",mean,'\n')
	cat("Variance:",var,'\n')
}	
simProbB(50000,1,0.2,0.2)
	
