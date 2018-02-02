# ProblemC.R in HwkII
# Siyuan Yao

Sim_B <- function() {
	return(sample(0:2, 1, prob = c(0.5, 0.4, 0.1)))
}

Exp_B <- function(nreps) {
	EB <- 0
	for(i in 1:nreps) {
		EB <- EB + Sim_B()
	}
	EB <- EB / nreps
	return(EB)
}

Var_B <- function(nreps) {
	EB <- Exp_B(nreps)
	var <- 0
	for(i in 1:nreps) {
		var <- var + (Sim_B() - EB) ^ 2
	}
	var <- var / nreps
	return(var)
}

StdDev_B <- function(nreps) {
	# sqrt(Var(B))
	return(sqrt(Var_B(nreps)))
}

Skewness_B <- function(nreps) {
	EB <- Exp_B(nreps)
	SDB <- StdDev_B(nreps)
	SB <- 0
	for(i in 1:nreps) {
		# (X-mu)^3 / sigma^3
		SB <- SB + (((Sim_B() - EB) ^ 3) / (SDB ^ 3))
	}
	SB <- SB / nreps
	return(SB)
}

print(Skewness_B(10000))