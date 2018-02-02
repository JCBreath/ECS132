# ProblemB.R in HwkII
# Siyuan Yao

# PAMsim in HwkI:Problem C
PAMsim <- function(nNodes) {
	# initialize degree vec
	degVec = c(1, 1)
	for (nodeCount in 2:(nNodes - 1)) {
		
		# initialize prob vec
		pVec = c()

		# build prob vec
		for(nodeNum in 1:length(degVec)) {
			pVec <- c(pVec, (degVec[nodeNum] / (nodeCount - 1) / 2))
		}
		
		# choose a node to form an edge
		selIndex = sample(1:nodeCount, 1, replace = T, prob = pVec)

		# add degree to selected node
		degVec[selIndex] <- degVec[selIndex] + 1
		
		# append new node to degree vec
		degVec <- c(degVec, 1)

	}
	return(degVec)
}

# Expected Value
EXP <- function(nreps, nodeIndex, nodeCount) {
	Ev <- 0
	for (i in 1:nreps) {
		degVec <- PAMsim(nodeCount)
		Ev <- Ev + degVec[nodeIndex]
	}
	Ev <- Ev / nreps
	return(Ev)
}

# Variance
Var <- function(nreps, nodeIndex, nodeCount) {
	# Get E(v)
	Ev <- EXP(nreps, nodeIndex, nodeCount)
	var <- 0
	for (i in 1:nreps) {
		# Simulate once
		degVec <- PAMsim(nodeCount)

		#(U-EU)^2
		var <- var + (degVec[nodeIndex] - Ev) ^ 2
	}

	# Var(U) = E[(U-EU)^2]
	var <- var / nreps

	return(var)
}

# Covariance
CoVar <- function(nreps, nodeU_Index, nodeV_Index, nodeCount) {
	# Get E(U)
	EU <- EXP(nreps, nodeU_Index, nodeCount)
	# Get E(V)
	EV <- EXP(nreps, nodeV_Index, nodeCount)
	
	co_var <- 0
	
	for(i in 1:nreps) {
		degVec <- PAMsim(nodeCount)

		# (U-EU)(V-EV)
		co_var <- co_var + ((degVec[nodeU_Index] - EU) * (degVec[nodeV_Index] - EV))
	}

	co_var_U <- 0
	co_var_V <- 0

	for(i in 1:nreps) {
		degVec <- PAMsim(nodeCount)

		co_var_U <- co_var_U + (degVec[nodeU_Index] - EU)
	}

	for(i in 1:nreps) {
		degVec <- PAMsim(nodeCount)

		co_var_V <- co_var_V + (degVec[nodeV_Index] - EV)
	}

	co_var_U <- co_var_U / nreps
	co_var_V <- co_var_V / nreps

	co_var <- co_var_U - co_var_V
	# Cov(U,V)=E[(U-EU)(V-EV)]
	#co_var <- co_var / nreps

	return(co_var)
}

# Begin Simulate
nreps <- 10000
print(EXP(nreps,1,4))
print(Var(nreps,1,4))
print(CoVar(nreps,1,2,4))