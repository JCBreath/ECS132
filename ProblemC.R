# PAMsim (ProblemC.R)
# Siyuan Yao

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

# Test
# PAMsim(4)