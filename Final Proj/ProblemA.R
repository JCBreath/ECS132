# 95% confidence interval for a list of data
CI_95 <- function(data) {
	wbar <- mean(data)
	s <- sqrt(mean(data^2) - wbar^2)
	n <- length(data)
	radius <- 1.96*s/sqrt(n)
	return(c(wbar-radius, wbar+radius))
}

E132_W17 <- read.table('ProblemAData/132/W17.txt')
E132_W16 <- read.table('ProblemAData/132/W16.txt')
E132_W15 <- read.table('ProblemAData/132/W15.txt')
E132_F13 <- read.table('ProblemAData/132/F13.txt')

E145_F14 <- read.table('ProblemAData/145/F14.txt')
E145_W17 <- read.table('ProblemAData/145/W17.txt')

E158_F13 <- read.table('ProblemAData/158/F13.txt')
E158_F17 <- read.table('ProblemAData/158/F17.txt')
E158_S16 <- read.table('ProblemAData/158/S16.txt')
E158_W15 <- read.table('ProblemAData/158/W15.txt')

E132 <- c(unlist(E132_W17['V2']), unlist(E132_W16['V2']), unlist(E132_W15['V2']), unlist(E132_F13['V2']))
E145 <- c(unlist(E145_F14['V2']), unlist(E145_W17['V2']))
E158 <- c(unlist(E158_F13['V2']), unlist(E158_F17['V2']), unlist(E158_S16['V2']), unlist(E158_W15['V2']))

CI_95(E132)
CI_95(E145)
CI_95(E158)