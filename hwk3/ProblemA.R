makePmf <- function(supp, probs) {
  if(sum(probs) != 1) {
  	stop()
  }
  
  for(p in probs) {
  	if(p < 0)
  	  stop()
  }

  if(length(supp) != length(probs)) {
  	stop()
  }
  
  pmf <- probs
  names(pmf) <- as.character(supp)
  mean <- supp %*% probs
  var <- (supp-c(mean))^2%*%probs
#  var <- sum((supp-mean)^2*probs)
  structure(pmf, meanx = mean, variancex = var, class = "pmf")
}

'+.pmf' <- function(p1, p2) {
  p3 = c()

  for(i in 1:length(pmf1)) {
	  for(j in 1:length(pmf2)) {
		  supp = as.numeric(names(pmf1)[i]) + as.numeric(names(pmf2)[j])
		  supp_index <- 0
		  supp_index <- which(names(p3) == as.character(supp))
		  if(length(supp_index) == 0) {
		  	p3 <- c(p3, pmf1[i]*pmf2[j])
		  	names(p3)[length(p3)] = supp
		  } else {
	 	 	  p3[supp_index] = p3[supp_index] + pmf1[i]*pmf2[j]
	 	  }
	  }
  }
  p3_supp <- names(p3)

  return(makePmf(as.numeric(p3_supp), p3))
}

#pmf1 <- makePmf(1:3,c(0.25,0.25,0.50))
#pmf1
#pmf2 <- makePmf(c(2,4),c(0.6,0.4))
#pmf3 <- pmf1 + pmf2
#pmf3

