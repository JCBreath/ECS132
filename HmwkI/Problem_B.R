#P(C2>= 1 | Total = 2)
#P(Total == 2 and C2 >= 1) / P(Total == 2)
ALOHA <- function(p,q,nreps){
	total2<-0
	c2greater0giventotal2<-0
	total <- 0
	for (i in 1:nreps){
		total <- 0
		c1 <- 0
		c2 <- 0
		x <- c(1,1)
		for (j in 1:2){	#in epoch 1
			if (runif(1) < p){ #1 is active, 0 is inactive
				c1 <- c1 +1
				x[j] <- 0
			} 
		}
		if ((x[1]+ x[2]) == 0){	#if both node are inactive at the end, change both to active
			x[1] <- 1
			x[2] <- 1
		}
		for (j in 1:2){		#in epoch 2
			if(x[j] == 0){		#if x[j] is inactive
				if (runif(1) < q){	#if regenerate
					x[j] <- 1
					if(runif(1) < p){	#if send successfully
						c2 <- c2+1
						x[j] <- 0
					} 
				} 
			} else if (x[j] == 1){	#if x[j] is active
				if (runif(1) < p){	#if send successfully
					c2 <- c2+1
					x[j] <- 0
				}
			}
		}
		total <- c1+c2  		#get total sended number
		if(total == 2){			#P(Total == 2)
			total2 <- total2 +1
			if (c2 >= 1){		#P(Total == 2 and C2 >= 1)
				c2greater0giventotal2 <- c2greater0giventotal2+1
			}
		}
	}
	return (c2greater0giventotal2/total2)	#P(Total == 2 and C2 >= 1) / P(Total == 2)
}

p <- 0.6
q <- 0.2
print(ALOHA(p,q,10000))