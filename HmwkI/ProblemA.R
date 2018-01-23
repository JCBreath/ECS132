#Problem A

nreps <- 100000

ProA_a_count <- 0
ProA_b_count <- 0 
CofSame <- 0
CofThree <- 0


for (rep in 1:nreps){
	ta <- sample(3:5,1,prob = c(0.5,0.25,0.25))
	tb <- sample(3:5,1,prob = c(0.5,0.25,0.25))
	tc <- sample(3:5,1,prob = c(0.5,0.25,0.25))
	#repeat tests, record the time that each car takes

	test <- c(ta,tb,tc)
	if (min(test) == 4){
		#if the min of the three is 4, then the first arrival is at 4
		ProA_a_count <- ProA_a_count + 1
		#increment counter for this case
	}
	
	
	
	if ((ta + tb + tc) == 10) {
		#if totally take 10 mins
		ProA_b_count <- ProA_b_count + 1
		#increment counter for this case
	}

	if ((ta == tb) && (ta == tc )){
		#if they are the same
		CofSame <- CofSame + 1
		#we count the number of this case

		#the following count number of (they are all 3 given they are all the same)
		if (ta == 3) {
			CofThree <- CofThree +1
		}
	}
	
	


}
	#probability of the first pass at 4
	P_a <- (ProA_a_count / nreps )

	#probability of total is 10
	P_b <- (ProA_b_count / nreps )
	
	#the probability of they are all three given they are the same
	P_c <- (CofThree/CofSame)


	print(P_a)
	print(P_b)
	print(P_c)
