#density
ducd <-function(x,c) {( 3/(2*(c^1.5-1)) ) *x^0.5};
#cdf
pucd <- function(q,c) {(q^1.5 /(c^1.5-1))-1/(c^1.5-1)};
#quantile function
qucd <- function(q,c) {((c^1.5-1)*q+1)^{2/3}};
#random number generator
rucd <- function(n,c) {
	tmp <- runif(n);
	qucd(tmp,c);
};

#print(mean(rucd(1000000,5)));
