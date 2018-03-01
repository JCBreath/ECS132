ducd <-function(x,c) {(3/(c^2-1))* x^2};
pucd <- function(q,c) {(q^3 /(c^2-1))*(c-1)};
qucd <- function(q,c) {(q*(c^2-1))^{1/3}};
rucd <- function(n,c) {
	tmp <- runif(n);
	qucd(tmp,c);
};


ducd(0.5,2);
pucd(0.5,2);
qucd(0.5,2);
rucd(100,2);