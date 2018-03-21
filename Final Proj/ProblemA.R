# 95% confidence interval for a list of data
CI_95 <- function(data) {
	wbar <- mean(data)
	s <- sqrt(mean(data^2) - wbar^2)
	n <- length(data)
	radius <- 1.96*s/sqrt(n)
	return(c(wbar-radius, wbar+radius))
}

CI_DIFF <- function(data1, data2) {
	xbar <- mean(data1)
	ybar <- mean(data2)
	s1 <- sqrt(mean(data1^2) - xbar^2)
	s2 <- sqrt(mean(data2^2) - ybar^2)
	n1 <- length(data1)
	n2 <- length(data2)
	radius <- 1.96*sqrt(s1^2/n1 + s2^2/n2)
	return(c(xbar-ybar-radius, xbar-ybar+radius))
}

Create_Vec <- function(length, value) {
	x <- vector(length = length)

	for(i in 1:length)
		x[i] <- value

	return(x)
}

url <- 'http://heather.cs.ucdavis.edu/~matloff/132/Hwk/'

E132_W17 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/132/W17.txt')
E132_W16 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/132/W16.txt')
E132_W15 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/132/W15.txt')
E132_F13 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/132/F13.txt')

E145_F14 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/145/F14.txt')
E145_W17 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/145/W17.txt')

E158_F13 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/158/F13.txt')
E158_F17 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/158/F17.txt')
E158_S16 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/158/S16.txt')
E158_W15 <- read.table('http://heather.cs.ucdavis.edu/~matloff/132/Hwk/ProblemAData/158/W15.txt')


E132 <- c(unlist(E132_W17['V2']), unlist(E132_W16['V2']), unlist(E132_W15['V2']), unlist(E132_F13['V2']))
E145 <- c(unlist(E145_F14['V2']), unlist(E145_W17['V2']))
E158 <- c(unlist(E158_F13['V2']), unlist(E158_F17['V2']), unlist(E158_S16['V2']), unlist(E158_W15['V2']))
print("95% confidence intervals for the population mean quiz average for each of the three courses: ")
CI_95(E132)
CI_95(E145)
CI_95(E158)
print("95% confidence interval for the difference in population mean quiz averages for ECS 132 and 145: ")
CI_DIFF(E132, E145)

LCSI_E145 <- c(unlist(subset(E145_F14, V1 == 'LCSI')['V2']), unlist(subset(E145_W17, V1 == 'LCSI')['V2']))
ECSE_E145 <- c(unlist(subset(E145_F14, V1 == 'ECSE')['V2']), unlist(subset(E145_W17, V1 == 'ECSE')['V2']))
print("95% confidence interval for the difference in population mean quiz averages in ECS 145 between the two majors: ")
CI_DIFF(LCSI_E145, ECSE_E145)
#length(c(E132,E145,E158))
course_vec <- c(Create_Vec(length(E132), 0), Create_Vec(length(E145), 1), Create_Vec(length(E158), 2))
year_vec <- c(Create_Vec(length(unlist(E132_W17['V2'])), 2017.00), 
			Create_Vec(length(unlist(E132_W16['V2'])), 2016.00), 
			Create_Vec(length(unlist(E132_W15['V2'])), 2015.00),
			Create_Vec(length(unlist(E132_F13['V2'])), 2013.75),
			Create_Vec(length(unlist(E145_F14['V2'])), 2014.75),
			Create_Vec(length(unlist(E145_W17['V2'])), 2017.00),
			Create_Vec(length(unlist(E158_F13['V2'])), 2013.75),
			Create_Vec(length(unlist(E158_F17['V2'])), 2017.75),
			Create_Vec(length(unlist(E158_S16['V2'])), 2016.25),
			Create_Vec(length(unlist(E158_W15['V2'])), 2015.00))

major_vec <- c(unlist(E132_W17['V1']), 
			unlist(E132_W16['V1']), 
			unlist(E132_W15['V1']), 
			unlist(E132_F13['V1']),
			unlist(E145_F14['V1']), 
			unlist(E145_W17['V1']), 
			unlist(E158_F13['V1']), 
			unlist(E158_F17['V1']), 
			unlist(E158_S16['V1']), 
			unlist(E158_W15['V1']))

major_vec[major_vec == 2] <- 0

#print(major_vec)

dat <- data.frame(Score = c(E132, E145, E158),
                  Course = course_vec,
                  Year = year_vec,
                  Major = major_vec)

#print(dat)

plot(dat)

plot(lm(Score ~ Course + Year + Major, data = dat))
print("Course: 0:ECS 132 1:ECS 145 2:ECS 158")
print("Major: 0:CS 1:CSE")
lm(Score ~ Course + Year + Major, data = dat)

CS <- c(length(unlist(subset(subset(dat, Year == 2017.75), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2017), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2016.25), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2016), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2015), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2014.75), Major == 0)['Score'])),
	length(unlist(subset(subset(dat, Year == 2013.75), Major == 0)['Score'])))
CSE <- c(length(unlist(subset(subset(dat, Year == 2017.75), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2017), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2016.25), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2016), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2015), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2014.75), Major == 1)['Score'])),
	length(unlist(subset(subset(dat, Year == 2013.75), Major == 1)['Score'])))

year <- c(2017.75,2017,2016.25,2016,2015,2014.75,2013.75)
names(CS) = year
print(CS)
names(CSE) = year
print(CSE)

prop <- CS / (CS + CSE)
names(prop) = year
print(prop)

lm(prop ~ year)