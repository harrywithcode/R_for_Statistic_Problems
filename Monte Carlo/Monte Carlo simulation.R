# to get the maximum of Xi. i = 1, 2, 3.
x=max(rexp(3,0.2))

# use Monte Carlo simulation to do this operation 10000 times
z<-replicate(10000, max(rexp(3,0.2))

# draw histogram of 10000 times of maximum of exponential distribution, which Lambda=0.2, n=3.
hist(z,ylim=c(0.0001,0.1),prob=TRUE)
          
#use curve function to draw the density
curve(0.6*(1-exp(-0.2*x))^2*exp(-0.2*x),add=TRUE)