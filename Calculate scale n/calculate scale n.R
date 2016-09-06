conf.int <- function(p,n,alpha) {
  x <- rbinom(1, n, p)
  phat<-x/n
  ci <- phat + c(-1, 1) * qnorm(1 - (alpha/2))*sqrt((phat*(1-phat))/n)
  return(ci)
}
p <- 0.05
n <- 5
alpha <- 0.15
nsim <- 10000
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.1
n <- 5
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.25
n <- 5
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.5
n <- 5
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.9
n <- 5
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.95
n <- 5
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.05
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.1
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.25
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.5
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.9
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.95
n <- 10
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.05
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.01
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.25
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.5
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.9
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.95
n <- 30
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.05
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.1
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.25
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.5
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.9
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.95
n <- 50
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.05
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.1
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.25
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.5
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.9
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )
p <- 0.95
n <- 100
ci.mat <- replicate(nsim, conf.int(p,n, alpha))
mean( (p >= ci.mat[1,])*(p <= ci.mat[2,]) )

p5<-c(0.2086,0.4052,0.6625,0.6176,0.3995,0.206)
p10<-c(0.3933,0.6313,0.6814,0.8922,0.6383,0.3917)
p30<-c(0.7811,0.7868,0.8506,0.8023,0.7894,0.7616)
p50<-c(0.6819,0.8291,0.8538,0.7988,0.8293,0.6879)
p100<-c(0.8453, 0.8507,0.8389,0.8551,0.844,0.8511)
p<-c(0.05, 0.1, 0.25, 0.5, 0.9, 0.95)
plot(p5~p, xlab="p(n=5)", ylab="Proportion of times the interval is correct")
lines(lowess(p,p5), col="blue")
plot(p10~p, xlab="p(n=10)", ylab="Proportion of times the interval is correct")
lines(lowess(p,p10), col="blue")
plot(p30~p, xlab="p(n=30)", ylab="Proportion of times the interval is correct")
lines(lowess(p,p30), col="blue")
plot(p50~p, xlab="p(n=50)", ylab="Proportion of times the interval is correct")
lines(lowess(p,p50), col="blue")
plot(p100~p, xlab="p(n=100)", ylab="Proportion of times the interval is correct")
lines(lowess(p,p100), col="blue")

