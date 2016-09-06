
  cpu <- scan(file="F:\\6313 statistic for DS\\project\\2\\cpu.txt", what ="numeric")
cpu<-as.numeric(cpu)

mnf <- function(N,u1,d1,u2,d2){
  result <- 0.7*dnorm(N, mean=u1, sd=d1, log=FALSE)+0.3*dnorm(N, mean=u2, sd=d2, log=FALSE)
  return(result)
}

#neg.loglik.fun is Negative of log-likelihood function assuming mixture normal distribution.
neg.loglik.fun <- function(par,N)
{
  
  result <- sum(log(mnf(N = N, u1=par[1], d1= d1<-par[2], u2 =par[3], d2=par[4]),10))
  return(-result)
}

# Minimize -log (L), i.e., maximize log (L)
ml.est <- optim(par=c(100,2,30,2), fn=neg.loglik.fun, method = "L-BFGS-B", lower=rep(0,2),
                hessian=TRUE, N=cpu)

ml.est$par

#find standard errors
sqrt(diag(solve(ml.est$hessian)))


  hist(cpu, freq=FALSE, xlab="cpu time", ylab="relative frequency", 
       main="CPU time distribution") 

curvefun <- function(N,u1=ml.est$par[1],d1=ml.est$par[2],u2=ml.est$par[3],d2=ml.est$par[4]){
  return(mnf(N,u1,d1,u2,d2))
}

x<-cpu
#hist(x, probability = TRUE, xlab="run time", main = "histogram of run times")
curve(curvefun(x),add = T)

