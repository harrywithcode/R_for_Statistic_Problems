#Question c
t<-(9.02-10)*sqrt(20)/2.22
t

#Question d
F(tobs)<-pt(-abs(t),df=19)
P-value<-1- F(tobs)

#Question e
count<-0
for (i in 1:10000){
  onetime=rnorm(n=20,mean=9.02,sd=2.22)
  onemean=mean(onetime)
  if (onemean>10){
    count=count+1
  }
}
percent<-count/10000
pvalue<-1-percent