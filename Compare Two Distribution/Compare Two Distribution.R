#Question a
data=read.csv("F:\\6313 statistic for DS\\project\\4\\bp.csv",header=T,sep = ",")
armsys=data$armsys
fingsys=data$fingsys

boxplot(armsys,fingsys,names=c('arm method','finger method'))

#Question b
hist(armsys, main ='arm method')
hist(fingsys, main ='finger method')
#QQplots:
qqnorm(armsys,main='arm method')
qqline(armsys)
qqnorm(fingsys,main='finger method')
qqline(fingsys)

#Question c
mean(armsys)
mean(fingsys)
n<-200
m<-200
alpha<-0.05                                      
mean(armsys)-mean(fingsys)+c(-1,1)* qnorm(1-alpha/2)*sqrt(sd(armsys) ^2/200+sd(fingsys) ^2/200)

#Question d
D<-0
T<-(mean(armsys)-mean(fingsys)-D)/sqrt(var(armsys)/n+var(fingsys)/m)
Pvalue<-2*(1-pt(abs(T), 199))