#Question a
alpha=0.05
maymean<-2887
janmean<-2635
maysd<-412
jansd<-365
m<-500
n<-400
v<-( maysd^2 /m+jansd^2/n) ^2/(maysd^4/(m^2*(m-1))+jansd^4/(n^2*(n-1)))
maymean-janmean+c(1,-1)*qt(alpha/2,v)*sqrt(maysd^2/m+jansd^2/n)

#Question
D<-0
T<-( maymean - janmean-D)/sqrt(maysd ^2/m+ jansd ^2/n)
v<-( maysd^2 /m+jansd^2/n) ^2/(maysd^4/(m^2*(m-1))+jansd^4/(n^2*(n-1)))
Pvalue<-2*(1-pt(abs(T),v))
