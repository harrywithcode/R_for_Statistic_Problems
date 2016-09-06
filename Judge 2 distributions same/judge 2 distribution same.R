#Question a:
  children<-c(40.3,55,45.7,43.3,50.3,45.9,53.5,43,44.2,44,47.4,44,33.6,55.1,48.8,50.4,37.8,60.3,46.5)
adult<-c(20,30.2,2.2,7.5,4.4,22.2,16.6,14.5,21.4,3.3,6.6,7.8,10.6,16.2,14.5,4.1,15.8,4.1,2.4,3.5,8.5,10,1,4.4,1.3,8.1,4.7,18.4)
qqnorm(children, main = "Children  Q-Q Plot")
qqline(children)
qqnorm(adult, main = "Adult  Q-Q Plot")
qqline(adult)

#Question b:
  n<-length(children)
m<-length(adult)
F<-var(children)/var(adult)
F1<-qf(0.025,n-1,m-1)
F2<-qf(0.975,n-1,m-1)
F*1/F1
F*1/F2

#Question c:
  sp=sqrt(((n-1)*var(children)+(m-1)*var(adult))/(n+m-2))
mean(children)-mean(adult)+c(1,-1)*qt((0.95),45)*sp*sqrt(1/n+1/m)
