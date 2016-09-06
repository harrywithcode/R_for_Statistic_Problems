mydata = read.table("/medicine.txt")
brandname = mydata$V2[2:21]
generic = mydata$V3[2:21]
brandname<-matrix(brandname,  nrow = 1, ncol = 20 )
generic<-matrix(generic, nrow=1, ncol=20)
xmat<-rbind(brandname,generic)
xmat<-apply(xmat,1,as.numeric)
chisq.test(xmat)
