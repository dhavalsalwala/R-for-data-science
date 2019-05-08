#setting seed to 99
set.seed(99)

#outcome of rolling first dice 1000 times (v1)
v1<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#outcome of rolling second dice 1000 times (v2)
v2<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#sum of outcome v1 and v2
v<-v1+v2

#counting no. of outcomes that ODD and EVEN
ans<-c(sum(v%%2!=0),sum(v%%2==0))

#assigning names to outcome
names(ans)<-c("Number Odd","Number Even")

#displaying result
ans
