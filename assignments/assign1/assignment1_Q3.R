#setting seed to 99
set.seed(99)

#outcome of rolling first dice 1000 times (v1)
v1<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#outcome of rolling second dice 1000 times (v2)
v2<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#sum of outcome v1 and v2
v<-v1+v2

#initializing temporary vector (tmp) with 1 to 1000
tmp<-1:1000

#extracting result from (v) using boolean vector
v[(tmp%%100==0 | tmp==1)]

