#setting seed to 99
set.seed(99)

#outcome of rolling first dice 1000 times (v1)
v1<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#outcome of rolling second dice 1000 times (v2)
v2<-sample(1:6,1000,prob=c(1/6,1/6,1/6,1/6,1/6,1/6),replace=T)

#sum of outcome v1 and v2
v<-v1+v2

#creating vector of unique elements from v
u<-sort(unique(v))

#calculating frequency of unique elemnts in v
ans1<-c()
for(i in u)
{
	ans1<-c(ans1,sum(v==i))
}

#assigning names to outcome
names(ans1)<-u

#displaying result
ans1
