#setting seed to 10
set.seed(10)

#no of students
N=10

#generating scores for students in 5 subjects.
cs1 <- rnorm(N,72,10)
cs2 <- rnorm(N,65,7)
cs3 <- rnorm(N,80,9)
cs4 <- rnorm(N,55,7)
cs5 <- rnorm(N,61,5)

#line break
cat ("\n")

#creating matrix for student data
m<-matrix(c(cs1,cs2,cs3,cs4,cs5),ncol=5,nrow=10)

#assigning column and row names
colnames(m)<-c("cs1","cs2","cs3","cs4","cs5")
rownames(m)<-1:nrow(m)

#displaying matrix
m

#line break
cat ("\n")

#This function search for value m in vector z
#if found returns named value of z[m]
get_named_value<-function(m,z)
{
	strtoi(names(z)[z==m])
}

#function to do the following:
#1) sort column vector in a decreasing order
#2) assigning names to the column vector
#3) call sapply on each vector value to find its rank
find_rank<-function(m)
{
  #sorting column vector in descending order
  x<-sort(m, decreasing=T)

  #assigning names(1 to length) to the column vector 
  names(x)<-1:length(x)

  #calling sapply on each element of a column vector m to find its rank
  sapply(m,get_named_value,z=x);
}

#calling apply on each column of a matrix m
ans<-apply(m,2,find_rank)

#assigning column names
rownames(ans)<-c("Student#1","Student#2","Student#3","Student#4","Student#5","Student#6","Student#7","Student#8","Student#9","Student#10")

#displaying result
ans

#line break
cat ("\n")

#function to find the median of a vector
find_median <- function(m)
{
	#if a length is even
	if(length(m)%%2==0)
	{
		m[(length(m)/2)+1]
	}
	#if a length is odd
	else
	{
		m[(length(m)+1)/2]
	}
}

#calling function to find median row wise
medians<-apply(ans,1,median)

#binding median column with the resultant matrix
ans2<-cbind(ans,medians)

#assigning name to the median column
colnames(ans2)[ncol(ans2)]<-"median"

#displaying final result
ans2
