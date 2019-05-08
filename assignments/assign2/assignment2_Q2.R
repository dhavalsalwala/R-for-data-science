my_table<-function(v)
{
	#check if input vector has atleast one NA value
	if(sum(is.na(v))>0)
	{
		print ("Error in my_table(v) : Error, input vector must not have NA value")
	}
	#check if input vector has atleast one non-character value
	else if(is.numeric(v))
	{
		print ("Error in my_table(v) : Error, input must be a character vector")
	}
	else
	{
		#calculating frequency of each charcter in input vector

		#result vector
		ans<-c()

		#extracting unique elements from the input vector
		u<-sort(unique(v))
		
		#Looping over unique vector to find freqency of each element in input vector
		for (i in u)
		{
			ans<-c(ans,sum(v==i))
		}

		#assinging names
		names(ans)<-u

		#displaying result
		ans
	}
}

#setting seed to 111
set.seed(111)

#Given input vector
v <- sample(c('M','F'),10,prob = c(.3,.7),replace = T)

#appending NA value to input vector v
v <- c(v,NA)

#calling function my_table
my_table(v)
