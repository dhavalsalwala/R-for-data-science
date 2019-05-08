#Dhaval Salwala
#18230845

#The Class Constructor (given)
myvec <- function(v){
  if(!is.numeric(v))
    stop("Error, data type must be numeric.")
  structure(list(data=v),class="myvec")
}

`<=.myvec` <- function(x,n)
{
  x$data<=n
}

`>=.myvec` <- function(x,n)
{
  x$data>=n
}

`==.myvec` <- function(x,n)
{
  x$data==n
}

#1 implementation of myvec version of print
print.myvec<-function(x)
{
  cat ("s3 class = ",class(x),"\n")
  cat ("Number of elements = ",length(x$data),"\n")
  cat (x$data,"\n")
}

#creating integer vector and invoking print.myvec version
x<-myvec(1:10)
x

#2 extending base operator '['
`[.myvec` <- function(x, i)
{
  x$data[i]
}
x[1:2]

#3 extending base operator '<-' for assigning value to a vector
`[<-.myvec` <- function(x, i,j, ..., value)
{
  x$data[i]<-value
  x
}
x[1:2]<-0

#4 extending base operator '<'
`<.myvec` <- function(x,n)
{
  x$data<n
}
x[x<1]

#5 extending base operator '>'
`>.myvec` <- function(x,n)
{
  x$data>n
}
x[x>1]

#6 extending base operator '!='
`!=.myvec` <- function(x,n)
{
  x$data!=n
}
x[x!=0]

#7 assigning NA value to x[1]
x[1]<-NA

#8 implementation of myvec version of sum
sum.myvec<-function(x, ..., na.rm = FALSE) 
{
  sum(x$data,na.rm=na.rm)
}
sum(x)
sum(x,na.rm = T)

#9 implementation of myvec version of mean
mean.myvec<-function(x, ..., na.rm = FALSE) 
{
  mean(x$data,na.rm=na.rm)
}
mean(x)
mean(x,na.rm=T)