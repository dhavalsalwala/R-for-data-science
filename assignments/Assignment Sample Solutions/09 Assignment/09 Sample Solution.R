# Constructor
myvec <- function(v){
  if(!is.numeric(v))
    stop("Error, data type must be numeric.")
  structure(list(data=v),class="myvec")
}

# Print function
print.myvec <- function(s){
  cat("s3 class = ",class(s),"\n")
  cat("Number of elements = ", length(s$data), "\n")
  cat(s$data,"\n")
}

# Filter operation
`[.myvec` <- function(s,q){
  s$data[q]
}

# Modification operation
`[<-.myvec` <- function(s,q,value){
  s$data[q]<-value
  s
}

# Logical operators
`<.myvec` <- function(s,q){
  s$data < q
}

`>.myvec` <- function(s,q){
  s$data > q
}

`<=.myvec` <- function(s,q){
  s$data <= q
}

`>=.myvec` <- function(s,q){
  s$data >= q
}

`!=.myvec` <- function(s,q){
  s$data != q
}

`==.myvec` <- function(s,q){
  s$data == q
}

# Summary functions
mean.myvec <- function(s,...){
  mean(s$data,...)
}

sum.myvec <- function(s,...){
  sum(s$data,...)
}


# Testing the class
x<-myvec(1:10)

x

x[1:2]

x[1:2] <- 0

x[x<1]

x[x>1]

x[x!=0]

x[1]<-NA

sum(x)

sum(x,na.rm = T)


