set.seed(1000)
ids    <- rep(as.character(1001:1005,2))
module <- c(rep("CT101",5),rep("CT102",5))
result <- c(rnorm(n = 5,mean = 70,sd = 5),
            rnorm(n = 5,mean = 50,sd = 8))
result[1] <- NA

dataf <- data.frame(ids, module, result,stringsAsFactors = F)

my_aggregate <- function(df, group_id, data_id, f, ...){
  # check for valid data frame
  # check for valid grouping column
  # check for valid data column
  # check for valid function
  if(class(df) != "data.frame")
    stop("First parameter is not a data frame object")
  
  if(!(group_id %in% colnames(dataf)))
    stop(paste("Error",group_id,"is not a valid column"))
  
  if(!(data_id %in% colnames(dataf)))
    stop(paste("Error",data_id,"is not a valid column"))
  
  if(!is.numeric(df[,data_id]))
    stop(paste("Error",data_id,"is not a numeric column"))
  
  if(!is.function(f))
    stop(paste("Error",f,"is not a function"))
  else{
    
  }
  
  ids <- unique(df[,group_id])
  
 
    
  sapply(ids,function(x){
    subset <- df[df[,group_id] == x,]
    f(subset[,data_id], ...)
  })
}

my_aggregate(dataf, "module", "result", mean)




