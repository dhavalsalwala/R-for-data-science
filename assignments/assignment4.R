#setting seed
set.seed(1000)

#creating ids, module and result
ids <- rep(as.character(1001:1005,2))
module <- c(rep("CT101",5),rep("CT102",5))
result <- c(rnorm(n = 5,mean = 70,sd = 5),
            rnorm(n = 5,mean = 50,sd = 8))

#creating data frame
dataf<-data.frame(ids,module,result)
dataf$result[1]=NA

#function to get data_ids for given group id
get_data<-function(field,df,group_id,data_id)
{
  df[df[group_id]==field, data_id, drop=F][[1]]
}

#function to apply function f on data_id group by group_id 
my_aggregate <- function(df, group_id, data_id, f, ...)
{
  
  #Error Handling
 if(!(exists('df') & is.data.frame(get('df'))))
 {
   stop ("First parameter is not a data frame object.")
 }
  else if(!(group_id %in% colnames(df)))
  {
    stop ("Error ",group_id," is not a valid column.")
  }
  else if(!(data_id %in% colnames(df)))
  {
    stop ("Error ",data_id," is not a valid column.")
  }
  else if(!(is.numeric(df[data_id][[1]])))
  {
    stop ("Error ",data_id," is not a numeric column")
  }
  else if(class(f)!="function")
  {
    stop ("Error ",f," is not a function")
  }
  else
  {
    #finding all unique group_id fields    
    group_by_fields<-unique(df[group_id])
    
    #calling apply on each group_id to get data from data_id field
    data_matrix<-apply(group_by_fields,1,get_data,df,group_id,data_id)
    
    #caling lapply on a number vector (1 to no. of data_matrix colmun). 
    #Fetching each matrix column as a part of the list.
    data_list<- lapply(c(1:ncol(data_matrix)), function(i) data_matrix[,i])
    
    #applying names to the list
    names(data_list)<-group_by_fields[[1]]
    
    #calling sapply on each element of a list and apply function f on it.
    ans<- sapply(data_list,f,...)
    
    #displaying result
    ans
  }
}

my_aggregate(dataf, "module", "result", mean)

my_aggregate(dataf, "module", "result", mean, na.rm=T)

my_aggregate(dataf, "ids", "result", mean)

my_aggregate(dataf, "ids", "result", mean,na.rm=T)

my_aggregate(1:10, "module", "result", mean, na.rm=T)

my_aggregate(dataf, "modul", "result", mean)

my_aggregate(dataf, "module", "ids", mean)

my_aggregate(dataf, "module", "result", 10)