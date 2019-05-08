library(readxl)
library(ggplot2)


# http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets

orig_list <- data.frame(readxl::read_excel("datasets/Titanic/titanic3_assignment.xls"))
plist     <- orig_list

# Preprocess the data
# (1) Convert survived to logical value
plist$survived                  <- as.logical(plist$survived)

# (2) Change class to string
plist$pclass                    <- ifelse(plist$pclass==1,"First",ifelse(plist$pclass==2,"Second","Third"))

# (3) Simple imputation of age (mean of all ages)
age_na_index                    <- is.na(plist$age)
plist[age_na_index,"age"]       <- mean(plist$age,na.rm=T)

# (4) Simple imputation of fare (mean of all fares)
fare_na_index                   <- is.na(plist$fare)
plist[fare_na_index,"fare"]     <- mean(plist$fare,na.rm=T)

# (5) Simple imputation of place of embarking (randomly generated) with seed of 99
set.seed(99)
emb_na_index                    <- is.na(plist$embarked)
plist[emb_na_index,"embarked"]  <- sample(c("C","S","Q"),sum(emb_na_index),replace = T)

# (6) Create new category (age cohort)
plist$age_cohort                <- ifelse(plist$age<16,"Child",ifelse(plist$age<60,"Adult","Elderly"))


# (7) Put in full town origin (Queenstown (Q) replaced by Cobh)
plist$embarked                  <- ifelse(plist$embarked=="S","Southampton",
                                      ifelse(plist$embarked=="C","Cherbourg",
                                           ifelse(plist$embarked=="Q","Cobh",plist$embarked)))


# Check the outputs
dim(plist)

table(plist$survived)
table(plist$survived,plist$age_cohort)
table(plist$survived,plist$sex)
table(plist$survived,plist$pclass)
table(plist$survived,plist$embarked)



# (1) Plot the counts of survival by different category
ggplot(data=plist)+geom_bar(aes(x=survived,fill=pclass))+theme(legend.position = "top")+xlab("Survived")+ylab("Number")+
  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Travel Class")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex))+theme(legend.position = "top")+xlab("Survived")+ylab("Number")+
  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Gender")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort))+theme(legend.position = "top")+xlab("Survived")+ylab("Number")+
  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Age Cohort")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=embarked))+theme(legend.position = "top")+xlab("Survived")+ylab("Number")+
  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Embarkation Location")

# (2) Plot the proportions of survival by different category
ggplot(data=plist)+geom_bar(aes(x=survived,fill=pclass),position="fill")+theme(legend.position = "top")+xlab("Survived")+
  ylab("Proportion")+  theme(legend.title=element_blank())+ggtitle("Survival Proportions by Class")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex),position="fill")+theme(legend.position = "top")+xlab("Survived")+
  ylab("Proportion")+  theme(legend.title=element_blank())+ggtitle("Survival Proportions by Gender")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort),position="fill")+theme(legend.position = "top")+xlab("Survived")+
  ylab("Proportion")+  theme(legend.title=element_blank())+ggtitle("Survival Proportions by Age Cohort")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=embarked),position="fill")+theme(legend.position = "top")+xlab("Survived")+
  ylab("Proportion")+  theme(legend.title=element_blank())+ggtitle("Survival Proportions by place of Embarkation")

# (3) Plot facet wraps
ggplot(data=plist)+geom_bar(aes(x=survived,fill=age_cohort))+facet_wrap(~pclass)+theme(legend.position = "top")+xlab("Survived")+
  ylab("Number")+  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Cohort and Travel Class")
ggplot(data=plist)+geom_bar(aes(x=survived,fill=sex))+facet_wrap(~pclass)+theme(legend.position = "top")+xlab("Survived")+
  ylab("Number")+  theme(legend.title=element_blank())+ggtitle("Survival Numbers by Gender and Travel Class")

# (4) Plot continuous variables
ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=embarked))+theme(legend.position = "top")+  
  theme(legend.title=element_blank())+xlab("Age")+ylab("Fare")+ggtitle("Age v Fare by Place of Embarkation")

ggplot(data=plist)+geom_point(aes(x=age,y=fare))+geom_smooth(aes(x=age,y=fare),method = "lm")+theme(legend.position = "top")+
  theme(legend.title=element_blank())+xlab("Age")+ylab("Fare")+ggtitle("Age v Fare with Linear Model")

ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=survived))+theme(legend.position = "top")+
 xlab("Age")+ylab("Fare")+ theme(legend.title=element_blank())+ggtitle("Age v Fare with with Survival Info")

ggplot(data=plist)+geom_point(aes(x=age,y=fare,colour=embarked))+facet_wrap(~pclass)+theme(legend.position = "top")+
  theme(legend.title=element_blank())+xlab("Age")+ylab("Fare")+ggtitle("Age v Fare By Travel Class and Point of Departure")

