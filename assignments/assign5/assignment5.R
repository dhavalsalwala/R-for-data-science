#importing library
library(readxl)
library(ggplot2)

#import data set and display summary
orig_list<-data.frame(readxl::read_excel("/home/dsalwala/NUIG/Programming for Data Analytics/assign/5titanic3_assignment.xls"))
plist<-orig_list
dim(plist)
summary(plist)

#Q1
plist$survived<-ifelse(plist$survived==1,T,F)
summary(plist)

#Q2
plist$pclass<-as.vector(plist$pclass,"character")
summary(plist)

#Q3
plist[!complete.cases(plist$age),'age']<-mean(plist$age,na.rm = T)
summary(plist)

#Q4
plist[!complete.cases(plist$fare),'fare']<-mean(plist$fare,na.rm = T)
summary(plist)

#Q5
set.seed(99)
#fetching all incomplete changes
incomplete_cases<-plist[!complete.cases(plist$embarked),'embarked']
#finding no. of incompelete records
size<-length(incomplete_cases)
#applying changes
plist[!complete.cases(plist$embarked),'embarked']<-sample(c("C","S","Q"),size)
summary(plist)
unique(plist$embarked)

#Q6
plist$age_cohort<-ifelse(plist$age<16,"Child",ifelse(plist$age<60,"Adults","Elderly"))
summary(plist)

#Q7
plist$embarked<-ifelse(plist$embarked=="S","Southampton",ifelse(plist$embarked=="Q","Cobh","Cherbourg"))
summary(plist)
unique(plist$embarked)

#Q8
#Verifying all changes
head(plist)
dim(plist)
table(plist$survived,plist$sex)
table(plist$survived)
table(plist$survived,plist$pclass)
table(plist$survived,plist$age_cohort)
table(plist$survived,plist$embarked)



#Plot1
#Survival Numbers by Travel Class
ggplot(data=plist)+geom_bar(aes(x=survived, fill=pclass))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Travel Class")+
  guides(fill=guide_legend(title=NULL))+ scale_fill_discrete(labels=c("First", "Second", "Third"))

#Plot2
#Survival Numbers by Gender
ggplot(data=plist)+geom_bar(aes(x=survived, fill=sex))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Gender")+
  guides(fill=guide_legend(title=NULL))

#Plot3
#Survival Numbers by Age Cohort
ggplot(data=plist)+geom_bar(aes(x=survived, fill=age_cohort))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Age Cohort")+
  guides(fill=guide_legend(title=NULL))

#Plot4
#Survival Numbers by Embarkation Location
ggplot(data=plist)+geom_bar(aes(x=survived, fill=embarked))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Embarkation Location")+
  guides(fill=guide_legend(title=NULL))

#Plot5
#Survival Proportions by Embarkation Location
ggplot(data=plist)+geom_bar(aes(x=survived, fill=pclass), position = "fill")+ylab("Proportion")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Proportions by Embarkation Location")+
  guides(fill=guide_legend(title=NULL))+ scale_fill_discrete(labels=c("First", "Second", "Third"))

#Plot6
#Survival Proportions by Gender
ggplot(data=plist)+geom_bar(aes(x=survived, fill=sex), position = "fill")+ylab("Proportion")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Proportions by Gender")+
  guides(fill=guide_legend(title=NULL))

#Plot7
#Survival Proportions by Age Cohort
ggplot(data=plist)+geom_bar(aes(x=survived, fill=age_cohort), position = "fill")+ylab("Proportion")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Proportions by Age Cohort")+
  guides(fill=guide_legend(title=NULL))

#Plot8
#Survival Proportions by place of Embarkation
ggplot(data=plist)+geom_bar(aes(x=survived, fill=embarked), position = "fill")+ylab("Proportion")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Proportions by place of Embarkation")+
  guides(fill=guide_legend(title=NULL))

#Plot9
#Survival Numbers by Cohort and Travel Class
#creating copy of dataset
plist2<-plist
#converting pclass(,2,3,) to pclass("First","Second","Third")
plist2$pclass<-ifelse(plist2$pclass==1,"First",ifelse(plist2$pclass==2,"Second","Third"))
#applying changes
ggplot(data=plist2)+geom_bar(aes(x=survived, fill=age_cohort))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Cohort and Travel Class")+
  guides(fill=guide_legend(title=NULL))+facet_grid(.~pclass, labeller = labeller(pclass = as_labeller(hospital_names)))

#plot10, using plist2 as defined earlier.
#Survival Numbers by Gender and Travel Class
ggplot(data=plist2)+geom_bar(aes(x=survived, fill=sex))+ylab("Number")+xlab("Survived")+
  theme(legend.position="top")+ggtitle("Survival Numbers by Gender and Travel Class")+
  guides(fill=guide_legend(title=NULL))+facet_grid(.~pclass)

#Plot11
#Age v Fare by Place of Embarkation
ggplot(data=plist)+geom_point(aes(x=age,y=fare,color=embarked))+ylab("Fare")+xlab("Age")+
  theme(legend.position="top")+ggtitle("Age v Fare by Place of Embarkation")+
  theme(legend.title=element_blank())
                              
#Plot12
#Age v Fare with Linear Model
ggplot(data=plist, aes(x=age,y=fare))+ylab("Fare")+xlab("Age")+
  ggtitle("Age v Fare with Linear Model")+geom_point()+geom_smooth(method='lm')

#Plot13
#Age v Fare with Survival Info
ggplot(data=plist)+geom_point(aes(x=age,y=fare,color=survived))+ylab("Fare")+xlab("Age")+
  theme(legend.position="top")+ggtitle("Age v Fare with Survival Info")+
  theme(legend.title=element_blank())

#Plot14, using plist2 defined earlier
#Age v Fare By Travel Class and Point of Departure
ggplot(data=plist2)+geom_point(aes(x=age,y=fare,color=embarked))+ylab("Fare")+xlab("Age")+
  theme(legend.position="top")+ggtitle("Age v Fare By Travel Class and Point of Departure")+
  theme(legend.title=element_blank())+facet_grid(.~pclass)
