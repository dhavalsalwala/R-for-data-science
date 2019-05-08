#1 loading all libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

#set working directory
setwd("/home/dsalwala")

#2 loading energy dataset
ener <- readxl::read_excel("IrelandData January 2017.xlsx")
ener

#3 using tidyr function separate to fetch Date/Time and HourOfDay/MinuteOfDay
ener<-ener %>% separate(DateTime,into=c("Date","Time"),sep=" ",remove=F) %>%
  separate(Time,into=c("HourOfDay","MinuteOfDay"),extra="drop",sep=":",remove=F) %>%
  mutate(DayOfWeek=wday(DateTime,label=T)) %>% mutate(HourOfDay = as.integer(HourOfDay)) %>% 
  mutate(MinuteOfDay = as.integer(MinuteOfDay)) %>% mutate(NIFlow=ifelse(NetImports<0,"Exporting","Importing")) %>%
  select(DateTime,Date,Time,HourOfDay,MinuteOfDay,DayOfWeek,NIFlow,everything())
ener

#changing NIFlow labels to exports/imports
ener$NIFlow<-ifelse(ener$NIFlow=="Exporting","Exports","Imports")

#4
ggplot(data=ener) + geom_point(aes(x=HourOfDay,y=NetImports,color=NIFlow))+xlab("Time(Hour of Day)")+ylab("Net Imports")+
  facet_wrap(~DayOfWeek)+ggtitle("Time v Net Imports By Day of Week")

#5
ggplot(data=ener) + geom_point(aes(x=Wind,y=CO2,color=NIFlow))+xlab("Wind Generation")+ylab("CO2 Emissions")+
  ggtitle("Wind Generation v CO2 Emissions")

#6
weather <- read_excel("/home/dsalwala/Mac Head Wind Data.xlsx")
weather

#7
weather<-weather %>% separate(Date,into = c("Date"), sep=" ", extra="drop") %>% mutate(Date=as.Date(Date, "%Y-%m-%d"))
weather

#8
ggplot(weather, aes(x=Date,y=AVRWind))+geom_point(color="Blue")+geom_line(linetype="dashed")+
  ylab("Average Wind Speed (Knots)")

#9
avr_daily_wind<-ener %>% mutate(Date=as.Date(Date, "%Y-%m-%d")) %>% group_by(Date) %>% summarise(AverageWindGeneration=mean(Wind)) %>%
  arrange(Date)
avr_daily_wind

#10
ggplot(data=inner_join(avr_daily_wind,weather))+geom_point(aes(x=AVRWind,y=AverageWindGeneration))+
  xlab("Average Wind Speed (Mace Head)")+ylab("Average Wind Generation")+
  ggtitle("Wind Speed v Wind Power Generated")

#11
ggplot(data=inner_join(avr_daily_wind,weather),aes(x=AVRWind,y=AverageWindGeneration))+geom_point()+
  xlab("Average Wind Speed (Mace Head)")+ylab("Average Wind Generation")+
  ggtitle("Wind Speed v Wind Power Generated, with linear model")+geom_smooth(method='lm')

#12
ggplot(data=inner_join(avr_daily_wind,weather),aes(x=AVRWind,y=AverageWindGeneration))+geom_point()+
  xlab("Average Wind Speed (Mace Head)")+ylab("Average Wind Generation")+
  ggtitle("Wind Speed v Wind Power Generated, with loess model")+geom_smooth(method='loess')
