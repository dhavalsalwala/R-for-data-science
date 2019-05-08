#1
library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

#2
my_flights <- flights
my_flights

#3 first page
my_flights<-my_flights %>% filter(!is.na(dep_delay),!is.na(arr_delay)) %>% select(time_hour,	origin,	dest,	carrier,	dep_delay,	arr_delay,	air_time,	distance)
my_flights

#3 second page
my_flights <- mutate(my_flights, DayOfWeek=wday(time_hour,label=T), HourOfDay=hour(time_hour), Month=month(time_hour,label = T,abbr = T))
my_flights
select(my_flights,time_hour,DayOfWeek,HourOfDay,everything())

#4
my_flights %>% group_by(HourOfDay) %>% summarise(AvrDepDelay=mean(dep_delay),SD=sd(dep_delay), MinDelay=min(dep_delay), MaxDelay=max(dep_delay), 
                                                 MaxDelayHours=MaxDelay/60) %>% arrange(desc(AvrDepDelay))

#5
my_flights %>% group_by(Month) %>% summarise(AvrDepDelay=mean(dep_delay),SD=sd(dep_delay), MinDelay=min(dep_delay), MaxDelay=max(dep_delay), 
                                                 MaxDelayHours=MaxDelay/60) %>% arrange(desc(AvrDepDelay))
#6
my_flights %>% group_by(carrier) %>% summarise(AvrDepDelay=mean(dep_delay),SD=sd(dep_delay), MinDelay=min(dep_delay), MaxDelay=max(dep_delay), 
                                             MaxDelayHours=MaxDelay/60, NObs=n()) %>% arrange(desc(AvrDepDelay))
#7
my_flights %>% group_by(origin,Month) %>% summarise(AvrDepDelay=mean(dep_delay),SD=sd(dep_delay), MinDelay=min(dep_delay), MaxDelay=max(dep_delay), 
                                               MaxDelayHours=MaxDelay/60, NObs=n()) %>% arrange(Month)

#8
my_flights %>% group_by(HourOfDay,origin) %>% summarise(AvrDepDelay=mean(dep_delay),SD=sd(dep_delay), MinDelay=min(dep_delay), MaxDelay=max(dep_delay), 
                                                    MaxDelayHours=MaxDelay/60, NObs=n()) %>% arrange(HourOfDay)

#9
my_flights<-my_flights %>% mutate(DaySection = case_when(
                                               HourOfDay>=5 & HourOfDay<12 ~ 'Morning',
                                               HourOfDay>=12 & HourOfDay<18 ~ 'Afternoon',
                                               HourOfDay>=18 ~ 'Evening'))
select(my_flights,DaySection,everything())

#10
set.seed(99)
myf_sample<-my_flights %>% sample_n(10000) %>% filter(dep_delay<=180) 
myf_sample

#11
ggplot(data=myf_sample) + geom_boxplot(aes(x=Month,y=dep_delay, color=DaySection))+ylab("Departure Delay")

