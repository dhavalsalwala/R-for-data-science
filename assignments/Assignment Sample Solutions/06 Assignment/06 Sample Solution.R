# include the relevant libraries
library(nycflights13)
library(dplyr)
library(ggplot2)
library(lubridate)

# create a local copy
my_flights <- flights

# Filter out missing values for dep and arr delays
my_flights <- my_flights %>%
              filter(!is.na(dep_delay),!is.na(arr_delay)) %>%
              select(time_hour,origin,dest,carrier,dep_delay,arr_delay,air_time,distance)

# Add columns for the day of the week and for the hour of the day
my_flights <- my_flights %>%
              mutate(Month     = month(time_hour,label = T,abbr = T),
                     DayOfWeek = wday(time_hour,label = T),
                     HourOfDay = hour(time_hour))


# Average departure delays by hour of day
delay_hourly <- my_flights %>%
  group_by(HourOfDay) %>% 
  summarise(AvrDepDelay=mean(dep_delay),
            SD= sd(dep_delay),
            MinDelay=min(dep_delay),
            MaxDelay=max(dep_delay),
            MaxDelayHours=MaxDelay/60) %>%
  arrange(desc(AvrDepDelay))

# Average departure delays by day of week
delay_weekday <- my_flights %>%
                 group_by(DayOfWeek) %>% 
                 summarise(AvrDepDelay=mean(dep_delay),
                           SD= sd(dep_delay),
                           MinDelay=min(dep_delay),
                           MaxDelay=max(dep_delay),
                           MaxDelayHours=MaxDelay/60) %>%
                 arrange(desc(AvrDepDelay))

# Average departure delays by month
delay_monthly <- my_flights %>%
  group_by(Month) %>% 
  summarise(AvrDepDelay=mean(dep_delay),
            SD= sd(dep_delay),
            MinDelay=min(dep_delay),
            MaxDelay=max(dep_delay),
            MaxDelayHours=MaxDelay/60) %>%
  arrange(desc(AvrDepDelay))

# Average delays by carrier
delay_carrier <- my_flights %>%
  group_by(carrier) %>% 
  summarise(AvrDepDelay=mean(dep_delay),
            SD= sd(dep_delay),
            MinDelay=min(dep_delay),
            MaxDelay=max(dep_delay),
            MaxDelayHours=MaxDelay/60,
            NObs=n()) %>%
  arrange(desc(AvrDepDelay)) 

# Average delays by airport by month
delay_airport_month <- my_flights %>%
  group_by(origin,Month) %>% 
  summarise(AvrDepDelay=mean(dep_delay),
            SD= sd(dep_delay),
            MinDelay=min(dep_delay),
            MaxDelay=max(dep_delay),
            MaxDelayHours=MaxDelay/60,
            NObs=n()) %>%
  arrange(Month) 

# Average delays by airport time of day
delay_airport_time <- my_flights %>%
  group_by(HourOfDay,origin) %>% 
  summarise(AvrDepDelay=mean(dep_delay),
            SD= sd(dep_delay),
            MinDelay=min(dep_delay),
            MaxDelay=max(dep_delay),
            MaxDelayHours=MaxDelay/60,
            NObs=n()) %>%
  arrange(HourOfDay) 

# Create a sample data set for exploration
my_flights <- my_flights %>%
              mutate(DaySection=case_when(
                HourOfDay >= 5 &  HourOfDay < 12 ~ "Morning",
                HourOfDay >= 12 &  HourOfDay < 18 ~ "Afternoon",
                HourOfDay >= 18  ~ "Evening",
                TRUE ~ "Undefined"
              ))
set.seed(99)
myf_sample <- sample_n(my_flights,10000) %>%
                filter(dep_delay <= 180) 

# Plot the boxplots by month and day section
ggplot(data=myf_sample,aes(x=Month,y=dep_delay,colour=DaySection)) + 
  geom_boxplot()+ylab("Departure Delay")+xlab("Month")



