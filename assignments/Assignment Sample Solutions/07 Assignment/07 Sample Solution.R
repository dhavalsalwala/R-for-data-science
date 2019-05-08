library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)

ener <- read_excel("datasets/Energy/IrelandData January 2017.xlsx")

ener <- mutate(ener, HourOfDay   =  hour(DateTime),
               MinuteOfDay =  minute(DateTime),
               DayOfWeek   =  wday(DateTime,label=T),
               NIFlow=case_when(
                 NetImports >= 0 ~ "Imports",
                 NetImports < 0  ~ "Exports"
               )) %>%
               separate(DateTime,c("Date","Time"),sep=" ",remove=F)

ggplot(data = ener) +
  geom_point(mapping = aes(x=HourOfDay,y=NetImports,colour=NIFlow)) +
  xlab("Time (Hour of Day)") + ylab("Net Imports") + 
  ggtitle("Time v Net Imports By Day of Week")+facet_wrap(~DayOfWeek)

ggplot(data = ener,mapping = aes(x=Wind,y=CO2,colour=NIFlow)) +
  geom_point() +
  xlab("Wind Generation") + ylab("CO2 Emissions") + 
  ggtitle("Wind Generation v CO2 Emissions")



weather <- read_excel("datasets/Energy/Mac Head Wind Data.xlsx")

weather <- mutate(weather,Date = ymd(Date))

ggplot(data = weather,mapping = aes(x=Date,y=AVRWind)) +
  geom_line(linetype="dashed") + geom_point(colour="blue") +
  xlab("Date") + ylab("Average Wind Speed (Knots")

avr_daily_wind <- ener %>% group_by(Date) %>% 
  summarise(AverageWindGeneration=mean(Wind)) %>%
  mutate(Date=ymd(Date))

gen_weather <- left_join(avr_daily_wind,weather) %>% 
  select(Date,AVRWind,AverageWindGeneration)

ggplot(data = gen_weather,mapping = aes(x=AVRWind,y=AverageWindGeneration)) +
  geom_point() +
  xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + 
  ggtitle("Wind Speed v Wind Power Generated")

ggplot(data = gen_weather,mapping = aes(x=AVRWind,y=AverageWindGeneration)) +
  geom_point() +
  geom_smooth(method = "lm",se=T)+
  xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + 
  ggtitle("Wind Speed v Wind Power Generated, with linear model")

ggplot(data = gen_weather,mapping = aes(x=AVRWind,y=AverageWindGeneration)) +
  geom_point() +
  geom_smooth()+
  xlab("Average Wind Speed (Mace Head)") + ylab("Average Wind Generation") + 
  ggtitle("Wind Speed v Wind Power Generated, with loess model")






