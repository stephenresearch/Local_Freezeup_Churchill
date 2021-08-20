library(tidyverse)
library(lubridate)
#library(reshape)
library(weathercan)

# Palette -----------------------------------------------------------------
#just for fun palette
library(wesanderson)
pal <- wes_palette(name = "Zissou1", type = "discrete")
pal2 <- c(pal[1], pal[3])

scale_color_manual(values=wes_palette(n=3, name="Zissou1"))
scale_fill_manual(values=wes_palette(n=3, name="Zissou1"))


# Polar bear data ---------------------------------------------------------
PB <- read.csv("PByearice_2019.csv", header=T)
PB <- mutate(PB, year=Year, LastRelease=ymd(LastRelease), EstIceIn= ymd(EstIceIn), IceOrd=yday(EstIceIn))

ggplot (PB, aes(x=year, y=IceOrd)) + geom_point()

# Hydro data --------------------------------------------------------------
hydro <- read.csv("hydro Readhead Daily__May-12-2021_02_16_04AM.csv", header=T)
hydro <- mutate(hydro, Date=ymd(Date), month=month(Date, label=T),
                year=year(Date))

flow <- hydro %>%
  filter(month %in% c("Oct", "Nov"), PARAM=="m3/s", year >="1983")%>%
  group_by(year, month)%>%
  summarise(meanFlow=mean(Value))

ggplot(flow, aes(x=year,y=meanFlow, col=month))+ geom_point()+
  scale_color_manual(values=wes_palette("Zissou1", type="discrete"))

# Weather data ------------------------------------------------------------
##load weather data from 1983 to 2020; can skip down
stations_search("Churchill", interval = "day")

##Station 48969 can provide Oct/Nov 2010 to 2019
##overlaped by 44244
#C48969 <-weather_dl(station_id = 48969, start = "1983-01-01", end = "2020-07-15", interval = "day")
#C48969 <- C48969 %>%
#  select(station_id, date, year, month, max_temp, mean_temp, min_temp,
#         dir_max_gust, spd_max_gust)%>%
#  mutate(date = ymd(date), month=month(date, label=T), year=year(date))%>%
#  filter(month %in% c("Oct", "Nov"))

#Station 3871 can provide Oct/Nov from 1983 to 2007
C3871 <-weather_dl(station_id = 3871, start = "1983-01-01", end = "2020-07-15", interval = "day")
C3871 <- C3871 %>%
  select(station_id, date, year, month, max_temp, mean_temp, min_temp,
         dir_max_gust, spd_max_gust)%>%
  mutate(date = ymd(date), month=month(date, label=T))%>%
  filter(month %in% c("Oct", "Nov"), year <="2004")

##Station 50148 can provide Oct/Nov from 2018 to 2020
##overlapped by station 44244
#C50148 <-weather_dl(station_id = 50148, start = "1983-01-01", end = "2020-12-15", interval = "day")
#C50148 <- C50148 %>%
#  select(station_id, date, year, month, max_temp, mean_temp, min_temp,
#         dir_max_gust, spd_max_gust)%>%
#  mutate(date = ymd(date), month=month(date, label=T), year=year(date))%>%
#  filter(month %in% c("Oct", "Nov"))


#Station 44244 can provide Oct/Nov 2012-21
C44244 <-weather_dl(station_id = 44244, start = "2005-01-01", end = "2021-01-01", interval = "day")
C44244 <- C44244 %>%
  select(station_id, date, year, month, max_temp, mean_temp, min_temp,
         dir_max_gust, spd_max_gust)%>%
  mutate(date = ymd(date), month=month(date, label=T))%>%
  filter(month %in% c("Oct", "Nov"))

AllWeather <- rbind(C3871,C44244)
write.csv(AllWeather,"AllWeatherChurchill.csv")

Weather <- AllWeather %>%
  group_by(year, month)%>%
  summarize(avg_temp = mean(mean_temp))

ggplot(Weather, aes(x=year, y=avg_temp, col=month))+ geom_point()+
  scale_color_manual(values=wes_palette("Zissou1", type="discrete"))

# Merging data sources ----------------------------------------------------
Environment <- merge(flow, Weather, by=c("year", "month"))
Total <- merge(Environment, PB, by="year")
write.csv(Total,"LocalFreeze.csv")

