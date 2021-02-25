# Elma Snow Data

library(rgdal)
library(sf)
library(scales)

# Anomalies


d=as.Date("2018-07-01")

season_start=as.Date("2019-11-01")
season_stop=as.Date("2020-4-01")

if(F){
stations=ghcnd_stations()

stations%>%filter(
  grepl("ELMA CENTER",name),
  grepl("NY",state),
  element=="SNOW",
  last_year>=2017,
)

stations%>%filter(
  grepl("BUFFALO",name),
  grepl("NY",state),
  element=="SNOW",
  last_year>=2017,
)

}


data=meteo_tidy_ghcnd(#"US1NYER0075", #elma
                      "USW00014733", #Buffalo
                      var=c("SNOW","SNWD"),
                      date_min = as.Date("1900-01-01")
                      )

data_2=data%>%
  mutate(
  snow=ifelse(is.na(snow),0,snow*.039),
  snwd=ifelse(is.na(snwd),0,snwd*.039),
  day=day(date),
  month=month(date),
  year=year(date),
  doy=yday(date),
  winter=ifelse(month<6,year-1,year),
  sday=date-ymd(paste(winter,11,1)),
  dyear=year+(month/12),
  fyear=ymd(paste(ifelse(month<6,year,year-1),month,day,sep="-")),
  sdate=ymd(paste(ifelse(month<6,2020,2019),month,day,sep="-")),
  season=ifelse(between(sdate,season_start,season_stop),T,F),
  decade=floor(winter/10)*10,
  era=cut(winter,breaks = c(1900,2000,2020)))%>%
  group_by(winter)

firstday<-
  data_2%>%
  filter(snwd>2)%>%
  summarize(first=first(date,order_by = date)) %>%
  mutate(month=month(first))

data_winter<- data_2 %>%
  group_by(winter)%>%
  summarize(
    days_total=n(),
    days_snow=sum(snwd>0,na.rm=T),
    days_depth5cm=sum(snwd>=5,na.rm=T),
    days_depth10cm=sum(snwd>=10,na.rm=T),
    days_snow5cm=sum(snow>=5,na.rm=T),
    days_snow15cm=sum(snow>=15,na.rm=T),
)
data_winter



data_2 %>%
  filter(winter>2000) %>%
  ggplot(aes(x=sdate))+
  facet_wrap(~winter,nrow=3)+
  xlim(season_start,season_stop)+
  geom_area(aes(y=snwd),fill=grey(0.6))+
  geom_line(aes(y=snow),col="red")+
  ylab("Snowfall (red) & Snow Depth (grey) (cm)")+
  ggtitle("Snowfall and Snowdepth 2001-2020 at Elma Meadows, NY",
          "Red indcates snowfall, grey indicates snow depth")

data_2 %>%
  filter(between(sdate,season_start,season_stop)) %>%
  ggplot(aes(x=sdate,y=year,fill=snwd))+
  geom_tile()+
  geom_point(data=filter(data_2,snow>0),aes(size=snow))+
  scale_fill_viridis_c(name="Snow Fall")+
  ylab("Year")


### Cumulative Snowfall
sum_snow <- data_2 %>%
  group_by(winter) %>%
  filter(between(sdate,season_start,season_stop)) %>%
  arrange(dyear) %>%
  mutate(snowfall=cumsum(snow),
         maxsnow=max(snowfall))

sum_snow %>%
  ggplot(aes(x=sdate,y=snowfall,color=winter,
             group=as.factor(winter)))+
  geom_line()+
  geom_line(data=filter(sum_snow,winter==2020),col="black",size=2)+
  geom_text(aes(label=winter,x=ymd("20200410"),y=maxsnow),size=4)+
  scale_color_viridis_c(name="Year")+
    scale_x_date(name = "Date",
               breaks='1 month',
               labels = date_format("%b"))+
    geom_smooth(aes(group=1),col="red")+
  ylab("Cumulative Snowfall (cm)")


### Mean Snowdepth
mean_snowd <- data_2 %>%
  filter(between(sdate,season_start,season_stop)) %>%
  group_by(winter) %>%
  arrange(dyear) %>%
  mutate(snowfall=rollmean(snwd,k = 5,fill = NA))

mean_snowd %>%
  ggplot(aes(x=sdate,y=snowfall,color=winter,
             group=as.factor(winter)))+
  geom_line()+
  geom_line(data=filter(mean_snowd,winter==2020),
            col="black",size=2)+
  scale_color_viridis_c(name="Year")+
  geom_smooth(aes(color=decade,group=decade),span=.1,col="red")+
  ylab("Mean Snow Depth (cm)")

mean_snowd %>%
  ggplot(aes(x=sdate,y=year,fill=snwd))+
  geom_raster()+
  scale_fill_viridis_c(name="Snow Depth")+
  ylab("Year")

### Skiable days
###
ski_depth=4

data_2 %>%
  group_by(winter, era) %>%
  arrange(dyear) %>%
  mutate(ski_day=snwd>ski_depth,
         ski_day_smooth=rollmean(ski_day,k = 3,fill = NA)) %>%
  group_by(fyear, era) %>%
  summarize(ski_day=mean(ski_day_smooth)) %>%
  ggplot(aes(x=fyear,y=ski_day*100,color=era, group=era))+
  geom_point()+
  geom_smooth(span=.5)+
  ylab("Proportion Skiable Days (%)")+
  scale_x_date(name = "Date",
               limits = ymd(c("20171015","20180501")),
               breaks='1 month',
               labels = date_format("%b"))+
  ggtitle("Proportion Skiable Days in Elma, NY",
          subtitle = paste("% Days with Snowdepth >",
                           ski_depth,"cm over ",
                           paste(range(data_2$year),
                                 collapse="-")))


