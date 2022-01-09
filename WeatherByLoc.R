library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(gt)

skiweather<-function() {
#datafrome of montana ski hills
MTHills<-data.frame(area=c("Great Divide","Blacktail","Whitefish","Snowbowl","Lookout Pass",
                           "Lost Trail","Turner","Discovery","Showdown","Bridger","Big Sky","Red Lodge",
                           "Bearpaw","Teton Pass"),
                    lat=c(46.753073,48.0166166,48.480590,47.014436,47.4555,45.69333056,48.6049508,
                          46.2563144,46.836496654,45.8171533,45.2807618,45.18749925,48.17333264,
                          47.9298),
                    long=c(-112.313478,-114.3731828,-114.350323,-113.999568,-115.69499722,
                           -113.94832954,-115.6304512,-113.2420029,-110.705330512,-110.8966091,
                           -111.406627,-109.350498598,-109.633830798,-112.8110),
                    elev=c(1812.563,1979,1361.740,1515.965,1436,2500,1175,2169,2500,1863,2365,
                           2448,1455,2195))

#rounding lat long of montan ski hills to 4 digits to work with weather.gov
MTHills<-MTHills %>% mutate(loc=paste0(round(lat,4),",",round(long,4)))


#function to get weather forecast from weather.gov based on lat and long
getweather<- function(loc) {
  url<-paste0('https://api.weather.gov/points/',loc)
  
  res<-GET(url)
  
  data<-fromJSON(rawToChar(res$content))
  
  res1<-GET(data$properties$forecast)
  
  data1<-fromJSON(rawToChar(res1$content))
  
  data2<-plyr::ldply(data1$properties$periods$name,data.frame)
  
  data2<-data1$properties$periods
  
  data2<-full_join(data2 %>% mutate(startTime=lubridate::ymd_hms(startTime),
                                    endTime=lubridate::ymd_hms(endTime),loc=loc),
                   MTHills,by="loc")
  
  return(data2)
}


df<-MTHills$loc[1:9] %>% purrr::map(function(x) {getweather(x)})
df1<-MTHills$loc[10:14] %>% purrr::map(function(x) {getweather(x)})

df2<-plyr::ldply(df,data.frame)
df3<-plyr::ldply(df1,data.frame)

df4<-bind_rows(df2,df3) %>% select(-elev,loc)

return(df4)
}

df4<-skiweather()

MT<-map_data("county","Montana")  

ggplot() + geom_polygon(data=MT, aes(long, lat, group = group),fill = "sienna3", colour = "black") + 
  coord_quickmap() +
  geom_point(data=df4%>% filter(number==1),aes(long,lat),color='snow2') +
  geom_text(data=df4%>% filter(number==1),aes(long,lat,label=area),nudge_y=0.1,color='snow2') +
  geom_text(data=df4%>% filter(number==1),
            aes(long,lat,label=paste0(temperature,temperatureUnit," Wind ",windSpeed,'\n',shortForecast)),
            nudge_y=-0.2,color='snow2')+
  ggtitle("NOAA Temp Forecast for Ski Hills") +
  labs(subtitle=Sys.Date()-1)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="grey27"),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_rect(fill="grey27"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(colour = "transparent", fill = "transparent"),
        legend.justification = c(1,.5), legend.position = c(.9,.6),
        legend.text=element_text(color="snow2",size=15,face='bold'),
        legend.title = element_text(color="snow2",size=15,face='bold'),
        plot.title = element_text(color="snow2",face="bold",size=25,hjust=.5,vjust=.8),
        plot.subtitle = element_text(color="snow2",hjust=.5))

ggsave('/Users/jimauer/R/Snow/plot2.png',plot=plot2,scale=1.75)


#datafrome of montana ski hills
MTHills<-data.frame(area=c("Great Divide","Blacktail","Whitefish","Snowbowl","Lookout Pass",
                           "Lost Trail","Turner","Discovery","Showdown","Bridger","Big Sky","Red Lodge",
                           "Bearpaw","Teton Pass"),
                    lat=c(46.753073,48.0166166,48.480590,47.014436,47.4555,45.69333056,48.6049508,
                          46.2563144,46.836496654,45.8171533,45.2807618,45.18749925,48.17333264,
                          47.9298),
                    long=c(-112.313478,-114.3731828,-114.350323,-113.999568,-115.69499722,
                           -113.94832954,-115.6304512,-113.2420029,-110.705330512,-110.8966091,
                           -111.406627,-109.350498598,-109.633830798,-112.8110),
                    elev=c(1812.563,1979,1361.740,1515.965,1436,2500,1175,2169,2500,1863,2365,
                           2448,1455,2195))

#rounding lat long of montan ski hills to 4 digits to work with weather.gov
MTHills<-MTHills %>% mutate(loc=paste0(round(lat,4),",",round(long,4)))


df4 %>% filter(number<7,isDaytime=="TRUE") %>% 
  select(area,name,temperature,temperatureUnit,windSpeed,detailedForecast) %>% 
  gt(rowname_col="name",
     groupname_col = "area") %>% 
  cols_label(
    name="",
    temperature="Temp",
    temperatureUnit= "",
    windSpeed="Wind Speed",
    detailedForecast = "Forecast"
  ) %>% 
  cols_align(
    align ="center",
    columns = detailedForecast
  ) %>% 
  cols_width(name ~ px(110))

