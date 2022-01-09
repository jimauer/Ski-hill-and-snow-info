library(snotelr)
library(tidyverse)
library(lubridate)
library(rgdal)

#snotel site data
meta_data <- snotel_info()

#filtering for montana sites 
site<-meta_data %>% filter(state=='MT') %>% select(site_id)

#setting montana sites as values
set<-site$site_id[1:92]

#scraping snotel sites in Montana
df <- snotel_download(site_id = set, internal = TRUE)

#cleaning up dates and converting to farenheit 
df1<-df %>% mutate(date=ymd(date),max_temp=(temperature_max*(9/5)+32),
                   min_temp=(temperature_min*(9/5)+32),
                   mean_temp=(temperature_mean*(9/5)+32)) %>% 
  select(-temperature_max,-temperature_min,-temperature_mean)

#table of precip in last week
df1 %>% filter(date>Sys.Date()-7,precipitation!='NA') %>%
  group_by(site_name,county) %>% 
  summarise(precipitation=sum(precipitation),max_temp=max(max_temp)) %>% 
  arrange(desc(precipitation))

#setting mat data for motnana counties 
MT<-map_data("county","Montana")  

#Mapping snotel sites with Snow Water Equiv, max temp and ski sites for previous day
#plot1<-
df1 %>% filter(date==max(date)-1) %>% 
  arrange(desc(snow_water_equivalent)) %>% 
  select(site_name,latitude,longitude,elev,county,snow_water_equivalent,
         max_temp,precipitation) %>%
  rename(`Max Temp`=max_temp,`Snow Water Equiv`=snow_water_equivalent) %>% 
  ggplot() +
  geom_polygon(data=MT, aes(long, lat, group = group),fill = "sienna3", colour = "black") + 
  coord_quickmap() +
  geom_point(aes(y=latitude,x=longitude,size=`Snow Water Equiv`,color=`Max Temp`))+
  scale_size_continuous(range = c(3, 12))+
  geom_point(data=MTHills,aes(y=lat,x=long),color="snow2")+
  geom_text(data=MTHills,aes(y=lat,x=long,label=area),nudge_y=-0.1,color='snow2')+
  ggtitle("Montana Snotel Site Data") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="black"),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_rect(fill="black"),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background = element_rect(colour = "transparent", fill = "transparent"),
        legend.justification = c(1,.5), legend.position = c(.95,.72),
        legend.text=element_text(color="snow2",size=15,face='bold'),
        legend.title = element_text(color="snow2",size=15,face='bold'),
        plot.title = element_text(color="snow2",face="bold",size=25,hjust=.5,vjust=.8))

ggsave('/Users/jimauer/R/Snow/plot1.png',plot=plot1,scale=1.75)

#building dataframe of montana ski hill locations
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


df1 %>% filter(date==Sys.Date()-1) %>% 
  arrange(desc(snow_water_equivalent)) %>% 
  select(site_name,latitude,longitude,elev,county,snow_water_equivalent,
         temperature_max,precipitation) %>% 
  mutate(temperature_max=(temperature_max*(9/5))+32) %>% 
  ggplot() +
  geom_polygon(data=MT, aes(long, lat, group = group),fill = "white", colour = "grey50") + 
  coord_quickmap() +
  geom_point(data=MTHills,aes(y=lat,x=long),color="red")
