library(lubridate)
library(readxl)
library(dplyr)
library(readr)
library(ggplot2)


# --------------- #
# import data
# --------------- #
# import known cod tags
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")

# check against detections
ids = c("A69-1601-60951","A69-1602-58719","A69-1602-58721","A69-1602-58735",
        "A69-1602-58746","A69-1602-58751","A69-1602-58753","A69-1602-58754",
        "A69-1602-58755","A69-1602-58800","A69-1602-58809","A69-1602-58810",
        "A69-1602-63876","A69-1604-63165","A69-9001-58738","A69-9001-59363") 
ids=as.data.frame(ids)
names(ids) = "TagID"

# check
ids = inner_join(ids, FreyTags, by = c("TagID"="Tag ID"))

# Open Dave's files
detections <- read_excel("Downloads/dkaragon_analysis_detections.xlsx", 
                         col_names = FALSE)
names(detections) = c("date","time","x","tag_id1","tag_id2","TagID","VMT")
detections$Species = "Unknown"
detections = detections %>% 
  mutate(Species = ifelse(TagID %in% ids$TagID,"Cod",Species),
         date = as.Date(date))
detections$date_time = as.POSIXct(paste(detections$date, sapply(strsplit(as.character(detections$time)," "),tail,1), sep=" "), tz="UTC")

# download from ERDDAP
# VMT and Glider time are in UTC, beware of daylight savings 
ru34_1 = read_csv("Downloads/ru34-20240112T1722-trajectory-raw-delayed_3ed1_2c6c_022b.csv",skip=1)
ru34_2 = read_csv("Downloads/ru34-20240301T1336-trajectory-raw-delayed_93d7_f71c_3191.csv",skip=1)
names(ru34_1) = c("date_time","latitude","longitude","depth")
names(ru34_2) = c("date_time","latitude","longitude","depth")
ru34 = rbind(ru34_1, ru34_2)
ru34 = unique(ru34) # remove dups
ru34$date_time <- as.POSIXct(ru34$date_time,tz="UTC") # define as UTC
#ru34$date_time = as.POSIXct(format(ru34$date_time,tz="EST")) # change to EST

# interpolate depths
#library(zoo)
#ru34$int_depth <- na.locf(ru34$depth) #not interp, just grabs closest in front
#ru342 = ru34 %>% filter(!is.na(depth))#mutate(dt = round_date(date_time, unit = '1 min')) 

# ru34_avg= ru34 %>% group_by(dt) %>% 
#   summarise(latitude=mean(latitude, na.rm=TRUE),
#             longitude=mean(longitude, na.rm=TRUE),
#             depth=mean(depth, na.rm=TRUE))

## made table
#dsum = group_by(detections,TagID) %>% 
#  summarize(n=n(),species=unique(Species),
#            dates=toString(unique(date)))

# import sunrise time for color paneling
ss <- read_excel("Downloads/RI_sunrise_sunset.xlsx")
ss$sunrise_time = as.POSIXct(paste(ss$Date, sapply(strsplit(as.character(ss$Sunrise)," "),tail,1), sep=" "))
ss$sunset_time = as.POSIXct(paste(ss$Date, sapply(strsplit(as.character(ss$Sunset)," "),tail,1), sep=" "))
# --------------- #

# ------------------------- #
# add locations
# ------------------------- #
setDT(ru34) 
setkey(ru34, date_time)
setDT(detections) 
setkey(detections, date_time)
combo <- ru34[detections, roll = "nearest" ]
# ------------------------- #

# # --------------- #
# # add depth to detection
# # --------------- #
# #detections = left_join(detections, ru34, by="date_time")
# detections = detections %>% 
#   mutate(dt = round_date(date_time, unit = '1 min')) %>% 
#   left_join(ru34_avg, by="dt")
# detections$date_time = as.POSIXct(format(detections$date_time, tz="EST")) # change to EST

# plots

# Tag v Date
p1 = ggplot()+
  geom_point(data = detections, aes(x=date,TagID,col=TagID,shape=Species),size=5)+
  theme_bw() +
  xlab("Date") + ylab("Tag ID")+
  guides(color="none") + 
  theme(legend.position="bottom") + 
  scale_x_date(date_labels = "%b %d")
p1
ggsave("REV_tag_date.png",p1)

# Tag v Time
x = filter(detections, TagID %in% "A69-1602-58721")#A69-1602-58755") 
p2 = ggplot()+ 
  theme_bw()+
  geom_rect(data = ss,(aes(xmin = sunrise_time, xmax=sunset_time,  
                           ymax= 0, ymin = -max(x$depth, na.rm=TRUE))), fill = "grey") +
  geom_boxplot(data = x, aes(x=date_time, y=-depth)) +
  geom_point(data = x, aes(x=date_time, y=-depth)) +
  ylab("Depth (m)") + 
  xlab("Time (EST)") + 
  xlim(c(min(x$date_time), xmax = max(x$date_time)))
p2
ggsave("REV_tagA69-1602-58755_time.png",p2)
  
#  geom_rect(aes(xmin = day_start(sampling_date_time), xmax = sunrise_date_time,
#                ymin = min(depth), ymax = max(depth)),
#            fill = "grey")
  


#_----------------_#
# # shapes
# library(raster)
# REV
# ru34_1 = ru34_1 %>% 
#   filter(depth < 0.5, !is.na(longitude), !is.na(latitude)) %>%
#   arrange(date_time) %>%
#   dplyr::select(longitude, latitude) 
# coordinates(ru34_1)=~longitude+latitude
# proj4string(ru34_1)<- CRS("+proj=longlat +datum=WGS84")
# ru34_sp_line1 <- as(ru34_1,"SpatialLines")
# raster::shapefile(ru34_sp_line2, "ru34_1.shp")
# 
# ru34_2 = ru34_2 %>% 
#   filter(depth < 0.5, !is.na(longitude), !is.na(latitude)) %>%
#   arrange(date_time) %>%
#   dplyr::select(longitude, latitude) 
# coordinates(ru34_2)=~longitude+latitude
# proj4string(ru34_2)<- CRS("+proj=longlat +datum=WGS84")
# ru34_sp_line2 <- as(ru34_2,"SpatialLines")
# raster::shapefile(ru34_sp_line2, "ru34_2.shp")
# 
# SFW
# ru34_0 = read_csv("Downloads/ru34-20230303T1521-trajectory-raw-delayed_1710_aa1f_b22b.csv", skip=1)
# names(ru34_0) = c("date_time","latitude","longitude","depth")
# ru34_0 = ru34_0 %>% 
#   filter(depth < 0.5, !is.na(longitude), !is.na(latitude)) %>%
#   arrange(date_time) %>%
#   dplyr::select(longitude, latitude) 
# coordinates(ru34_0)=~longitude+latitude
# proj4string(ru34_0)<- CRS("+proj=longlat +datum=WGS84")
# ru34_sp_line0 <- as(ru34_0,"SpatialLines")
# raster::shapefile(ru34_sp_line0, "ru34_0.shp")
