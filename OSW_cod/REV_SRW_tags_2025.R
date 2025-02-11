#---------------#
## load packages
#---------------#
library(cowplot)
library(readr)
library(dplyr)
library(lubridate) 
library(raster)
library(ggplot2)
library(sf)
library(data.table)
#---------------#

# --------------- #
# import data
# --------------- #
# lease boundaries
leases = st_read(dsn = "~/Downloads/BOEM_Renewable_Energy_Shapefiles_1/",
                 layer = "Wind_Lease_Outlines_2_2023")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
rm(leases)
p = ggplot() + 
  geom_sf(data = REV, fill=NA) + 
  geom_sf(data = SFW, col="cornflowerblue", fill=NA) + 
  geom_sf(data = SRW, col="orange", fill=NA) + theme_bw()
#p 

# import known cod tags
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")
ru34_mission1_tags = read_csv("Downloads/ru34-20241102T1737-rxlive-detectionsonly.csv")
#unit1190_mission2_tags = read_csv("Downloads/.csv")
#ru34_mission3_tags = read_csv("Downloads/.csv")
#unit1190_mission4_tags = read_csv("Downloads/.csv")

# create tag date time
ru34_mission1_tags$date_time = as.POSIXct(ru34_mission1_tags$`Date and Time (UTC)`, format="%m/%d/%Y %H:%M:%S", tz = "UTC")
ru34_mission1_tags = dplyr::select(ru34_mission1_tags, date_time, Transmitter, MISSION_ID)

# match tag time to position
ru34_mission1_path = read_csv("Downloads/ru34-20241102T1737-trajectory-raw-delayed_95c2_7988_89a9.csv", skip=1)
names(ru34_mission1_path) = c("date_time", "latitude", "longitude", "depth") #UTC, m 
ru34_mission1_path$date_time =  as.POSIXct(ru34_mission1_path$date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
ru34_mission1_path = unique(ru34_mission1_path) #remove dups
ru34_mission1_path = st_as_sf(ru34_mission1_path, coords = c("longitude", "latitude"), 
                              crs = 4326, remove = FALSE) 
#unit1190_mission2_path = read_csv('~/Downloads/unit_1190-20241218T1433-trajectory-raw-delayed_07d8_cb64_24fa.kml')
#ru34_mission3_path = read_csv('~/Downloads/.kml')
#unit1190_mission4_path = read_csv('~/Downloads/.kml')

# filter to first day in the water
ru34_mission1_tags = filter(ru34_mission1_tags, 
                            date_time >= min(ru34_mission1_path$date_time) & 
                            date_time <= max(ru34_mission1_path$date_time))

# combine detection with location
find_nearest_date <- function(date, date_match){  
  date_nearest <- date %>% 
    map_dbl(find_nearest_date_worker, date_match) %>% 
    as.POSIXct(tz = "UTC", origin = "1970-01-01")
  return(date_nearest)
}

find_nearest_date_worker <- function(date, date_vector) {
  delta <- abs(date - date_vector)
  index <- which.min(delta)
  x <- date_vector[index]
  return(x)
}

ru34_mission1_tags$match_time = find_nearest_date(ru34_mission1_tags$date_time, ru34_mission1_path$date_time)
ru34_mission1_tags = left_join(ru34_mission1_tags, ru34_mission1_path, by =c("match_time","date_time"))

# 
# # creating more matches than tag times
# setDT(ru34_mission1_path) 
# setDT(ru34_mission1_tags) 
# setkey(ru34_mission1_path, date_time)
# setkey(ru34_mission1_tags, date_time)
# ru34_mission1_tag_coords <-ru34_mission1_path[ru34_mission1_tags, roll = "nearest", mult = "all"]

p + geom_point(data = ru34_mission1_tags, aes(x=Longitude, y=Latitude, col=Transmitter))


#---------------#
# plots
#---------------#
# custom colors
colpal = c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00",
           "#CAB2D6","#6A3D9A","#FFFF99","yellow2","#B15928","gold4","lightgrey","darkgrey")
# library(ggplot2)

# Tag v Date
x = detections2 %>% 
  mutate(mo = month(date_time)) %>%
  group_by(mo, Species) %>%
  summarise(n=n())
y = detections2 %>% 
  mutate(mo = month(date_time)) %>%
  group_by(mo, TagID, Species) %>%
  summarise(n=n()) %>%
  group_by(mo,Species) %>%
  summarise(n=n())

p0a = ggplot()+
  geom_bar(data = x, aes(x=mo,y=n,fill=Species), 
           stat="identity", position = position_stack(reverse = TRUE),col="black") +
  theme_bw() +
  xlab("Month") + ylab("Number of Detections")+
  scale_fill_manual(values = c("#A6CEE3","#1F78B4"))+
  theme(legend.position="none", 
        text = element_text(size = 15)) 
p0a
p0b = ggplot()+
  geom_bar(data = y, aes(x=mo,y=n,fill=Species), 
           stat="identity", position = position_stack(reverse = TRUE),col="black") +
  theme_bw() +
  xlab("Month") + ylab("Unique Tags Detected")+
  scale_fill_manual(values = c("#A6CEE3","#1F78B4"))+
  theme(legend.position="bottom", 
        text = element_text(size = 15)) +
  scale_x_date(date_labels = "%b") 
p0b

#library(cowplot)
p0c = plot_grid(p0a, p0b, labels = "AUTO",ncol=1)
p0c

# Tag v Date
p1 = ggplot()+
  geom_point(data = detections, aes(x=date,TagID,shape=Species),size=6, col="black")+
  geom_point(data = detections, aes(x=date,TagID,col=TagID,shape=Species),size=5)+
  theme_bw() +
  xlab("Date") + ylab("Tag ID")+
  guides(color="none") + 
  theme(legend.position="bottom", 
        text = element_text(size = 15)) + 
  scale_x_date(date_labels = "%b %d") + 
  scale_colour_manual(values = colpal)
p1
#ggsave("REV_tag_date.png",p1)

p0 = plot_grid(p0c, p1, labels = c("","C"))
p0
ggsave("REV_tag_month.png",p0)

# Tag v Time
x = filter(detections2, TagID %in% "A69-1601-60951") 
p2 = ggplot()+ 
  theme_bw()+
  geom_rect(data = ss,(aes(xmin = sunrise_time, xmax=sunset_time,  
                           ymax= 0, ymin = -max(x$depth, na.rm=TRUE))), fill = "grey") +
  #geom_boxplot(data = x, aes(x=date_time, y=-depth)) +
  geom_point(data = x, aes(x=date_time, y=-depth)) +
  ylab("Depth (m)") + 
  xlab("Time (EST)") + 
  xlim(c(min(x$date_time), xmax = max(x$date_time)))
p2
ggsave("REV_tagA69-1602-58755_time.png",p2)

# Tag v Time
x = group_by(detections2, TagID, date_time) 
#y = filter(ru34, date(date_time) %in% date(x$date_time) & hour(date_time) %in% hour(x$date_time))
p3 = ggplot(x, aes(x=date_time, y=-depth, col=TagID, pch=Species), size=5)+ 
  geom_point()+
  #facet_wrap(date(x$date_time), scales="free") + 
  facet_wrap(~TagID+date(date_time), scales="free_x", ncol=7) + 
  theme_bw() +
  ylab("Depth (m)") + 
  xlab("Time (EST)") +
  #guides(color="none") + 
  theme(legend.position="bottom") #+ 
#geom_point(data = y, aes(x=date_time, y=-depth), pch=1)
p3
ggsave("REV_time.png",p3)

# spatial
#library(RColorBrewer)
p4 = ggplot() + 
  geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
  geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
  geom_point(data = detections2, aes(x=longitude, y=latitude, pch = Species), col= "black", size=6)+
  geom_point(data = detections2, aes(x=longitude, y=latitude, col = TagID, pch = Species), size=5) +
  scale_colour_manual(values = colpal) + 
  theme_bw()+
  labs(x="Longitude", y="Latitude") + 
  theme(text = element_text(size = 15),
        legend.position="bottom")
#scale_color_brewer(palette = "Paired")
p4
ggsave("REV_track_tags.png",p4)

taglist = unique(detections2$TagID)
taglist = sort(taglist)
for (a in 1:16){
  td = filter(detections2,TagID %in% taglist[a])
  ts = ifelse(td$Species[1] %in% "Unknown",17,16)
  ts2 = ifelse(td$Species[1] %in% "Unknown",2,1)
  pp1 = ggplot() + 
    geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
    geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
    geom_point(data = td, aes(x=longitude, y=latitude, col = TagID), pch = ts, size=5) +
    geom_point(data = td, aes(x=longitude, y=latitude), pch=ts2, col= "black", size=5)+
    scale_colour_manual(values = colpal[a]) + 
    theme_bw()+
    labs(x="", y="Latitude") + 
    theme(text = element_text(size = 15), legend.position="none")
  #pp1
  pp2= ggplot() +  
    #geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
    geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
    geom_point(data = td, aes(x=longitude, y=latitude, col = TagID), pch = ts, size=5) +
    geom_point(data = td, aes(x=longitude, y=latitude), pch=ts2, col= "black", size=5)+
    scale_colour_manual(values = colpal[a]) + 
    theme_bw()+
    labs(x="Longitude", y="Latitude") + 
    theme(text = element_text(size = 15),
          axis.text.x = element_text(angle=45),
          legend.position="bottom") +
    ylim(min(td$latitude, na.rm=TRUE)-0.0003, max(td$latitude, na.rm=TRUE)+0.0003) + 
    xlim(min(td$longitude, na.rm=TRUE)-0.0003,max(td$longitude, na.rm=TRUE)+0.0003) +
    coord_fixed()
  #pp2
  
  pp3 = ggplot() +
    geom_point(data = td, aes(x=date_time, y=-depth, col = TagID), pch = ts, size=5) +
    geom_point(data = td, aes(x=date_time, y=-depth), pch = ts2, col="black", size=5) +
    scale_colour_manual(values = colpal[a]) + 
    theme_bw() + 
    theme(text = element_text(size = 15), legend.position="none") + 
    labs(x="Date and/or Time", y="Depth (m)") 
  pp3
  
  pp4 = plot_grid(pp2, pp3, labels = "")
  pp4
  
  pp5 = plot_grid(pp1, pp4, labels = "", ncol=1)
  pp5
  
  ggsave(paste("REV_track_tags_",td$TagID[1],".png", sep=""),pp3)
}
#---------------#


#---------------#
# cod in REV in 2024 spawning season and 2025 spawning season
#---------------#
#---------------#
