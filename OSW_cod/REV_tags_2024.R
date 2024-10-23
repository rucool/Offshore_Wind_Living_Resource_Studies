#---------------#
## load packages
#---------------#
library(rgdal)
library(cowplot)
library(readxl)
library(readr)
library(dplyr)
library(lubridate) 
library(raster)
library(sp)
library(zoo)
library(ggplot2)
#---------------#

#---------------#
## load data
#---------------#
# REV
# library(rgdal)
leases = readOGR(dsn = "~/Downloads/BOEM-Renewable-Energy-Shapefiles_22_10_21/", layer = "BOEMWindLeaseOutlines_6_30_2022")
REV = leases[leases@data$LEASE_NUMB %in% "OCS-A 0486",]
rm(leases)

# import known cod tags
# library(readxl)
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")

# check against detections
ids = c("A69-1601-60951","A69-1602-58719","A69-1602-58721","A69-1602-58735",
        "A69-1602-58746","A69-1602-58751","A69-1602-58753","A69-1602-58754",
        "A69-1602-58755","A69-1602-58800","A69-1602-58809","A69-1602-58810",
        "A69-1602-63876","A69-1604-63165","A69-9001-58738","A69-9001-59363") 
ids=as.data.frame(ids)
names(ids) = "TagID"

# check
# library(dplyr)
ids = inner_join(ids, FreyTags, by = c("TagID"="Tag ID"))

# Open Dave's files
#library(readxl)
detections <- read_excel("Downloads/dkaragon_analysis_detections.xlsx", 
                         col_names = FALSE)
names(detections) = c("date","time","x","tag_id1","tag_id2","TagID","VMT")
detections$Species = "Unknown"
detections = detections %>% 
  mutate(Species = ifelse(TagID %in% ids$TagID,"Cod",Species),
         date = as.Date(date))
detections$date_time = as.POSIXct(paste(detections$date, sapply(strsplit(as.character(detections$time)," "),tail,1), sep=" "), tz="UTC")

# summary table
dsum = group_by(detections,TagID) %>% 
  summarize(n=n(),species=unique(Species),
            dates=toString(unique(date)))

# library(R.matlab)
# daves <- readMat("~/Downloads/ru34-20240301T1336-trajectory-raw-rt_84fb_2d2b_3d82.mat")
# str(daves)
# head(daves$ru34.20240301T1336.trajectory.r)
# daves_df = as.data.frame(matrix(data = NA, ncol=3, nrow=255094))
# names(daves_df) = c("time","latitude","longitude")
# daves_df$time = daves$ru34.20240301T1336.trajectory.r[[1]]
# daves_df$latitude = daves$ru34.20240301T1336.trajectory.r[[2]]
# daves_df$longitude = daves$ru34.20240301T1336.trajectory.r[[3]]
# daves2 <- readMat("~/Downloads/ru34-20240112T1722-trajectory-raw-rt_a7ff_9db4_2f8c.mat")
# str(daves2)
# head(daves2$ru34.20240112T1722.trajectory.r)

## download from ERDDAP
# VMT and Glider time are in UTC, beware of daylight savings 
#ru34_1 = read_csv("Downloads/ru34-20240112T1722-profile-sci-delayed_d818_b1b6_2158.csv",skip=1)
#ru34_2 = read_csv("Downloads/ru34-20240301T1336-profile-sci-delayed_2936_c8a7_2595.csv",skip=1)
#names(ru34_1) = c("date_time","latitude","longitude","depth","pressure","bathy")
#names(ru34_2) = c("date_time","latitude","longitude","depth","pressure","bathy")
# library(readr)
ru34_1 = read_csv("Downloads/ru34-20240112T1722-trajectory-raw-delayed_4744_9558_2e17.csv",skip=1)
ru34_2 = read_csv("Downloads/ru34-20240301T1336-trajectory-raw-delayed_a92e_a68a_e11e.csv",skip=1)
names(ru34_1) = c("date_time","latitude","longitude","depth")
names(ru34_2) = c("date_time","latitude","longitude","depth")
ru34_1$deployment=1
ru34_2$deployment=2
ru34 = rbind(ru34_1, ru34_2)
ru34 = unique(ru34) # remove dups
ru34$date_time <- as.POSIXct(ru34$date_time,tz="UTC") # define as UTC

# max dive depth range
x = group_by(ru34, date(date_time)) %>% summarize(maxd = max(depth, na.rm=TRUE))
min(x$maxd)
max(x$maxd)
rm(x)

#ru34$date_time = as.POSIXct(format(ru34$date_time,tz="EST")) # change to EST
# interpolate depths
#library(zoo)
#ru34$int_depth <- na.locf(ru34$depth) #not interp, just grabs closest in front
# library(lubridate)
ru342 = ru34 %>% 
  mutate(dt = round_date(date_time, unit = '1 min')) %>%
  dplyr::select(-date_time) %>%
  filter(!is.na(depth)) %>% 
  group_by(dt) %>%
  summarize(latitude=mean(latitude, na.rm=TRUE),
            longitude=mean(longitude, na.rm=TRUE),
            depth=mean(depth, na.rm=TRUE),
            deployment=first(deployment))

# library(raster)
# library(sp)
sp.RU34 = ru342 %>% filter(!is.na(longitude), !is.na(latitude))
sp.RU34$id = sp.RU34$deployment
coordinates(sp.RU34) = ~longitude+latitude
sp.l.RU34 <- lapply(split(sp.RU34, sp.RU34$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
lines <- SpatialLines(sp.l.RU34)
data <- data.frame(id = unique(sp.RU34$id))
rownames(data) <- data$id
spldf <- SpatialLinesDataFrame(lines, data)

# sp.ru34_1=ru34_1 %>% filter(!is.na(longitude), !is.na(latitude))
# coordinates(sp.ru34_1) = ~longitude+latitude
# sp.ru34_1$id = 1
# l.ru34_1 = lapply(split(sp.ru34_1, sp.ru34_1$id), function(x) Lines(list(Line(coordinates(x))), x$id[1L]))
# spl.ru34_1 <- SpatialLines(l.ru34_1)


# import sunrise time for color paneling
ss <- read_excel("Downloads/RI_sunrise_sunset.xlsx")
ss$sunrise_time = as.POSIXct(paste(ss$Date, sapply(strsplit(as.character(ss$Sunrise)," "),tail,1), sep=" "))
ss$sunset_time = as.POSIXct(paste(ss$Date, sapply(strsplit(as.character(ss$Sunset)," "),tail,1), sep=" "))

# lunar phase
lunar <- read_excel("Downloads/Lunar Phase RI.xlsx")
lunar$Date = as.Date(lunar$Date)

lunar_d = as.data.frame(unique(date(detections$date_time))) 
names(lunar_d) = "Date"
lunar_d = lunar_d %>% 
  mutate(Phase = NA) %>%
  rowwise() %>%
  mutate(Phase = ifelse(as.character(Date) %in% as.character(lunar$Date),
                        lunar$Phase, NA))
lunar_d = bind_rows(lunar_d, lunar) %>% 
  arrange(Date) 

# library(zoo)
lunar_d$Phase = na.locf(lunar_d$Phase)
names(lunar_d) = c("date","phase")
detections2 = left_join(detections2, lunar_d, by=c("date"))

# add depth to detections
#detections = left_join(detections, ru34, by="date_time")
library(lubridate)
detections2 = detections %>% 
  mutate(dt = round_date(date_time, unit = '1 min')) %>% 
  left_join(., ru342, by="dt")
detections2$date_time = as.POSIXct(format(detections2$dt, tz="EST")) # change to EST
#---------------#

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

#  geom_rect(aes(xmin = day_start(sampling_date_time), xmax = sunrise_date_time,
#                ymin = min(depth), ymax = max(depth)),
#            fill = "grey")
