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
library(readxl)
library(purrr)
library(stringr)
library(geosphere)
#---------------#

# --------------- #
# import data
# --------------- #
### lease boundaries
#leases = st_read(dsn = "~/Downloads/BOEM_Renewable_Energy_Shapefiles_1/", layer = "Wind_Lease_Outlines_2_2023")
leases = st_read(dsn = "~/Downloads/BOEM_Renewable_Energy_Shapefiles_0/", layer = "BOEM_Wind_Lease_Outlines_06_06_2024")
REV = leases[leases$LEASE_NUMB %in% "OCS-A 0486",]
SRW = leases[leases$LEASE_NUMB %in% "OCS-A 0487",]
SFW = leases[leases$LEASE_NUMB %in% "OCS-A 0517",]
rm(leases)
PA1 = st_read(dsn = "~/Downloads/NMFS_Priority_Zone1/", layer = "NMFS_Priority_Zone1")
PA1 = st_transform(PA1, crs = st_crs(4326))
# ggplot() + 
#   geom_sf(data = REV, fill = NA) + 
#   geom_sf(data = SFW[1,], fill = NA) + 
#   geom_sf(data = SRW, fill = NA) + 
#   geom_sf(data = PA1, col="blue", fill = NA)

#### import known tags
FreyTags <- read_excel("Downloads/FreyAllSMASTTags.xlsx")
gvmt = read_csv("Downloads/glider_vmt_transmitters.csv")
cfrf = read_excel("~/Downloads/Orsted_leases_sync_tags.xlsx")

### import glider tags
laura_tag_summary = read_csv("~/Downloads/Orsted cod - tag detections/orstedcod_detectionsummary.txt") 
ru34_m1_tags = read_csv("~/Downloads/Orsted cod - tag detections/ru34-20241102T1737-rxlive-detectionsonly.csv") 
u1190_m2_tags = read_csv("~/Downloads/Orsted cod - tag detections/revcod_qualified_detections_2024.csv")
ru34_m3_tags = read_csv("~/Downloads/Orsted cod - tag detections/ru34-20250113T1244-rxlive-detectionsonly.csv") 
u1190_m4_tags = read_csv("~/Downloads/Orsted cod - tag detections/VUE_Export_unit_1190-20250224T1405.csv") 
ru34_m5_tags = read_csv("~/Downloads/Orsted cod - tag detections/ru34-20250311T1220-rxlive-detectionsonly.csv") 

### create tag date time
ru34_m1_tags$date_time = as.POSIXct(ru34_m1_tags$`Date and Time (UTC)`, 
                                    format="%m/%d/%Y %H:%M:%S", tz = "UTC")
ru34_m1_tags = dplyr::select(ru34_m1_tags, date_time, Transmitter, MISSION_ID)

ru34_m3_tags$date_time = as.POSIXct(ru34_m3_tags$`Date and Time (UTC)`, 
                                    format="%m/%d/%Y %H:%M:%S", tz = "UTC")
ru34_m3_tags = dplyr::select(ru34_m3_tags, date_time, Transmitter)

u1190_m4_tags$date_time = as.POSIXct(u1190_m4_tags$`Date and Time (UTC)`, 
                                     format="%m/%d/%Y %H:%M:%S", tz = "UTC")
u1190_m4_tags = dplyr::select(u1190_m4_tags, date_time, Transmitter)

ru34_m5_tags$date_time = as.POSIXct(ru34_m5_tags$`Date and Time (UTC)`, 
                                    format="%m/%d/%Y %H:%M:%S", tz = "UTC")
ru34_m5_tags = dplyr::select(ru34_m5_tags, date_time, Transmitter)

### match tag time to position
ru34_m1_path = read_csv("Downloads/ru34-20241102T1737-trajectory-raw-delayed_95c2_7988_89a9.csv", skip=1)
names(ru34_m1_path) = c("date_time", "latitude", "longitude", "depth") #UTC, m 
ru34_m1_path$date_time =  as.POSIXct(ru34_m1_path$date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
ru34_m1_path = unique(ru34_m1_path) #remove dups
#ru34_mission1_path = st_as_sf(ru34_mission1_path, coords = c("longitude", "latitude"), 
#                              crs = 4326, remove = FALSE) 

#unit1190_mission2_path = read_csv('~/Downloads/unit_1190-20241218T1433-trajectory-raw-delayed_07d8_cb64_24fa.kml')

ru34_m3_path = read_csv("Downloads/ru34-20250113T1244-trajectory-raw-delayed_60f3_f6e7_77a8.csv", skip=1)
names(ru34_m3_path) = c("date_time", "latitude", "longitude", "depth") #UTC, m 
ru34_m3_path$date_time =  as.POSIXct(ru34_m3_path$date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
ru34_m3_path = unique(ru34_m3_path) #remove dups

u1190_m4_path = read_csv("Downloads/unit_1190-20250224T1405-trajectory-raw-delayed_08b7_b81d_e54b.csv", skip=1)
names(u1190_m4_path) = c("date_time", "latitude", "longitude", "depth") #UTC, m 
u1190_m4_path$date_time =  as.POSIXct(u1190_m4_path$date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
u1190_m4_path = unique(u1190_m4_path) #remove dups

ru34_m5_path = read_csv("Downloads/ru34-20250311T1220-trajectory-raw-rt_490c_c8b2_edb6.csv", skip=1)
names(ru34_m5_path) = c("date_time", "latitude", "longitude", "depth") #UTC, m 
ru34_m5_path$date_time =  as.POSIXct(ru34_m5_path$date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")
ru34_m5_path = unique(ru34_m5_path) #remove dups

# filter to first day in the water
ru34_m1_tags = filter(ru34_m1_tags, 
                      date_time >= min(ru34_m1_path$date_time) & 
                        date_time <= max(ru34_m1_path$date_time))

u1190_m4_tags = filter(u1190_m4_tags, 
                      date_time >= min(u1190_m4_path$date_time) & 
                        date_time <= max(u1190_m4_path$date_time))

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

ru34_m1_tags$match_time = find_nearest_date(ru34_m1_tags$date_time, ru34_m1_path$date_time)
ru34_m1_tags2 = left_join(ru34_m1_tags, ru34_m1_path, by =c("match_time"="date_time"))

ru34_m3_tags$match_time = find_nearest_date(ru34_m3_tags$date_time, ru34_m3_path$date_time)
ru34_m3_tags2 = left_join(ru34_m3_tags, ru34_m3_path, by =c("match_time"="date_time"))

u1190_m4_tags$match_time = find_nearest_date(u1190_m4_tags$date_time, u1190_m4_path$date_time)
u1190_m4_tags2 = left_join(u1190_m4_tags, u1190_m4_path, by =c("match_time"="date_time"))

ru34_m5_tags$match_time = find_nearest_date(ru34_m5_tags$date_time, ru34_m5_path$date_time)
ru34_m5_tags2 = left_join(ru34_m5_tags, ru34_m5_path, by =c("match_time"="date_time"))

# #bad spatial 
# "A69-1602-58816"
# "A69-1602-58804"
# "A69-1602-58784"
# b1 = tags %>% filter(Transmitter == "A69-1602-58816")
# b2 = tags %>% filter(Transmitter == "A69-1602-58804")
# b3 = tags %>% filter(Transmitter == "A69-1602-58784")

  
# 
# # creating more matches than tag times
# setDT(ru34_mission1_path) 
# setDT(ru34_mission1_tags) 
# setkey(ru34_mission1_path, date_time)
# setkey(ru34_mission1_tags, date_time)
# ru34_mission1_tag_coords <-ru34_mission1_path[ru34_mission1_tags, roll = "nearest", mult = "all"]

u1190_m2_tags2 = dplyr::select(u1190_m2_tags, datecollected, 
                               fieldnumber, latitude, longitude) %>%
  rename(date_time = datecollected, Transmitter = fieldnumber) %>%
  mutate(date_time = as.POSIXct(date_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
  filter(date_time > as.Date("2024-10-31")) # includes last seasons' 2024 data from MATOS, filter out
u1190_m2_tags2$mission = 2
ru34_m3_tags2$mission = 3
u1190_m4_tags2$mission = 4
ru34_m5_tags2$mission = 5

tags = bind_rows(dplyr::select(ru34_m1_tags2, -MISSION_ID, -match_time) %>% 
                   mutate(mission=1), 
                 u1190_m2_tags2, ru34_m3_tags2, u1190_m4_tags2, ru34_m5_tags2) %>%
  filter(date_time > as.POSIXct("2024-10-31 12:00:00", format="%Y-%m-%d %H:%M:%S", tz = "UTC"))

tags$species = "unknown"
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "cod"], "cod", species))
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "Black sea Bass"], "BSB", species))
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "Fluke"], "SF", species))
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "Striped Bass"], "SB", species))
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "VR2TX internal sync"], "sync tag", species))
tags = mutate(tags, species = ifelse(Transmitter %in% FreyTags$`Tag ID`[FreyTags$species %in% "external sync"], "sync tag", species))
gvmt$species = "glider"
tags = mutate(tags, species = ifelse(Transmitter %in% gvmt$TransmitterID, "glider", species))
tags = mutate(tags, species = ifelse(Transmitter %in% str_trim(cfrf$ID), "sync tag", species))
tags = mutate(tags, species = ifelse(Transmitter %in% c('A69-1602-58728',
                                                        'A69-1602-58789',
                                                        'A69-1602-58793',
                                                        'A69-1602-58798',
                                                        'A69-1602-58805',
                                                        'A69-1602-58814', 
                                                        'A69-1602-58776'), 
                                     "dead cod", species))
#---------------#


#---------------#
# Time diff
#---------------#
cod = tags %>% filter(species %in% "cod")
cod_sum = cod %>% 
  group_by(Transmitter) %>%
  summarise(first_hit = min(date_time),
            last_hit = max(date_time))

export = as.data.frame(matrix(ncol=4,nrow=20,data=NA))
names(export) = c("first_lat","first_lon","last_lat","last_lon")
for(a in 1:20) {
  export$first_lat[a] = tags$latitude[tags$date_time %in% cod_sum$first_hit[a] & tags$species %in% "cod"]
  export$first_lon[a] = tags$longitude[tags$date_time %in% cod_sum$first_hit[a] & tags$species %in% "cod"]
  export$last_lat[a] = tags$latitude[tags$date_time %in% cod_sum$last_hit[a] & tags$species %in% "cod"]
  export$last_lon[a] = tags$longitude[tags$date_time %in% cod_sum$last_hit[a] & tags$species %in% "cod"]
}

cod_sum2 = cbind(cod_sum, export) %>%
  rowwise() %>% 
  mutate(distance = round(distm(c(first_lon, first_lat), c(last_lon, last_lat), 
                          fun = distHaversine), digits = 2), 
         secs = (last_hit - first_hit),
         mins = ifelse(secs > 60, round(secs/60, digits = 2), NA),
         days = ifelse(secs > 86400, round(secs/86400, digits = 2), NA))

median(cod_sum2$days[!cod_sum2$Transmitter %in% "A69-1602-58804"], na.rm=T)
mean(cod_sum2$days, na.rm=T)
sd(cod_sum2$days, na.rm=T)

round(median(cod_sum2$secs, na.rm=T)/86400, digits = 2)
round(mean(cod_sum2$secs, na.rm=T)/86400, digits = 2)
round(sd(cod_sum2$secs, na.rm=T)/86400, digits = 2)

round(median(cod_sum2$distance), digits = 2)
round(mean(cod_sum2$distance), digits = 2)
round(sd(cod_sum2$distance), digits = 2)
#---------------#

#---------------#
# plots
#---------------#
## custom colors
#colpal = c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00",
#           "#CAB2D6","#6A3D9A","#FFFF99","yellow2","#B15928","gold4","lightgrey","darkgrey")
# library(ggplot2)
#p + geom_point(data = ru34_m1_tags2, aes(x=longitude, y=latitude, col=Transmitter))

p = ggplot() + 
  geom_sf(data = REV, fill=NA) + 
  geom_sf(data = SFW[1,], col="cornflowerblue", fill=NA) + 
  geom_sf(data = SRW, col="orange", fill=NA) + 
  theme_bw()
p 

p4 = p + 
  geom_point(data = tags %>% filter(species %in% "cod"), 
             aes(x=longitude, y=latitude, col=Transmitter),size=2) + #, shape = as.character(mission)))+
  labs(title="Cod Detections", x="Longitude", y="Longitude") + #, shape="Mission")
  theme_bw() + 
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 13), 
        legend.text = element_text(size = 10)) 
p4
ggsave("REV_SRW_2425_spatial_tags.png",p4)

p6 = p + geom_point(data = tags %>% filter(species %in% "cod"), 
                    aes(x=longitude, y=latitude, col=Transmitter))+
  facet_wrap(~Transmitter)+
  labs(title="Cod Detections",x="Longitude",y="Longitude",shape="Mission")+
  theme_bw() + 
  theme(text = element_text(size=12), 
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
p6
ggsave("REV_SRW_2425_cod_tags_lease.png",p6, dpi=320)

p5 = ggplot() + 
  geom_point(data = tags %>% filter(species %in% "cod"), 
             aes(x=longitude, y=latitude, 
                 col=date_time, shape = as.character(mission)))+
  facet_wrap(~Transmitter, scales = "free", ncol=4)+
  labs(title="Cod Detections", x="Longitude", y="Longitude", 
       shape="Mission", col = "Date")+
  theme_bw() + 
  theme(text = element_text(size=10), 
        legend.position = "bottom") 
p5
ggsave("REV_SRW_2425_cod_space_time.png",p5, dpi=320)


# by tag
#z = tags[tags$Transmitter %in% "A69-1602-58805",]
tag_list = unique(tags$Transmitter[tags$species %in% "cod"])
ggplot() + 
  geom_point(data = tags[tags$Transmitter %in% tag_list[11],], 
             aes(x=longitude, y=latitude, col=Transmitter, 
                 shape = as.character(mission)))+
  labs(title="Cod Tags",x="Longitude",y="Longitude",shape="Mission")
#p + 
ggplot() +   geom_sf(data = SRW, col="orange", fill=NA) + 
  geom_point(data = tags[tags$Transmitter %in% tag_list[1],], 
             aes(x=longitude, y=latitude)) +
  labs(title="Cod Detections", x="Longitude", y="Longitude")+
  theme_bw() + 
  theme(text = element_text(size=12)) 

# Tag v Date
x = tags %>% 
  filter(!species %in% c("glider","dead cod","sync tag")) %>%
  mutate(mo = month(date_time)) %>%
  group_by(mo, species) %>%
  summarise(n=n()) %>%
  mutate(m = NA, 
         m = ifelse(mo %in% 11, "Nov.", m),
         m = ifelse(mo %in% 12, "Dec.", m),
         m = ifelse(mo %in% 1, "Jan.", m),
         m = ifelse(mo %in% 2, "Feb.", m),
         m = ifelse(mo %in% 3, "Mar.", m),
         o = NA, 
         o = ifelse(mo %in% 11, 1, o),
         o = ifelse(mo %in% 12, 2, o),
         o = ifelse(mo %in% 1, 3, o),
         o = ifelse(mo %in% 2, 4, o),
         o = ifelse(mo %in% 3, 5, o))
y = tags %>%
  filter(!species %in% c("glider","dead cod","sync tag")) %>%
  mutate(mo = month(date_time)) %>%
  group_by(mo, Transmitter, species) %>%
  summarise(n=n()) %>%
  group_by(mo, species) %>%
  summarise(n=n())  %>%
  mutate(m = NA, 
         m = ifelse(mo %in% 11, "Nov.", m),
         m = ifelse(mo %in% 12, "Dec.", m),
         m = ifelse(mo %in% 1, "Jan.", m),
         m = ifelse(mo %in% 2, "Feb.", m),
         m = ifelse(mo %in% 3, "Mar.", m),
         o = NA, 
         o = ifelse(mo %in% 11, 1, o),
         o = ifelse(mo %in% 12, 2, o),
         o = ifelse(mo %in% 1, 3, o),
         o = ifelse(mo %in% 2, 4, o),
         o = ifelse(mo %in% 3, 5, o))

p0a = ggplot()+
  geom_bar(data = x, aes(x=reorder(m,o), y=n, fill=species), 
           stat="identity", position = position_stack(reverse = TRUE),col="black") +
  theme_bw() +
  xlab("Month") + ylab("Number of Detections")+
  scale_fill_manual(values = c("#A6CEE3","#1F78B4"))+
  theme(legend.position="none", 
        text = element_text(size = 15)) 
p0a
p0b = ggplot()+
  geom_bar(data = y, aes(x=reorder(m, o), y=n, fill=species), 
           stat="identity", position = position_stack(reverse = TRUE),col="black") +
  theme_bw() +
  xlab("Month") + ylab("Unique Tags Detected")+
  scale_fill_manual(values = c("#A6CEE3","#1F78B4"))+
  theme(legend.position="bottom", 
        text = element_text(size = 15))# +
#scale_x_date(date_labels = "%b") 
p0b

#library(cowplot)
p0c = plot_grid(p0a, p0b, labels = "AUTO",ncol=1)
p0c
ggsave("REV_SRW_2425_tag_month_hist.png",p0c)

# Tag v Date
p1 = ggplot()+
  geom_point(data = tags %>% filter(species %in% "cod"), 
             aes(x = date_time, Transmitter), size=5, col="black")+
  geom_point(data = tags %>% filter(species %in% "cod"), 
             aes(x = date_time, Transmitter, col = Transmitter),size=4)+
  theme_bw() +
  xlab("Date") + ylab("Tag ID")+
  guides(color="none") + 
  theme(legend.position="none", 
        text = element_text(size = 15)) #+ 
#scale_x_date(date_labels = "%b %d") + 
# scale_colour_manual(values = colpal)
p1
#ggsave("REV_tag_date.png",p1)

p0 = plot_grid(p0c, p1, labels = c("","C"))
p0
ggsave("REV_SRW_2425_tag_month.png",p0)

# Tag v Time
x = tags %>% filter(species %in% "cod") %>% 
  group_by(Transmitter, date_time) 
#y = filter(ru34, date(date_time) %in% date(x$date_time) & hour(date_time) %in% hour(x$date_time))
p3 = ggplot(x, aes(x=date_time, y=-depth, col=Transmitter), size=5)+ 
  geom_point()+
  #facet_wrap(date(x$date_time), scales="free") + 
  facet_wrap(~Transmitter + date(date_time), scales="free_x", ncol=7) + 
  theme_bw() +
  ylab("Depth (m)") + 
  xlab("Time (EST)") +
  #guides(color="none") + 
  theme(legend.position="bottom") #+ 
#geom_point(data = y, aes(x=date_time, y=-depth), pch=1)
p3
ggsave("REV_SRW_2425_depth_time.png",p3)


# # spatial
# #library(RColorBrewer)
# p4 = ggplot() + 
#   geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
#   geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
#   geom_point(data = detections2, aes(x=longitude, y=latitude, pch = Species), col= "black", size=6)+
#   geom_point(data = detections2, aes(x=longitude, y=latitude, col = TagID, pch = Species), size=5) +
#   scale_colour_manual(values = colpal) + 
#   theme_bw()+
#   labs(x="Longitude", y="Latitude") + 
#   theme(text = element_text(size = 15),
#         legend.position="bottom")
# #scale_color_brewer(palette = "Paired")
# p4
# ggsave("REV_track_tags.png",p4)
# 
# taglist = unique(detections2$TagID)
# taglist = sort(taglist)
# for (a in 1:16){
#   td = filter(detections2,TagID %in% taglist[a])
#   ts = ifelse(td$Species[1] %in% "Unknown",17,16)
#   ts2 = ifelse(td$Species[1] %in% "Unknown",2,1)
#   pp1 = ggplot() + 
#     geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
#     geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
#     geom_point(data = td, aes(x=longitude, y=latitude, col = TagID), pch = ts, size=5) +
#     geom_point(data = td, aes(x=longitude, y=latitude), pch=ts2, col= "black", size=5)+
#     scale_colour_manual(values = colpal[a]) + 
#     theme_bw()+
#     labs(x="", y="Latitude") + 
#     theme(text = element_text(size = 15), legend.position="none")
#   #pp1
#   pp2= ggplot() +  
#     #geom_polygon(data = REV, aes(x=long, y=lat, group=group), fill="lightcyan1") +
#     geom_path(data = spldf, aes(x=long, y=lat, group=group), col="snow3") +
#     geom_point(data = td, aes(x=longitude, y=latitude, col = TagID), pch = ts, size=5) +
#     geom_point(data = td, aes(x=longitude, y=latitude), pch=ts2, col= "black", size=5)+
#     scale_colour_manual(values = colpal[a]) + 
#     theme_bw()+
#     labs(x="Longitude", y="Latitude") + 
#     theme(text = element_text(size = 15),
#           axis.text.x = element_text(angle=45),
#           legend.position="bottom") +
#     ylim(min(td$latitude, na.rm=TRUE)-0.0003, max(td$latitude, na.rm=TRUE)+0.0003) + 
#     xlim(min(td$longitude, na.rm=TRUE)-0.0003,max(td$longitude, na.rm=TRUE)+0.0003) +
#     coord_fixed()
#   #pp2
#   
#   pp3 = ggplot() +
#     geom_point(data = td, aes(x=date_time, y=-depth, col = TagID), pch = ts, size=5) +
#     geom_point(data = td, aes(x=date_time, y=-depth), pch = ts2, col="black", size=5) +
#     scale_colour_manual(values = colpal[a]) + 
#     theme_bw() + 
#     theme(text = element_text(size = 15), legend.position="none") + 
#     labs(x="Date and/or Time", y="Depth (m)") 
#   pp3
#   
#   pp4 = plot_grid(pp2, pp3, labels = "")
#   pp4
#   
#   pp5 = plot_grid(pp1, pp4, labels = "", ncol=1)
#   pp5
#   
#   ggsave(paste("REV_track_tags_",td$TagID[1],".png", sep=""),pp3)
# }
# #---------------#
# 

#---------------#
# cod in REV in 2024 spawning season and 2025 spawning season
#---------------#
#---------------#
