library(dplyr)
library(readxl)
library(lubridate)
library(data.table)
library(sf)

# ------------------------- #
# import data
# ------------------------- #
## REV
Cod <- read_excel("Downloads/REV_CodGrunts_2024.xlsx")
names(Cod) = c("start","end","Lfreq","Hfreq")

# download from ERDDAP
library(readr)
ru34_1 = read_csv("Downloads/ru34-20240112T1722-trajectory-raw-delayed_4744_9558_2e17.csv",skip=1)
ru34_2 = read_csv("Downloads/ru34-20240301T1336-trajectory-raw-delayed_a92e_a68a_e11e.csv",skip=1)
names(ru34_1) = c("date_time","latitude","longitude","depth")
names(ru34_2) = c("date_time","latitude","longitude","depth")
ru34_1$lineID = 1
ru34_2$lineID = 2

# combine tracks
ru34 = rbind(ru34_1, ru34_2)
ru34 = unique(ru34) # remove dups
ru34$date_time <- as.POSIXct(ru34$date_time,tz="UTC") # define as UTC
#ru34$date_time = as.POSIXct(format(ru34$date_time,tz="EST")) # change to EST

## SFW
ru34_sfw = read_csv("Downloads/ru34-20230303T1521-trajectory-raw-delayed_1710_aa1f_b22b.csv", skip=1)
names(ru34_sfw) = c("date_time","latitude","longitude","depth")
# ru34_0 = ru34_0 %>% 
#   filter(depth < 0.5, !is.na(longitude), !is.na(latitude)) %>%
#   arrange(date_time) %>%
#   dplyr::select(longitude, latitude) 
# coordinates(ru34_0)=~longitude+latitude
# proj4string(ru34_0)<- CRS("+proj=longlat +datum=WGS84")
# ru34_sp_line0 <- as(ru34_0,"SpatialLines")
# raster::shapefile(ru34_sp_line0, "ru34_0.shp")

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


# SFW Cod
SFW_Cod = as.data.frame(rbind(c('8 Mar 2023 06:29', 41.108, -71.129),
                              c('16 Mar 2023 08:28', 41.084, -71.162),
                              c('17 Mar 2023 19:04', 41.109, -71.197),
                              c('19 Mar 2023 00:40', 41.099, -71.114)))
names(SFW_Cod) = c('date_time','latitude','longitude')
setDT(SFW_Cod) 
# ------------------------- #

# ------------------------- #

# ------------------------- #
# add locations
# ------------------------- #
# combine grunts and locations
startCod= Cod %>% rename(date_time=start)
setDT(ru34) 
setkey(ru34, date_time)
setDT(startCod) 
setkey(startCod, date_time)
combined <- ru34[startCod, roll = "nearest" ]

# combine detections and locations
setDT(detections) 
setkey(detections, date_time)
dcombined <- ru34[detections, roll = "nearest" ]
# ------------------------- #

# ------------------------- #
# make spatial and export shape
# ------------------------- #
# make spatial feature
spCod = combined %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

spRU34 = ru34 %>% 
  filter(!is.na(longitude),!is.na(latitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326) %>%  
  group_by(lineID) %>%
  dplyr::summarize(do_union=FALSE) %>%  
  st_cast("LINESTRING") 

spSFWCod = SFW_Cod %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

spVemco = dcombined %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)

# export
st_write(spCod, "REV_Cod_2024.shp")
st_write(spRU34, "REV_RU34_2024.shp")
st_write(spSFWCod, "SFW_Cod_2024.shp")
st_write(spVemco, "REV_Vemco_2024.shp")
# ------------------------- #
