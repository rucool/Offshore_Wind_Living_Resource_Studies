library(dplyr)
library(readxl)
library(lubridate)
library(data.table)
library(sf)

# ------------------------- #
# import data
# ------------------------- #
NARW <- read_excel("Downloads/NARW_REV_2024.xlsx")
names(NARW) = c("start","end","Lfreq","Hfreq")
  
# download from ERDDAP
# VMT and Glider time are in UTC, beware of daylight savings 
library(readr)
#ru34_1 = read_csv("Downloads/ru34-20240112T1722-profile-sci-delayed_d818_b1b6_2158.csv",skip=1)
ru34_1 = read_csv("Downloads/ru34-20240112T1722-trajectory-raw-delayed_3ed1_2c6c_022b.csv",skip=1)
#ru34_2 = read_csv("Downloads/ru34-20240301T1336-profile-sci-delayed_2936_c8a7_2595.csv",skip=1)
ru34_2 = read_csv("Downloads/ru34-20240301T1336-trajectory-raw-delayed_93d7_f71c_3191.csv",skip=1)
names(ru34_1) = c("date_time","latitude","longitude","depth")
names(ru34_2) = c("date_time","latitude","longitude","depth")
ru34_1$lineID = 1
ru34_2$lineID = 2

# combine tracks
ru34 = rbind(ru34_1, ru34_2)
ru34 = unique(ru34) # remove dups
ru34$date_time <- as.POSIXct(ru34$date_time,tz="UTC") # define as UTC
#ru34$date_time = as.POSIXct(format(ru34$date_time,tz="EST")) # change to EST
# ------------------------- #

# ------------------------- #
# add locations
# ------------------------- #
startNARW= NARW %>% rename(date_time=start)
setDT(ru34) 
setkey(ru34, date_time)
setDT(startNARW) 
setkey(startNARW, date_time)
combined <- ru34[startNARW, roll = "nearest" ]
# ------------------------- #

# ------------------------- #
# make spatial and export shape
# ------------------------- #
# make spatial feature
spNARW = combined %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326)
spRU34 = ru34 %>% 
  filter(!is.na(longitude),!is.na(latitude)) %>% 
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(4326) %>%  
  group_by(lineID) %>%
  dplyr::summarize(do_union=FALSE) %>%  
  st_cast("LINESTRING") 

# export
st_write(spNARW, "REV_NARWs.shp")
st_write(spRU34, "REV_RU34_2024.shp")
# ------------------------- #
