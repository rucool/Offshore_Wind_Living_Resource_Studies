# load packages
require(ggplot2)
require(lubridate)
require(readr)
require(cmocean)
require(ggpubr)

# load data
# mission 1: ru34-20241102T1737
# mission 2: unit_1190-20241218T1433
# mission 3: ru34-20250113T1244
# mission 4: unit_1190-20250224T1405
# mission 5: ru34-20250311T1220

# mission 1: ru34-20241102T1737
ru34_sci_nov24 = read_csv("~/Downloads/ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
names(ru34_sci_nov24) = c("date_time","latitude","longitude","depth", 
                          "cdom","chl","alt","pitch","roll","oxy","pressure",
                          "sal", "temp", "u", "v", "wdep")
ru34_sci_nov24$date_time <- as.POSIXct(ru34_sci_nov24$date_time,tz="UTC") # define as UTC
ru34_sci_nov24 = unique(ru34_sci_nov24) # remove dups

# mission 2: unit_1190-20241218T1433
unit1190_sci_dec24 = read_csv("~/Downloads/unit_1190-20241218T1433-profile-sci-delayed_5874_356b_e4af.csv", skip=1)
names(unit1190_sci_dec24) = c("date_time","latitude","longitude","depth", 
                          "alt","pitch","roll","pressure",
                          "sal", "temp", "u", "v", "wdep")
unit1190_sci_dec24$date_time <- as.POSIXct(unit1190_sci_dec24$date_time,tz="UTC") # define as UTC
unit1190_sci_dec24 = unique(unit1190_sci_dec24) # remove dups
unit1190_sci_dec24 = mutate(unit1190_sci_dec24, cdom=NaN, chl=NaN, oxy=NaN)

# mission 3: ru34-20250113T1244
ru34_sci_jan25 = read_csv("~/Downloads/ru34-20250113T1244-profile-sci-rt_3c58_fe07_83d5.csv", skip=1)
names(ru34_sci_jan25) = c("date_time","latitude","longitude","depth", 
                          "cdom","chl","alt","pitch","roll","oxy","pressure",
                          "sal", "temp", "u", "v", "wdep")
ru34_sci_jan25$date_time <- as.POSIXct(ru34_sci_jan25$date_time,tz="UTC") # define as UTC
ru34_sci_jan25 = unique(ru34_sci_jan25) # remove dups

# mission 4: unit_1190-20250224T1405
unit1190_sci_feb25 = read_csv("~/Downloads/unit_1190-20250224T1405-profile-sci-rt_d43a_0c32_acb2.csv", skip=1)
names(unit1190_sci_feb25) = c("date_time","latitude","longitude","depth", 
                              "alt","pitch","roll","pressure",
                              "sal", "temp", "wdep")
unit1190_sci_feb25$date_time <- as.POSIXct(unit1190_sci_feb25$date_time,tz="UTC") # define as UTC
unit1190_sci_feb25 = unique(unit1190_sci_feb25) # remove dups
unit1190_sci_feb25 = mutate(unit1190_sci_feb25, 
                            u=NaN, v=NaN, cdom=NaN, chl=NaN, oxy=NaN,
                            sal = ifelse(sal > 100, NaN, sal))

# mission 5: ru34-20250311T1220
ru34_sci_mar25 = read_csv("~/Downloads/ru34-20250311T1220-profile-sci-rt_5f2a_6a52_0263.csv", skip=1)
names(ru34_sci_mar25) = c("date_time","latitude","longitude","depth", 
                          "cdom","chl","alt","pitch","roll","oxy","pressure",
                          "sal", "temp", "u", "v", "wdep")
ru34_sci_mar25$date_time <- as.POSIXct(ru34_sci_mar25$date_time,tz="UTC") # define as UTC
ru34_sci_mar25 = unique(ru34_sci_mar25) # remove dups

#library(gtools)
#do.call(smartbind,l)
whole_mission = rbind(ru34_sci_nov24, unit1190_sci_dec24)
whole_mission = rbind(whole_mission, ru34_sci_jan25)
whole_mission = rbind(whole_mission, unit1190_sci_feb25)
whole_mission = rbind(whole_mission, ru34_sci_mar25)

# plots
# TEMP
all_temp = ggplot() + 
  geom_point(data = whole_mission, aes(x = date_time, y = -depth, col = temp)) + 
  scale_colour_cmocean(name = "thermal")+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)", title = "Temperature")+ 
  theme_bw() + theme(text = element_text(size= 20)) 
   

m1_temp = ggplot() + 
  geom_point(data = ru34_sci_nov24 %>% filter(!is.na(alt)), aes(x = date_time, y = -alt, col = temp)) + 
  scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
  theme_bw()

m2_temp = ggplot() + 
  geom_point(data = unit1190_sci_dec24, aes(x = date_time, y = -depth, col = temp)) + 
  scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
  theme_bw()

m3_temp = ggplot() + 
  geom_point(data = ru34_sci_jan25, aes(x = date_time, y = -depth, col = temp)) + 
  scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
  theme_bw()

m4_temp = ggplot() + 
  geom_point(data = unit1190_sci_feb25, aes(x = date_time, y = -depth, col = temp)) + 
  scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
  theme_bw()

m5_temp = ggplot() + 
  geom_point(data = ru34_sci_mar25 %>% filter(!is.na(temp)), aes(x = date_time, y = -depth, col = temp)) + 
  scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
  theme_bw()

# SAL
all_sal = ggplot() + 
  geom_point(data = whole_mission%>% filter(!is.na(sal)), 
             aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity", title = "Salinity")+ 
  theme_bw() + theme(text = element_text(size= 20)) 

m1_sal = ggplot() + 
  geom_point(data = ru34_sci_nov24, aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
  theme_bw()

m2_sal = ggplot() + 
  geom_point(data = unit1190_sci_dec24, aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
  theme_bw()

m3_sal = ggplot() + 
  geom_point(data = ru34_sci_jan25, aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
  theme_bw()

m4_sal = ggplot() + 
  geom_point(data = unit1190_sci_feb25 %>% filter(!is.na(sal)), aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
  theme_bw()

m5_sal = ggplot() + 
  geom_point(data = ru34_sci_mar25 %>% filter(!is.na(sal)), aes(x = date_time, y = -depth, col = sal)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
  theme_bw()

# OXY
all_oxy = ggplot() + 
  geom_point(data = whole_mission %>% filter(!is.na(oxy)), 
             aes(x = date_time, y = -depth, col = oxy)) + 
  scale_colour_cmocean(name = "deep")+
  xlim(min(whole_mission$date_time), max(whole_mission$date_time)) + 
  labs(x = "Date", y = "Depth (m)", col = "Oxygen", title = "Oxygen")+ 
  theme_bw() + theme(text = element_text(size= 20)) 

m1_oxy = ggplot() + 
  geom_point(data = ru34_sci_nov24 %>% filter(!is.na(oxy)), aes(x = date_time, y = -depth, col = oxy)) + 
  scale_colour_cmocean(name = "deep")+
  labs(x = "Date", y = "Depth (m)", col = "Oxygen")+ 
  theme_bw()+ 
  theme(legend.position="bottom") 

m5_oxy = ggplot() + 
  geom_point(data = ru34_sci_mar25 %>% filter(!is.na(oxy)), aes(x = date_time, y = -depth, col = oxy)) + 
  scale_colour_cmocean(name = "deep")+
  labs(x = "Date", y = "Depth (m)", col = "Oxygen")+ 
  theme_bw()+ 
  theme(legend.position="bottom") 

# Chl
all_chl = ggplot() + 
  geom_point(data = whole_mission %>% filter(!is.na(chl)), 
             aes(x = date_time, y = -depth, col = chl)) + 
  scale_colour_cmocean(name = "algae")+
  xlim(min(whole_mission$date_time), max(whole_mission$date_time)) + 
  labs(x = "Date", y = "Depth (m)", col = "Chl", title = "Chlorophyll a")+ 
  theme_bw() + theme(text = element_text(size= 20)) 

m1_chl = ggplot() + 
  geom_point(data = ru34_sci_nov24 %>% filter(!is.na(chl)), aes(x = date_time, y = -depth, col = chl)) + 
  scale_colour_cmocean(name = "algae")+
  labs(x = "Date", y = "Depth (m)", col = "Chl")+ 
  theme_bw()

m5_chl = ggplot() + 
  geom_point(data = ru34_sci_mar25 %>% filter(!is.na(chl)), aes(x = date_time, y = -depth, col = chl)) + 
  scale_colour_cmocean(name = "algae")+
  labs(x = "Date", y = "Depth (m)", col = "Chl")+ 
  theme_bw()

# combo plots
ggarrange(all_temp, all_sal, all_chl, all_oxy,
          labels = c("A.", "B.","C.","D."),
          ncol = 1, nrow = 4) %>%
  ggexport(width = 800, height = 800, filename = "cod2425_temp_sal.tiff")

ggarrange(m1_temp, m2_temp, m3_temp, m4_temp, m5_temp,
          labels = c("Mission 1", "Mission 2", "Mission 3", "Mission 4"),
          ncol = 1, nrow = 4, common.legend = TRUE, legend="right") %>%#, legend="bottom") %>%
  ggexport(width = 800, height = 800, filename = "cod2425_temp.tiff")

ggarrange(m1_sal, m2_sal, m3_sal, m4_sal,  m5_sal,
          labels = c("Mission 1", "Mission 2", "Mission 3", "Mission 4"),
          ncol = 1, nrow = 4, common.legend = TRUE, legend="right") %>%#, legend="bottom") %>%
  ggexport(width = 800, height = 800, filename = "cod2425_sal.tiff")

ggarrange(m1_oxy, m3_oxy, m5_oxy,
          labels = c("Mission 1", "Mission 3"),
          ncol = 1, nrow = 2)

ggarrange(m1_chl, m3_chl, m5_chl,
          labels = c("Mission 1", "Mission 3"),
          ncol = 1, nrow = 2)

