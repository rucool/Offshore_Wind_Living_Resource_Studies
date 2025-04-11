# -------------------------- #
# load met data
#### nbdc.noaa.gov
# -------------------------- #

require(readxl)
require(lubridate)
require(ggplot2)
require(dplyr)

#### wind data
# wind speed
buzm3_nov2024 <- read_excel("Downloads/buzm3_nov2024.xlsx", skip = 1)
buzm3_dec2024 <- read_excel("Downloads/buzm3_dec2024.xlsx", skip = 1)
buzm3_jan2025 <- read_excel("Downloads/buzm3_jan2025.xlsx", skip = 1)
#buzm3_feb2025 <- read_excel("Downloads/buzm3_feb2025.xlsx", skip = 1)
#buzm3_mar2025 <- read_excel("Downloads/buzm3_mar2025.xlsx", skip = 1)

buz_wind_df = rbind(buzm3_nov2024, buzm3_dec2024, buzm3_jan2025)#, 
                    #buzm3_feb2025, buzm3_mar2025)
names(buz_wind_df) = c("year","month","day","hour","min","WDI","WSP","GD","GST","Gtime")
buz_wind_df$date_time =  make_datetime(year = buz_wind_df$year, 
                                       month = buz_wind_df$month,
                                       day = buz_wind_df$day,
                                       hour = buz_wind_df$hour,
                                       min = buz_wind_df$min)

#ggplot()+geom_line(data = buz_wind_df, aes(x = date_time, y = WSP))
#ggplot()+geom_line(data = buz_wind_df, aes(x = date_time, y = WDI))
#ggplot() + geom_quiver(data = buz_wind_df)


compass_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
degrees_per_sector <- 22.5
wind_to_compass <- function(degrees) {
  degrees <- degrees %% 360  # Ensure degrees are within 0-359 range
  sector_index <- ceiling(degrees / degrees_per_sector)
  # Handle edge case where sector_index is 17 (should be 1)
  if (sector_index == 17) {sector_index <- 1}
  if (sector_index == 0) {sector_index <- 1}
  return(compass_directions[sector_index])
}

library(dplyr)
compass_direction <- buz_wind_df %>% 
  rowwise() %>% 
  mutate(heading = wind_to_compass(WDI))

sum_comp_dir = compass_direction %>% group_by(heading, WDI, round(WSP)) %>% summarise(n=n())

p.wr1 <- plot.windrose(data = buz_wind_df,
                       spd = buz_wind_df$WSP,
                       dir = buz_wind_df$WDI)
p.wr2 <- plot.windrose(data = buz_wind_df[buz_wind_df$month %in% 11,],
                       spd = buz_wind_df[buz_wind_df$month %in% 11,]$WSP,
                       dir = buz_wind_df[buz_wind_df$month %in% 11,]$WDI)
p.wr3 <- plot.windrose(data = buz_wind_df[buz_wind_df$month %in% 12,],
                       spd = buz_wind_df[buz_wind_df$month %in% 12,]$WSP,
                       dir = buz_wind_df[buz_wind_df$month %in% 12,]$WDI)
p.wr4 <- plot.windrose(data = buz_wind_df[buz_wind_df$month %in% 1,],
                       spd = buz_wind_df[buz_wind_df$month %in% 1,]$WSP,
                       dir = buz_wind_df[buz_wind_df$month %in% 1,]$WDI)

library(ggpubr)
ggarrange(p.wr1, p.wr2, p.wr3, p.wr4, 
          labels = c("Total", "Nov. 24'", "Dec. 24'","Jan 25'"),
          ncol = 4, nrow = 1)


require(metR)
ggplot() +
  geom_arrow(data = buz_wind_df[buz_wind_df$month %in% 1,], 
             aes(x = date_time, y = 0, mag = WSP, angle = WDI, col = WSP),
             start = 0, direction = 'cw', pivot = 0) + 
  theme(legend.position = "none")

ggplot() +
  geom_arrow(data = buz_wind_df, aes(x = date_time, y = 0, mag = WSP, angle = WDI, col = WDI),
             start = 0, direction = 'cw', pivot = 1) + #dircetion = clockwise, pivot 0 mean arrow at the begging
  # Set arrow size in legend reference
  scale_mag_continuous(#max = 2,
                       labels = expression(2 ~  m ~ s^{-1})) +
  # Remove dark background and unneccessary figure elements
  theme_classic() +
  # Remove y axis text and position legend inside figure
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        legend.position = c(0.8, 0.2)) +
  # Remove x axis and legend labels
  labs(x = NULL,
       mag = NULL)


ggplot(buz_wind_df) +
  geom_segment(aes(x = date_time,
                   y = 0,
                   xend = date_time + lubridate::dhours(WSP * 1 * -cos((90-WDI) / 360 * 2 * pi)),
                   yend = WSP * 1 * -sin((90-WDI) / 360 * 2 * pi),
                   col = WDI), #factor(date_time)),
  arrow = arrow(length = unit(0.5, "cm"))) +
  #geom_point(aes(date_time, 0), size = 1) +
  coord_fixed(36000) +
  theme(legend.position = "none")




library(tidyverse)
library(scales)

ggplot()+#data = buz_wind_df, aes(x = date_time, y = WSP)) +
  # Here we create the wind vectors as a series of segments with arrow tips
  geom_segment(data = buz_wind_df, aes(x = date_time, xend = date_time, 
                   y = 180, yend = WDI), 
               arrow = arrow(length = buz_wind_df$WSP), size = 0.5, alpha = 0.7) +
               #arrow = arrow(length = unit(0.15, 'cm')), size = 0.5, alpha = 0.7) +
  # Change the x and y axis labels
  labs(x = NULL, y = 'Wind Speed (m/s)') +
  coord_equal(ylim = c(min(buz_wind_df$WSP/buz_wind_df$WDI), 
                       max(buz_wind_df$WSP/buz_wind_df$WDI))) +
  theme(axis.text.x = element_text(angle = 20, hjust = 0.4, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8))


#### wave data
# wave height (m), period (sec), water temp (deg C)
station44097_nov2024 <- read_excel("Downloads/station44097_nov2024.xlsx", skip = 1)
station44097_dec2024 <- read_excel("Downloads/station44097_dec2024.xlsx", skip = 1)
station44097_jan2025 <- read_excel("Downloads/station44097_jan2025.xlsx")
#station44097_feb2025 <- read_excel("Downloads/station44097_feb2025.xlsx", skip = 1)
#station44097_mar2025 <- read_excel("Downloads/station44097_mar2025.xlsx", skip = 1)

s44097_wave_df = rbind(station44097_nov2024, station44097_dec2024, station44097_jan2025)#,
                       #station44097_feb2025, station44097_mar2025)
names(s44097_wave_df) = c("year","month","day","hour","min",
                          "WDIR","WSPD","GST","WVHT","WPD","APD","MWD",
                          "PRES","ATMP","WTMP","DEWP","VIS","TIDE")
s44097_wave_df$date_time =  make_datetime(year = s44097_wave_df$year,
                                          month = s44097_wave_df$month,
                                          day = s44097_wave_df$day,
                                          hour = s44097_wave_df$hour,
                                          min = s44097_wave_df$min)

buz44085_nov2024 <- read_excel("Downloads/buz44085_nov2024.xlsx", skip = 1)
buz44085_dec2024 <- read_excel("Downloads/buz44085_dec2024.xlsx", skip = 1)
buz44085_jan2025 <- read_excel("Downloads/buz44085_jan2025.xlsx")
#buz44085_feb2025 <- read_excel("Downloads/buz44085_feb2025.xlsx", skip = 1)
#buz44085_mar2025 <- read_excel("Downloads/buz44085_mar2025.xlsx", skip = 1)

buz44085_wave_df = rbind(buz44085_nov2024, buz44085_dec2024, buz44085_jan2025)#, 
                         #buz44085_feb2025, buz44085_mar2025)
names(buz44085_wave_df) = c("year","month","day","hour","min",
                            "WDIR","WSPD","GST","WVHT","WPD","APD","MWD",
                            "PRES","ATMP","WTMP","DEWP","VIS","TIDE")
buz44085_wave_df$date_time =  make_datetime(year = buz44085_wave_df$year,
                                            month = buz44085_wave_df$month,
                                            day = buz44085_wave_df$day,
                                            hour = buz44085_wave_df$hour,
                                            min = buz44085_wave_df$min)

buzm3_wave_nov2024 <- read_excel("Downloads/buzm3_wave_nov2024.xlsx", skip = 1)
buzm3_wave_dec2024 <- read_excel("Downloads/buzm3_wave_dec2024.xlsx", skip = 1)
buzm3_wave_jan2025 <- read_excel("Downloads/buzm3_wave_jan2025.xlsx", skip = 1)
#buzm3_wave_feb2025 <- read_excel("Downloads/buzm3_wave_feb2025.xlsx", skip = 1)
#buzm3_wave_mar2025 <- read_excel("Downloads/buzm3_wave_mar2025.xlsx", skip = 1)

names(buzm3_wave_jan2025) = names(buzm3_wave_dec2024) #slightly different
buzm3_wave_df = rbind(buzm3_wave_nov2024, buzm3_wave_dec2024, buzm3_wave_jan2025)#, 
                      #buzm3_wave_feb2025, buzm3_wave_mar2025)
names(buzm3_wave_df) = c("year","month","day","hour","min",
                            "WDIR","WSPD","GST","WVHT","WPD","APD","MWD",
                            "PRES","ATMP","WTMP","DEWP","VIS","TIDE")
buzm3_wave_df$date_time =  make_datetime(year = buzm3_wave_df$year,
                                         month = buzm3_wave_df$month,
                                         day = buzm3_wave_df$day,
                                         hour = buzm3_wave_df$hour,
                                         min = buzm3_wave_df$min)

#ggplot()+  geom_line(data = buzm3_wave_df, aes(x = date_time, y = WTMP))

### Currents
# depth m (all 1 m), direction (deg), speed (cm/s)
buz44085_currents_nov2024 <- read_excel("Downloads/buz44085_currents_nov2024.xlsx", skip = 1)
buz44085_currents_dec2024 <- read_excel("Downloads/buz44085_currents_dec2024.xlsx", skip = 1)
buz44085_currents_jan2025 <- read_excel("Downloads/buz44085_currents_jan2025.xlsx", skip = 1)
#buz44085_currents_feb2025 <- read_excel("Downloads/buz44085_currents_feb2025.xlsx", skip = 1)
#buz44085_currents_mar2025 <- read_excel("Downloads/buz44085_currents_mar2025.xlsx", skip = 1)

buz44085_currents_df = rbind(buz44085_currents_nov2024, buz44085_currents_dec2024, buz44085_currents_jan2025)#,
                             #buz44085_currents_feb2025, buz44085_currents_mar2025)
names(buz44085_currents_df) = c("year","month","day","hour","min","DEP","DIR","SPD")
buz44085_currents_df$date_time =  make_datetime(year = buz44085_currents_df$year,
                                                month = buz44085_currents_df$month,
                                                day = buz44085_currents_df$day,
                                                hour = buz44085_currents_df$hour,
                                                min = buz44085_currents_df$min)

buz44085_currents_df = filter(buz44085_currents_df, 
                              !SPD %in% 999, !DIR %in% 999)

ggplot()+  geom_line(data = buz44085_currents_df, aes(x = date_time, y = SPD))
ggplot()+  geom_line(data = buz44085_currents_df, aes(x = date_time, y = DIR))

ggplot(data = buz44085_currents_df, aes(x = date_time, y = 0)) +  
  geom_segment(aes(xend = date_time + 1, 
                   yend = SPD * sin(DIR * pi/180), 
                   color = SPD),
               arrow = arrow(length = unit(0.2, "cm"))) +
  geom_point(aes(y = 0, color = SPD), shape = 1) +
  #coord_equal() +
  scale_color_continuous(name = "Speed") +
  labs(x = "Time") +
  theme_bw()
# -------------------------- #
