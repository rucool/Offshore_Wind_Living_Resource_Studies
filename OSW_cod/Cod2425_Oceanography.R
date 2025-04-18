# ----------------- #
# load packages
# ----------------- #
require(ggplot2)
require(lubridate)
require(readr)
require(cmocean)
require(ggpubr)
require(dplyr)
# ----------------- #

# ----------------- #
# load data
# ----------------- #
# mission 1: ru34-20241102T1737
# mission 2: unit_1190-20241218T1433
# mission 3: ru34-20250113T1244
# mission 4: unit_1190-20250224T1405
# mission 5: ru34-20250311T1220

# mission 1: ru34-20241102T1737
ru34_sci_nov24 = read_csv("~/Downloads/ru34-20241102T1737-profile-sci-delayed_0430_fc1f_8714.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
ru34_sci_nov24 = ru34_sci_nov24[-1,]
ru34_sci_nov24$date_time <- as.POSIXct(gsub("T", "", ru34_sci_nov24$time), tz="UTC") # define as UTC
ru34_sci_nov24 = unique(ru34_sci_nov24) # remove dups

# mission 2: unit_1190-20241218T1433
unit1190_sci_dec24 = read_csv("~/Downloads/unit_1190-20241218T1433-profile-sci-delayed_a678_d246_5ce3.csv") # unit_1190-20241218T1433-profile-sci-delayed_5874_356b_e4af.csv", skip=1)
unit1190_sci_dec24 = unit1190_sci_dec24[-1,]
unit1190_sci_dec24$date_time <- as.POSIXct(gsub("T", "", unit1190_sci_dec24$time), tz="UTC") # define as UTC
unit1190_sci_dec24 = unique(unit1190_sci_dec24) # remove dups
unit1190_sci_dec24 = mutate(unit1190_sci_dec24)

# mission 3: ru34-20250113T1244
ru34_sci_jan25 = read_csv("~/Downloads/ru34-20250113T1244-profile-sci-delayed_2ede_d73e_1ea5.csv") # ru34-20250113T1244-profile-sci-rt_3c58_fe07_83d5.csv", skip=1)
ru34_sci_jan25 = ru34_sci_jan25[-1,]
ru34_sci_jan25$date_time <- as.POSIXct(gsub("T", "", ru34_sci_jan25$time), tz="UTC") # define as UTC
ru34_sci_jan25 = unique(ru34_sci_jan25) # remove dups

# mission 4: unit_1190-20250224T1405
unit1190_sci_feb25 = read_csv("~/Downloads/unit_1190-20250224T1405-profile-sci-delayed_f045_2bfc_4ccc.csv")# unit_1190-20250224T1405-profile-sci-rt_d43a_0c32_acb2.csv", skip=1)
unit1190_sci_feb25 = unit1190_sci_feb25[-1,]
unit1190_sci_feb25$date_time <- as.POSIXct(gsub("T", "", unit1190_sci_feb25$time), tz="UTC") # define as UTC
unit1190_sci_feb25 = unique(unit1190_sci_feb25) # remove dups
unit1190_sci_feb25 = mutate(unit1190_sci_feb25, 
                            salinity = ifelse(salinity > 100, NaN, salinity))

# mission 5: ru34-20250311T1220
ru34_sci_mar25 = read_csv("~/Downloads/ru34-20250311T1220-profile-sci-rt_d46d_11e7_ca79.csv", skip=1)
names(ru34_sci_mar25) = c("time","latitude","longitude","depth","trajectory","profile_id",  
                          "profile_lat","profile_lon","source_file", "platform","instrument_ctd",
                          "instrument_hydrophone","instrument_optode","instrument_rxlive",
                          "beta_700nm_reference","beta_700nm_signal","cdom","chlorophyll_a",
                          "chlorophyll_a_qartod_gross_range_test","chlorophyll_a_qartod_summary_flag",
                          "conductivity","conductivity_hysteresis_test",
                          "conductivity_qartod_climatology_test","conductivity_qartod_flat_line_test",
                          "conductivity_qartod_gross_range_test","conductivity_qartod_rate_of_change_test",
                          "conductivity_qartod_spike_test","conductivity_qartod_summary_flag","crs", 
                          "ctd_timestamp","density","density_qartod_climatology_test",
                          "density_qartod_flat_line_test","density_qartod_rate_of_change_test",
                          "density_qartod_spike_test","density_qartod_summary_flag","depth_interpolated",
                          "m_pitch","m_roll","m_science_clothesline_lag","optode_water_temperature",
                          "oxygen_concentration","oxygen_concentration_optimal_shift",
                          "oxygen_concentration_qartod_flat_line_test",
                          "oxygen_concentration_qartod_gross_range_test",
                          "oxygen_concentration_qartod_summary_flag","oxygen_concentration_shifted",
                          "oxygen_concentration_shifted_mgL","oxygen_saturation",
                          "oxygen_saturation_optimal_shift",
                          "oxygen_saturation_qartod_flat_line_test",
                          "oxygen_saturation_qartod_gross_range_test",
                          "oxygen_saturation_qartod_summary_flag","oxygen_saturation_shifted",
                          "potential_temperature","pressure","pressure_qartod_flat_line_test",              
                          "pressure_qartod_gross_range_test","pressure_qartod_pressure_test",
                          "pressure_qartod_rate_of_change_test","pressure_qartod_spike_test",                  
                          "pressure_qartod_summary_flag","profile_time","salinity",
                          "salinity_qartod_climatology_test","salinity_qartod_flat_line_test",
                          "salinity_qartod_rate_of_change_test","salinity_qartod_spike_test",
                          "salinity_qartod_summary_flag","sci_m_present_time",
                          "sci_oxy3835_wphase_dphase","sci_water_pressure","sound_speed",
                          "temperature","temperature_hysteresis_test",
                          "temperature_qartod_climatology_test","temperature_qartod_flat_line_test",
                          "temperature_qartod_gross_range_test","temperature_qartod_rate_of_change_test",
                          "temperature_qartod_spike_test","temperature_qartod_summary_flag",
                          "u","v","water_depth")
ru34_sci_mar25$date_time <- as.POSIXct(ru34_sci_mar25$time, tz="UTC") # define as UTC
ru34_sci_mar25 = unique(ru34_sci_mar25) # remove dups
ru34_sci_mar25 = mutate(ru34_sci_mar25, 
                        time = as.character(time), 
                        latitude = as.character(latitude),
                        longitude = as.character(longitude),
                        depth = as.character(depth),
                        profile_lat = as.character(profile_lat),
                        profile_lon = as.character(profile_lon),
                        beta_700nm_reference = as.character(beta_700nm_reference),
                        beta_700nm_signal = as.character(beta_700nm_signal),
                        cdom = as.character(cdom),
                        chlorophyll_a = as.character(chlorophyll_a),
                        conductivity = as.character(conductivity),
                        density = as.character(density),
                        depth_interpolated = as.character(depth_interpolated),
                        m_pitch= as.character(m_pitch),
                        m_roll= as.character(m_roll),
                        m_science_clothesline_lag = as.character(m_science_clothesline_lag),
                        optode_water_temperature = as.character(optode_water_temperature),
                        oxygen_concentration = as.character(oxygen_concentration),
                        oxygen_concentration_optimal_shift = as.character(oxygen_concentration_optimal_shift),
                        oxygen_concentration_shifted = as.character(oxygen_concentration_shifted),
                        oxygen_concentration_shifted_mgL = as.character(oxygen_concentration_shifted_mgL),
                        oxygen_saturation = as.character(oxygen_saturation),
                        oxygen_saturation_optimal_shift = as.character(oxygen_saturation_optimal_shift),
                        oxygen_saturation_shifted = as.character(oxygen_saturation_shifted),
                        potential_temperature = as.character(potential_temperature),
                        pressure = as.character(pressure),
                        profile_time = as.character(profile_time),
                        sci_m_present_time = as.character(sci_m_present_time),
                        sci_oxy3835_wphase_dphase = as.character(sci_oxy3835_wphase_dphase),
                        sci_water_pressure = as.character(sci_water_pressure),
                        sound_speed = as.character(sound_speed),
                        temperature = as.character(temperature),
                        u=as.character(u),
                        v=as.character(v),
                        water_depth = as.character(water_depth))

#library(gtools)
#do.call(smartbind,l)
whole_mission = bind_rows(ru34_sci_nov24, unit1190_sci_dec24, ru34_sci_jan25,
                          unit1190_sci_feb25, ru34_sci_mar25) %>% 
  mutate(temperature = as.numeric(temperature),
         salinity = as.numeric(salinity),
         chlorophyll_a = as.numeric(chlorophyll_a),
         oxygen_concentration_shifted_mgL = as.numeric(oxygen_concentration_shifted_mgL),
         depth = as.numeric(depth))

min(whole_mission$temperature, na.rm=T)
max(whole_mission$temperature, na.rm=T)
mean(whole_mission$temperature, na.rm=T)
sd(whole_mission$temperature, na.rm=T)

min(whole_mission$salinity, na.rm=T)
max(whole_mission$salinity, na.rm=T)
mean(whole_mission$salinity, na.rm=T)
sd(whole_mission$salinity, na.rm=T)

min(whole_mission$chlorophyll_a, na.rm=T)
max(whole_mission$chlorophyll_a, na.rm=T)
mean(whole_mission$chlorophyll_a, na.rm=T)
sd(whole_mission$chlorophyll_a, na.rm=T)

min(whole_mission$oxygen_concentration_shifted_mgL, na.rm=T)
max(whole_mission$oxygen_concentration_shifted_mgL, na.rm=T)
mean(whole_mission$oxygen_concentration_shifted_mgL, na.rm=T)
sd(whole_mission$oxygen_concentration_shifted_mgL, na.rm=T)

# ----------------- #

# ----------------- #
# plots
# ----------------- #
# TEMP
all_temp = ggplot() + 
  geom_point(data = whole_mission, aes(x = date_time, y = -depth, col = temperature)) + 
  scale_colour_cmocean(name = "thermal")+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)", title = "Temperature")+ 
  theme_bw() + theme(text = element_text(size= 20), legend.position = "bottom") +
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5))

# m1_temp = ggplot() + 
#   geom_point(data =  ru34_sci_nov24, 
#              aes(x = date_time, y = -as.numeric(depth), col = as.numeric(temperature))) + 
#   scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
#   labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
#   theme_bw()
# 
# m2_temp = ggplot() + 
#   geom_point(data = unit1190_sci_dec24, 
#              aes(x = date_time, y = -as.numeric(depth), col = as.numeric(temperature))) + 
#   scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
#   labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
#   theme_bw()
# 
# m3_temp = ggplot() + 
#   geom_point(data = ru34_sci_jan25, aes(x = date_time, y = -depth, col = temp)) + 
#   scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
#   labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
#   theme_bw()
# 
# m4_temp = ggplot() + 
#   geom_point(data = unit1190_sci_feb25, aes(x = date_time, y = -depth, col = temp)) + 
#   scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
#   labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
#   theme_bw()
# 
# m5_temp = ggplot() + 
#   geom_point(data = ru34_sci_mar25 %>% filter(!is.na(temp)), aes(x = date_time, y = -depth, col = temp)) + 
#   scale_colour_cmocean(name = "thermal", limits = c(3.5,18.5))+
#   labs(x = "Date", y = "Depth (m)", col = "Temp. (C)")+ 
#   theme_bw()

# SAL
all_sal = ggplot() + 
  geom_point(data = whole_mission%>% filter(!is.na(salinity)), 
             aes(x = date_time, y = -depth, col = salinity)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity (ppt)", title = "Salinity")+ 
  theme_bw() + theme(text = element_text(size= 20), legend.position = "bottom")+
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5)) 

# m1_sal = ggplot() + 
#   geom_point(data = ru34_sci_nov24, aes(x = date_time, y = -depth, col = sal)) + 
#   scale_colour_cmocean(name = "haline")+
#   labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
#   theme_bw()
# 
# m2_sal = ggplot() + 
#   geom_point(data = unit1190_sci_dec24, aes(x = date_time, y = -depth, col = sal)) + 
#   scale_colour_cmocean(name = "haline")+
#   labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
#   theme_bw()
# 
# m3_sal = ggplot() + 
#   geom_point(data = ru34_sci_jan25, aes(x = date_time, y = -depth, col = sal)) + 
#   scale_colour_cmocean(name = "haline")+
#   labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
#   theme_bw()
# 
# m4_sal = ggplot() + 
#   geom_point(data = unit1190_sci_feb25 %>% filter(!is.na(sal)), aes(x = date_time, y = -depth, col = sal)) + 
#   scale_colour_cmocean(name = "haline")+
#   labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
#   theme_bw()
# 
# m5_sal = ggplot() + 
#   geom_point(data = ru34_sci_mar25 %>% filter(!is.na(sal)), aes(x = date_time, y = -depth, col = sal)) + 
#   scale_colour_cmocean(name = "haline")+
#   labs(x = "Date", y = "Depth (m)", col = "Salinity")+ 
#   theme_bw()

# OXY
all_oxy = ggplot() + 
  geom_point(data = whole_mission %>% filter(!is.na(oxygen_concentration_shifted_mgL)), 
             aes(x = date_time, y = -depth, col =oxygen_concentration_shifted_mgL)) + 
  scale_colour_cmocean(name = "deep")+
  xlim(min(whole_mission$date_time), max(whole_mission$date_time)) + 
  labs(x = "Date", y = "Depth (m)", col = "Oxygen (mg/L)", title = "Oxygen")+ 
  theme_bw() + theme(text = element_text(size= 20), legend.position = "bottom") +
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5))
# 
# m1_oxy = ggplot() + 
#   geom_point(data = ru34_sci_nov24 %>% filter(!is.na(oxy)), aes(x = date_time, y = -depth, col = oxy)) + 
#   scale_colour_cmocean(name = "deep")+
#   labs(x = "Date", y = "Depth (m)", col = "Oxygen")+ 
#   theme_bw()+ 
#   theme(legend.position="bottom") 
# 
# m5_oxy = ggplot() + 
#   geom_point(data = ru34_sci_mar25 %>% filter(!is.na(oxy)), aes(x = date_time, y = -depth, col = oxy)) + 
#   scale_colour_cmocean(name = "deep")+
#   labs(x = "Date", y = "Depth (m)", col = "Oxygen")+ 
#   theme_bw()+ 
#   theme(legend.position="bottom") 

# Chl
all_chl = ggplot() + 
  geom_point(data = whole_mission %>% filter(!is.na(chlorophyll_a)), 
             aes(x = date_time, y = -depth, col = chlorophyll_a)) + 
  scale_colour_cmocean(name = "algae")+
  xlim(min(whole_mission$date_time), max(whole_mission$date_time)) + 
  labs(x = "Date", y = "Depth (m)", col = "Chl (ug/L)", title = "Chlorophyll a")+ 
  theme_bw() + theme(text = element_text(size= 20), legend.position = "bottom") +
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5))

# m1_chl = ggplot() + 
#   geom_point(data = ru34_sci_nov24 %>% filter(!is.na(chl)), aes(x = date_time, y = -depth, col = chl)) + 
#   scale_colour_cmocean(name = "algae")+
#   labs(x = "Date", y = "Depth (m)", col = "Chl")+ 
#   theme_bw()
# 
# m5_chl = ggplot() + 
#   geom_point(data = ru34_sci_mar25 %>% filter(!is.na(chl)), aes(x = date_time, y = -depth, col = chl)) + 
#   scale_colour_cmocean(name = "algae")+
#   labs(x = "Date", y = "Depth (m)", col = "Chl")+ 
#   theme_bw()

# combo plots
ggarrange(all_temp, all_sal, all_chl, all_oxy,
          labels = c("A.", "B.","C.","D."),
          ncol = 1, nrow = 4) %>%
  ggexport(width = 1000, height = 1200, filename = "cod2425_temp_sal.tiff")
# 
# ggarrange(m1_temp, m2_temp, m3_temp, m4_temp, m5_temp,
#           labels = c("Mission 1", "Mission 2", "Mission 3", "Mission 4"),
#           ncol = 1, nrow = 4, common.legend = TRUE, legend="right") %>%#, legend="bottom") %>%
#   ggexport(width = 800, height = 800, filename = "cod2425_temp.tiff")
# 
# ggarrange(m1_sal, m2_sal, m3_sal, m4_sal,  m5_sal,
#           labels = c("Mission 1", "Mission 2", "Mission 3", "Mission 4"),
#           ncol = 1, nrow = 4, common.legend = TRUE, legend="right") %>%#, legend="bottom") %>%
#   ggexport(width = 800, height = 800, filename = "cod2425_sal.tiff")
# 
# ggarrange(m1_oxy, m3_oxy, m5_oxy,
#           labels = c("Mission 1", "Mission 3"),
#           ncol = 1, nrow = 2)
# 
# ggarrange(m1_chl, m3_chl, m5_chl,
#           labels = c("Mission 1", "Mission 3"),
#           ncol = 1, nrow = 2)
# -------------- #

# -------------- #
# match to tag data
# run after REV_SRW_tags_2025.R
# -------------- #
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

ru34_m1_tags$match_time = find_nearest_date(ru34_m1_tags$date_time, whole_mission$date_time)
ru34_m1_tags2 = left_join(ru34_m1_tags, whole_mission, by =c("match_time"="date_time"))

u1190_m2_tags$match_time = find_nearest_date(u1190_m2_tags$datecollected, whole_mission$date_time)
u1190_m2_tags2 = left_join(u1190_m2_tags, whole_mission, by =c("match_time"="date_time"))

ru34_m3_tags$match_time = find_nearest_date(ru34_m3_tags$date_time, whole_mission$date_time)
ru34_m3_tags2 = left_join(ru34_m3_tags, whole_mission, by =c("match_time"="date_time"))

u1190_m4_tags$match_time = find_nearest_date(u1190_m4_tags$date_time, whole_mission$date_time)
u1190_m4_tags2 = left_join(u1190_m4_tags, whole_mission, by =c("match_time"="date_time"))

ru34_m5_tags$match_time = find_nearest_date(ru34_m5_tags$date_time, whole_mission$date_time)
ru34_m5_tags2 = left_join(ru34_m5_tags, whole_mission, by =c("match_time"="date_time"))

tags = bind_rows(ru34_m1_tags2, u1190_m2_tags2, ru34_m3_tags2, u1190_m4_tags2, ru34_m5_tags2)
min(tags$temperature, na.rm=T)
max(tags$temperature, na.rm=T)
mean(tags$temperature, na.rm=T)
sd(tags$temperature, na.rm=T)

min(tags$salinity, na.rm=T)
max(tags$salinity, na.rm=T)
mean(tags$salinity, na.rm=T)
sd(tags$salinity, na.rm=T)

min(tags$chlorophyll_a, na.rm=T)
max(tags$chlorophyll_a, na.rm=T)
mean(tags$chlorophyll_a, na.rm=T)
sd(tags$chlorophyll_a, na.rm=T)

min(tags$oxygen_concentration_shifted_mgL, na.rm=T)
max(tags$oxygen_concentration_shifted_mgL, na.rm=T)
mean(tags$oxygen_concentration_shifted_mgL, na.rm=T)
sd(tags$oxygen_concentration_shifted_mgL, na.rm=T)
# -------------- #

