# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
require(readr)


# upload all data

# mod 1 mission 1 ru34-20230303T1521 
ru34_sci_mar23 = read_csv("~/Downloads/ru34-20230303T1521-profile-sci-delayed_0b9c_ba4d_28d6.csv") 
ru34_sci_mar23 = ru34_sci_mar23[-1,]
ru34_sci_mar23$date_time <- as.POSIXct(gsub("T", "", ru34_sci_mar23$time), tz="UTC") # define as UTC
ru34_sci_mar23 = unique(ru34_sci_mar23) # remove dups

# mod 2 mission 1 ru34-20240112T1722
ru34_sci_jan24 = read_csv("~/Downloads/ru34-20240112T1722-profile-sci-delayed_4d53_325b_da2c.csv") 
ru34_sci_jan24 = ru34_sci_jan24[-1,]
ru34_sci_jan24$date_time <- as.POSIXct(gsub("T", "", ru34_sci_jan24$time), tz="UTC") # define as UTC
ru34_sci_jan24 = unique(ru34_sci_jan24) # remove dups

# mod 2 mission 2 ru34-20240301T1336 
ru34_sci_mar24 = read_csv("~/Downloads/ru34-20240301T1336-profile-sci-delayed_d7f3_206c_620c.csv") 
ru34_sci_mar24 = ru34_sci_mar24[-1,]
ru34_sci_mar24$date_time <- as.POSIXct(gsub("T", "", ru34_sci_mar24$time), tz="UTC") # define as UTC
ru34_sci_mar24 = unique(ru34_sci_mar24) # remove dups

# mod3 mission 1: ru34-20241102T1737
ru34_sci_nov24 = read_csv("~/Downloads/ru34-20241102T1737-profile-sci-delayed_0430_fc1f_8714.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
ru34_sci_nov24 = ru34_sci_nov24[-1,]
ru34_sci_nov24$date_time <- as.POSIXct(gsub("T", "", ru34_sci_nov24$time), tz="UTC") # define as UTC
ru34_sci_nov24 = unique(ru34_sci_nov24) # remove dups

# mod3 mission 2: unit_1190-20241218T1433
unit1190_sci_dec24 = read_csv("~/Downloads/unit_1190-20241218T1433-profile-sci-delayed_a678_d246_5ce3.csv") # unit_1190-20241218T1433-profile-sci-delayed_5874_356b_e4af.csv", skip=1)
unit1190_sci_dec24 = unit1190_sci_dec24[-1,]
unit1190_sci_dec24$date_time <- as.POSIXct(gsub("T", "", unit1190_sci_dec24$time), tz="UTC") # define as UTC
unit1190_sci_dec24 = unique(unit1190_sci_dec24) # remove dups
unit1190_sci_dec24 = mutate(unit1190_sci_dec24)

# mod3 mission 3: ru34-20250113T1244
ru34_sci_jan25 = read_csv("~/Downloads/ru34-20250113T1244-profile-sci-delayed_2ede_d73e_1ea5.csv") # ru34-20250113T1244-profile-sci-rt_3c58_fe07_83d5.csv", skip=1)
ru34_sci_jan25 = ru34_sci_jan25[-1,]
ru34_sci_jan25$date_time <- as.POSIXct(gsub("T", "", ru34_sci_jan25$time), tz="UTC") # define as UTC
ru34_sci_jan25 = unique(ru34_sci_jan25) # remove dups

# mod3 mission 4: unit_1190-20250224T1405
unit1190_sci_feb25 = read_csv("~/Downloads/unit_1190-20250224T1405-profile-sci-delayed_f045_2bfc_4ccc.csv")# unit_1190-20250224T1405-profile-sci-rt_d43a_0c32_acb2.csv", skip=1)
unit1190_sci_feb25 = unit1190_sci_feb25[-1,]
unit1190_sci_feb25$date_time <- as.POSIXct(gsub("T", "", unit1190_sci_feb25$time), tz="UTC") # define as UTC
unit1190_sci_feb25 = unique(unit1190_sci_feb25) # remove dups
unit1190_sci_feb25 = mutate(unit1190_sci_feb25, 
                            salinity = ifelse(salinity > 100, NaN, salinity))

# mod3 mission 5: ru34-20250311T1220
ru34_sci_mar25 = read_csv("~/Downloads/ru34-20250311T1220-profile-sci-delayed_802d_fa9f_3fee.csv")
ru34_sci_mar25 = ru34_sci_mar25[-1,]
ru34_sci_mar25$date_time <- as.POSIXct(gsub("T", "", ru34_sci_mar25$time), tz="UTC") # define as UTC
ru34_sci_mar25 = unique(ru34_sci_mar25) # remove dups

# mod4 mission 1: ru34-20251103T1347
m1 = read_csv("~/Downloads/ru34-20251103T1347-profile-sci-delayed_21ab_c844_d444.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m1 = m1[-1,]
m1$date_time <- as.POSIXct(gsub("T", "", m1$time), tz="UTC") # define as UTC
m1 = unique(m1) # remove dups

# mod4 mission 2: unit_1190-20251209T1402
m2 = read_csv("~/Downloads/unit_1190-20251209T1402-profile-sci-delayed_e81d_ae7b_c4fc.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m2 = m2[-1,]
m2$date_time <- as.POSIXct(gsub("T", "", m2$time), tz="UTC") # define as UTC
m2 = unique(m2) # remove dups

# mod4 mission 3: unit_1190-20260121T1322
m3 = read_csv("~/Downloads/unit_1190-20260121T1322-profile-sci-delayed_f526_6616_6eef.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m3 = m3[-1,]
m3$date_time <- as.POSIXct(gsub("T", "", m3$time), tz="UTC") # define as UTC
m3 = unique(m3) # remove dups

# mod4 mission 4: unit_1190-20260307T1315 STILL NEED DELAYED MODE DATA
m4 = read_csv("~/Downloads/unit_1190-20260307T1315-profile-sci-delayed_d366_c1fe_c371.csv") #unit_1190-20260307T1315-profile-sci-rt_2b9b_c61d_192f.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m4 = m4[-1,]
m4$date_time <- as.POSIXct(gsub("T", "", m4$time), tz="UTC") # define as UTC
m4 = unique(m4) # remove dups

#library(gtools)
#do.call(smartbind,l)
whole_mission = bind_rows((ru34_sci_mar23 %>% dplyr::select(date_time, latitude, longitude, 
                                                           depth, temperature, salinity)),
                          (ru34_sci_jan24 %>% dplyr::select(date_time, latitude, longitude, 
                                                             depth, temperature, salinity)),
                          (ru34_sci_mar24 %>% dplyr::select(date_time, latitude, longitude, 
                                                           depth, temperature, salinity)),
                          (ru34_sci_nov24 %>% dplyr::select(date_time, latitude, longitude, 
                                                            depth, temperature, salinity)),
                          (unit1190_sci_dec24 %>% dplyr::select(date_time, latitude, longitude, 
                                                               depth, temperature, salinity)),
                          (ru34_sci_jan25 %>% dplyr::select(date_time, latitude, longitude, 
                                                           depth, temperature, salinity)),
                          (unit1190_sci_feb25 %>% dplyr::select(date_time, latitude, longitude, 
                                                                depth, temperature, salinity)),
                          (ru34_sci_mar25 %>% dplyr::select(date_time, latitude, longitude, 
                                                            depth, temperature, salinity)),
                          (m1 %>% dplyr::select(date_time, latitude, longitude, 
                                                depth, temperature, salinity)),
                          (m2 %>% dplyr::select(date_time, latitude, longitude, 
                                                depth, temperature, salinity)),
                          (m3 %>% dplyr::select(date_time, latitude, longitude, 
                                                depth, temperature, salinity)),
                          (m4 %>% dplyr::select(date_time, latitude, longitude, 
                                                depth, temperature, salinity))) %>% 
  mutate(temperature = as.numeric(temperature),
         salinity = as.numeric(salinity),
         depth = as.numeric(depth))



# stats
# min(whole_mission$temperature, na.rm=T)
# max(whole_mission$temperature, na.rm=T)
# mean(whole_mission$temperature, na.rm=T)
# sd(whole_mission$temperature, na.rm=T)
# 
# min(whole_mission$salinity, na.rm=T)
# max(whole_mission$salinity, na.rm=T)
# mean(whole_mission$salinity, na.rm=T)
# sd(whole_mission$salinity, na.rm=T)
# 
# min(as.numeric(m1$chlorophyll_a), na.rm=T)
# max(as.numeric(m1$chlorophyll_a), na.rm=T)
# mean(as.numeric(m1$chlorophyll_a), na.rm=T)
# sd(as.numeric(m1$chlorophyll_a), na.rm=T)
# 
# min(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
# max(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
# mean(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
# sd(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)



#plots
whole_mission = mutate(whole_mission, 
                       ord = NA,
                       ord = ifelse(month(date_time) %in% 10, 1, ord),
                       ord = ifelse(month(date_time) %in% 11, 2, ord),
                       ord = ifelse(month(date_time) %in% 12, 3, ord),
                       ord = ifelse(month(date_time) %in% 1, 4, ord),
                       ord = ifelse(month(date_time) %in% 2, 5, ord),
                       ord = ifelse(month(date_time) %in% 3, 6, ord),
                       ord = ifelse(month(date_time) %in% 4, 7, ord),
                       campaign = NA,
                       campaign = ifelse(date_time > as.Date("2025-06-30"),"2025-2026",campaign),
                       campaign = ifelse(date_time > as.Date("2024-06-30") & date_time < as.Date("2025-06-30") ,"2024-2025",campaign),
                       campaign = ifelse(date_time > as.Date("2023-06-30") & date_time < as.Date("2024-06-30") ,"2023-2024",campaign),
                       campaign = ifelse(date_time < as.Date("2023-06-30") ,"2022-2023",campaign))

cbPalette <- c("#CC79A7","#999999", "#E69F00", "#56B4E9")#), "#009E73")#, "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data=whole_mission, 
       aes(y=temperature, x=reorder(month(date_time),ord), 
           fill=campaign)) + 
         geom_boxplot() + 
  labs(fill="Campaign", y="Temperature (C)", x="Month") +
  theme_bw() +
  theme(text = element_text(size = 20)) + 
  scale_fill_manual(values=cbPalette)


ggplot(data=whole_mission, 
       aes(y=salinity, x=reorder(month(date_time),ord), 
           fill=campaign)) + 
  geom_boxplot() + 
  labs(fill="Campaign", y="Salinity", x="Month") +
  theme_bw() + 
  theme(text = element_text(size = 20)) + 
  ylim(29,34) +
  scale_fill_manual(values=cbPalette)


ggplot(data=whole_mission, 
       aes(y=temperature, x=salinity, col=campaign)) +
  geom_point() + 
  theme_bw() + 
  xlim(28,34)

# 
# ggplot(data=whole_mission, 
#        aes(y=temperature, x=ord), 
#            col=campaign)) + 
#   geom_point() + 
#   geom_smooth(method = 'lm') + 
#   labs(col="Campaign", y="Temperature (C)", x="Month") +
#   theme_bw()
# 
# gplot(data=whole_mission, 
#       aes(y=salinity, x=reorder(month(date_time), ord), 
#           col=campaign)) + 
#   geom_point() + 
#   geom_smooth(method = 'lm') + 
#   labs(col="Campaign", y="Salinity", x="Month") +
#   theme_bw()
