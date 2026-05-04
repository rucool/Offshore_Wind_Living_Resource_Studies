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
# mission 1: ru34-20251103T1347
# mission 2: unit_1190-20251209T1402
# mission 3: unit_1190-20260121T1322
# mission 4: unit_1190-20260307T1315

# mission 1: ru34-20251103T1347
m1 = read_csv("~/Downloads/ru34-20251103T1347-profile-sci-delayed_21ab_c844_d444.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m1 = m1[-1,]
m1$date_time <- as.POSIXct(gsub("T", "", m1$time), tz="UTC") # define as UTC
m1 = unique(m1) # remove dups

# mission 2: unit_1190-20251209T1402
m2 = read_csv("~/Downloads/unit_1190-20251209T1402-profile-sci-delayed_e81d_ae7b_c4fc.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m2 = m2[-1,]
m2$date_time <- as.POSIXct(gsub("T", "", m2$time), tz="UTC") # define as UTC
m2 = unique(m2) # remove dups

# mission 3: unit_1190-20260121T1322
m3 = read_csv("~/Downloads/unit_1190-20260121T1322-profile-sci-delayed_f526_6616_6eef.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m3 = m3[-1,]
m3$date_time <- as.POSIXct(gsub("T", "", m3$time), tz="UTC") # define as UTC
m3 = unique(m3) # remove dups

# mission 4: unit_1190-20260307T1315 STILL NEED DELAYED MODE DATA
m4 = read_csv("~/Downloads/unit_1190-20260307T1315-profile-sci-delayed_d366_c1fe_c371.csv") #unit_1190-20260307T1315-profile-sci-rt_2b9b_c61d_192f.csv")   # ru34-20241102T1737-profile-sci-delayed_9cd0_d462_d3b3.csv", skip=1)
m4 = m4[-1,]
m4$date_time <- as.POSIXct(gsub("T", "", m4$time), tz="UTC") # define as UTC
m4 = unique(m4) # remove dups

#library(gtools)
#do.call(smartbind,l)
whole_mission = bind_rows((m1 %>% dplyr::select(date_time, latitude, longitude, 
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

min(whole_mission$temperature, na.rm=T)
max(whole_mission$temperature, na.rm=T)
mean(whole_mission$temperature, na.rm=T)
sd(whole_mission$temperature, na.rm=T)

min(whole_mission$salinity, na.rm=T)
max(whole_mission$salinity, na.rm=T)
mean(whole_mission$salinity, na.rm=T)
sd(whole_mission$salinity, na.rm=T)

min(as.numeric(m1$chlorophyll_a), na.rm=T)
max(as.numeric(m1$chlorophyll_a), na.rm=T)
mean(as.numeric(m1$chlorophyll_a), na.rm=T)
sd(as.numeric(m1$chlorophyll_a), na.rm=T)

min(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
max(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
mean(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)
sd(as.numeric(m1$oxygen_concentration_shifted_mgL), na.rm=T)

# ----------------- #

# ----------------- #
# plots
# ----------------- #
# TEMP
all_temp = ggplot() + 
  annotate("rect", xmin = ymd("2025-12-22"), xmax = ymd("2026-01-12"), 
           ymin = -Inf, ymax = Inf, alpha = 1, fill = "grey")+
  annotate("rect", xmin = ymd("2026-01-12"), xmax = ymd("2026-02-02"), 
           ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  geom_point(data = whole_mission %>% filter(!is.na(temperature)), 
             aes(x = date_time, y = -depth, col = temperature)) + 
  scale_colour_cmocean(name = "thermal")+
  labs(x = "Date", y = "Depth (m)", col = "Temp. (C)", title = "Temperature")+ 
  theme_bw() + 
  theme(text = element_text(size= 20), legend.position = "bottom") +
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5)) +
  xlim(ymd("2025-11-03"), ymd("2026-03-29"))

#geom_vline(xintercept = as.Date("2025-12-22"), color = "grey", linetype = "dashed") + 
  #geom_vline(xintercept = as.Date("2026-01-12"), color = "grey", linetype = "dashed") 
#12/22/25 - 1/12/26

# SAL
all_sal = ggplot() + 
  annotate("rect", xmin = ymd("2025-12-22"), xmax = ymd("2026-01-12"), 
           ymin = -Inf, ymax = Inf, alpha = 1, fill = "grey")+
  annotate("rect", xmin = ymd("2026-01-12"), xmax = ymd("2026-02-02"), 
           ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  geom_point(data = whole_mission %>% filter(!is.na(salinity)), 
             aes(x = date_time, y = -depth, col = salinity)) + 
  scale_colour_cmocean(name = "haline")+
  labs(x = "Date", y = "Depth (m)", col = "Salinity (ppt)", title = "Salinity")+ 
  theme_bw() + theme(text = element_text(size= 20), legend.position = "bottom")+
  guides(col = guide_colorbar(barwidth = 20, barheight = 0.5)) +
  xlim(ymd("2025-11-03"), ymd("2026-03-29"))

m1_oxy = ggplot() + 
  annotate("rect", xmin = ymd("2025-12-22"), xmax = ymd("2026-01-12"), 
           ymin = -Inf, ymax = Inf, alpha = 1, fill = "grey")+
  annotate("rect", xmin = ymd("2026-01-12"), xmax = ymd("2026-02-02"), 
           ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  geom_point(data = m1 %>% 
                filter(!is.na(as.numeric(oxygen_concentration_shifted_mgL))) %>%
                mutate(depth=as.numeric(depth), 
                       oxygen_concentration_shifted_mgL=as.numeric(oxygen_concentration_shifted_mgL)), 
              aes(x = date_time, y = -depth, col = oxygen_concentration_shifted_mgL)) + 
  scale_colour_cmocean(name = "deep")+
  labs(x = "Date", y = "Depth (m)", col = "Oxygen (mg/L)", title = "Oxygen")+ 
  theme_bw() + 
  theme(text = element_text(size= 20), legend.position="bottom") +
  xlim(ymd("2025-11-03"), ymd("2026-03-29"))
  

m1_chl = ggplot() + 
  annotate("rect", xmin = ymd("2025-12-22"), xmax = ymd("2026-01-12"), 
           ymin = -Inf, ymax = Inf, alpha = 1, fill = "grey")+
  annotate("rect", xmin = ymd("2026-01-12"), xmax = ymd("2026-02-02"), 
           ymin = -Inf, ymax = Inf, alpha = 0.5, fill = "grey")+
  geom_point(data = m1 %>% filter(!is.na(as.numeric(chlorophyll_a))) %>% 
               mutate(depth=as.numeric(depth), 
                      chlorophyll_a=as.numeric(chlorophyll_a)), 
             aes(x = date_time, y = -depth, col = chlorophyll_a)) + 
  scale_colour_cmocean(name = "algae")+
  labs(x = "Date", y = "Depth (m)", col = "Chlorophyll", title = "Chlorophyll a")+ 
  theme_bw() +
  theme(text = element_text(size= 20), legend.position="bottom")  +
  xlim(ymd("2025-11-03"), ymd("2026-03-29"))

# combo plots
ggarrange(all_temp, all_sal, m1_oxy, m1_chl,
          labels = c("A.", "B.","C.","D."),
          ncol = 1, nrow = 4) %>%
  ggexport(width = 1000, height = 1200, 
           filename = "cod2526_temp_sal.tiff")
# -------------- #
