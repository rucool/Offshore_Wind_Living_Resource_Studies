# https://www.ndbc.noaa.gov/station_history.php?station=buzm3

# laod packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# load data
buzm3h2023 <- read_table("~/Downloads/buzm3h2023.txt", skip = 1)
buzm3h2024 <- read_table("Downloads/buzm3h2024.txt", skip = 1)
buzm3h2025 <- read_table("Downloads/buzm3h2025.txt", skip = 1)
buzm3h12026 <- read_table("Downloads/buzm312026.txt", skip = 1)
buzm3h22026 <- read_table("Downloads/buzm322026.txt", skip = 1)
buzm3h32026 <- read_table("Downloads/buz3Mar26.rtf", skip = 10)

wspd = rbind(buzm3h2025[,1:7], buzm3h2024[,1:7], 
             buzm3h12026[,1:7], buzm3h22026[,1:7], 
             buzm3h32026[,1:7], buzm3h2023[,1:7]) 
wspd = mutate(wspd,
              date_time = as.POSIXct(paste(paste(`#yr`, mo, dy, sep="-"), 
                                           paste(hr, mn, sep=":"), sep = " "),
                                     format = "%Y-%m-%d %H:%M")) %>% 
  rename("wspd" = `m/s`,
         "wdir" = "degT") %>% 
  arrange(date_time)



season2223 = wspd %>% filter(date_time > as.Date("2023-03-02"), 
                             date_time < as.Date("2023-03-28"))
season2324 = wspd %>% filter(date_time > as.Date("2024-01-11"), 
                             date_time < as.Date("2024-04-10"))
season2425 = wspd %>% filter(date_time > as.Date("2024-11-01"), 
                             date_time < as.Date("2025-03-31"))
season2526 = wspd %>% filter(date_time > as.Date("2025-11-01"))

# p1 = ggplot() + 
#   geom_line(data = season2425, aes(x =date_time, y = wspd), color = "cornflowerblue") + 
#   theme_bw()+ labs(x = "Time", y = "Wind Speed (m/s)")
# p2 = ggplot() +
#   geom_line(data = season2526, aes(x = date_time, y = wspd), color = "navy")+ 
#   theme_bw() + labs(x = "Time", y = "Wind Speed (m/s)")
# ggarrange(p1, p2, ncol = 1) 


avg_wspd2223 = season2223 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
avg_wspd2324 = season2324 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
avg_wspd2425 = season2425 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
avg_wspd2526 = season2526 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
ggplot() +
  geom_line(data = avg_wspd2223, aes(x = `date(date_time)`+730, y = avwspd), color = "gold")+ 
  geom_line(data = avg_wspd2324, aes(x = `date(date_time)`+365, y = avwspd), color = "lightblue")+ 
  geom_line(data = avg_wspd2425, aes(x = `date(date_time)`, y = avwspd), color = "cornflowerblue")+ 
  geom_line(data = avg_wspd2526, aes(x = `date(date_time)`-365, y = avwspd), color = "navy")+ 
  theme_bw() + labs(x = "Time", y = "Avg. Wind Speed (m/s)")  
  
# Wind Rose
compass_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
degrees_per_sector <- 22.5

wind_to_compass <- function(degrees) {
  degrees <- degrees %% 360  # Ensure degrees are within 0-359 range
  sector_index <- floor((degrees / 22.5) + 0.5) + 1
  #sector_index <- floor((degrees + 11.25) / 22.5) + 1
  #sector_index <- ceiling(degrees / degrees_per_sector)
  # Handle edge case where sector_index is 17 (should be 1)
  if (sector_index == 17) {sector_index <- 1}
  if (sector_index == 0) {sector_index <- 1}
  return(compass_directions[sector_index])
}

season2223 = mutate(season2223, 
                    direction = sapply(wdir, wind_to_compass),
                    direction = factor(direction, levels = compass_directions),
                    wspdbin = cut(wspd, breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27,30), 
                                  labels=c("0-2","3-5","6-8","9-11","12-14","15-17",
                                           "18-20","21-23","24-26","27-29"))) %>%
  filter(!is.na(wspdbin))
season2324 = mutate(season2324, 
                    direction = sapply(wdir, wind_to_compass),
                    direction = factor(direction, levels = compass_directions),
                    wspdbin = cut(wspd, breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27,30), 
                                  labels=c("0-2","3-5","6-8","9-11","12-14","15-17",
                                           "18-20","21-23","24-26","27-29"))) %>%
  filter(!is.na(wspdbin))
season2425 = mutate(season2425, 
                    direction = sapply(wdir, wind_to_compass),
                    direction = factor(direction, levels = compass_directions),
                    wspdbin = cut(wspd, breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27,30), 
                                  labels=c("0-2","3-5","6-8","9-11","12-14","15-17",
                                           "18-20","21-23","24-26","27-29")))%>%
  filter(!is.na(wspdbin))
#wspdbin = cut(wspd, breaks = c(0, 5, 10, 15, 20, 25, 30), 
                    #              labels=c("0-5","6-10","11-15","16-20","21-25","25-30")))
season2526 = mutate(season2526, 
                    direction = sapply(wdir, wind_to_compass),
                    direction = factor(direction, levels = compass_directions),
                    wspdbin = cut(wspd, breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27,30), 
                                  labels=c("0-2","3-5","6-8","9-11","12-14","15-17",
                                           "18-20","21-23","24-26","27-29")))%>%
  filter(!is.na(wspdbin))
# season2425$direction <- sapply(season2425$wdir, wind_to_compass)
# season2425$direction <- factor(season2425$direction, 
#                                levels = compass_directions)
# season2526$direction <- sapply(season2526$wdir, wind_to_compass)
# season2526$direction <- factor(season2526$direction, 
#                                levels = compass_directions)

#pals <- colorRampPalette(brewer.pal(9, "Blues"))(20)  # 20 colors
p1 = ggplot(data = season2223, aes(x = direction, fill = as.factor(wspdbin))) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Mar. 23')",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  #scale_y_continuous(limits = c(0, 500)) +
  scale_fill_brewer(palette = "Paired") 
p2 = ggplot(data = season2324, aes(x = direction, fill = as.factor(wspdbin))) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Jan. 24' - Mar. 24')",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  #scale_y_continuous(limits = c(0, 500)) +
  scale_fill_brewer(palette = "Paired") 
p3 = ggplot(data = season2425, aes(x = direction, fill = as.factor(wspdbin))) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Nov. 24' - Mar. 25')",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  scale_fill_brewer(palette = "Paired") 
p4 = ggplot(data = season2526, aes(x = direction, fill = as.factor(wspdbin))) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Nov. 25' - Mar. 26')",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  scale_fill_brewer(palette = "Paired") 
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
          

# compass_direction <- season2425 %>% 
#   rowwise() %>% 
#   mutate(heading = wind_to_compass(wdir))
# 
# sum_comp_dir = compass_direction %>% 
#   group_by(heading, wdir, round(wspd)) %>% 
#   summarise(n=n())
# 
# ggplot(data = sum_comp_dir, aes(x = heading, fill = `round(wspd)`)) +
#   geom_bar() +
#   coord_polar(start = -pi/16) + # -pi/16 aligns sectors properly
#   theme_minimal() +
#   labs(title = "Wind Direction Frequency",
#        x = "Direction",
#        y = "Frequency",
#        fill = "Speed (m/s)") +
#   scale_fill_viridis_c() # Adds nice color coding
# 
# 
# 
# 
# plot.windrose <- function(data, spd, dir, spdres = 2, dirres = 30, spdmin = 2, spdmax = 20, spdseq = NULL, palette = "YlGnBu", countmax = NA) {
#   if (is.numeric(spd) & is.numeric(dir)) {
#     data <- data.frame(spd = spd, dir = dir)
#     spd <- "spd"
#     dir <- "dir"
#   }
#   
#   n.in <- NROW(data)
#   dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
#   data[[spd]][dnu] <- NA
#   data[[dir]][dnu] <- NA
#   
#   if (missing(spdseq)) {
#     spdseq <- seq(spdmin, spdmax, spdres)
#   }
#   
#   n.spd.seq <- length(spdseq)
#   n.colors.in.range <- n.spd.seq - 1
#   spd.colors <- colorRampPalette(brewer.pal(min(max(3, n.colors.in.range), min(9, n.colors.in.range)), palette))(n.colors.in.range)
#   
#   if (max(data[[spd]], na.rm = TRUE) > spdmax) {
#     spd.breaks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
#     spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq])), paste(spdmax, "-", max(data[[spd]], na.rm = TRUE)))
#     spd.colors <- c(spd.colors, "grey50")
#   } else {
#     spd.breaks <- spdseq
#     spd.labels <- paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq]))
#   }
#   
#   data$spd.binned <- cut(x = data[[spd]], breaks = spd.breaks, labels = spd.labels, ordered_result = TRUE)
#   
#   dir.breaks <- c(-dirres/2, seq(dirres/2, 360-dirres/2, by = dirres), 360+dirres/2)
#   dir.labels <- c(paste(360-dirres/2,"-",dirres/2), paste(seq(dirres/2, 360-3*dirres/2, by = dirres), "-", seq(3*dirres/2, 360-dirres/2, by = dirres)), paste(360-dirres/2,"-",dirres/2))
#   dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
#   levels(dir.binned) <- dir.labels
#   data$dir.binned <- dir.binned
#   
#   p.windrose <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) +
#     geom_bar() +
#     scale_x_discrete(drop = FALSE) +
#     coord_polar(start = -((dirres/2)/360) * 2*pi) +
#     scale_fill_manual(name = "Wind Speed (m/s)", values = spd.colors, drop = FALSE) +
#     theme(axis.title.x = element_blank())
#   
#   if (!is.na(countmax)) {
#     p.windrose <- p.windrose + ylim(c(0, countmax))
#   }
#   
#   print(p.windrose)
#   return(p.windrose)
# }
# 
# 
# p1 = plot.windrose(data = season2324, spd = season2324$wspd, dir = season2324$wdir)
# p2 = plot.windrose(data = season2425, spd = season2425$wspd, dir = season2425$wdir)
# p3 = plot.windrose(data = season2526, spd = season2526$wspd, dir = season2526$wdir)
# ggarrange(p1, p2, p3, ncol = 3,
#              labels = c("Jan. 2024 - Mar. 2024","Nov. 2024 - Mar. 2025","Nov. 2025 - Mar. 2026"))
# 
