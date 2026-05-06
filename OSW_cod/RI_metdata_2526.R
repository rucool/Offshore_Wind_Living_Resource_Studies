# https://www.ndbc.noaa.gov/station_history.php?station=buzm3

# laod packages
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggpubr)

# load data
buzm3h2025 <- read_table("Downloads/buzm3h2025.txt", skip = 1)
buzm3h2024 <- read_table("Downloads/buzm3h2024.txt", skip = 1)
buzm3h12026 <- read_table("Downloads/buzm312026.txt", skip = 1)
buzm3h22026 <- read_table("Downloads/buzm322026.txt", skip = 1)
buzm3h32026 <- read_table("Downloads/buz3Mar26.rtf", skip = 10)

wspd = rbind(buzm3h2025[,1:7], buzm3h2024[,1:7], 
             buzm3h12026[,1:7], buzm3h22026[,1:7], 
             buzm3h32026[,1:7]) 
wspd = mutate(wspd,
              date_time = as.POSIXct(paste(paste(`#yr`, mo, dy, sep="-"), 
                                           paste(hr, mn, sep=":"), sep = " "),
                                     format = "%Y-%m-%d %H:%M")) %>% 
  rename("wspd" = `m/s`,
         "wdir" = "degT") %>% 
  arrange(date_time)



season2425 = wspd %>% filter(date_time > as.Date("2024-11-01"), 
                             date_time < as.Date("2025-03-31"))
season2526 = wspd %>% filter(date_time > as.Date("2025-11-01"))

p1 = ggplot() + 
  geom_line(data = season2425, aes(x =date_time, y = wspd), color = "cornflowerblue") + 
  theme_bw()+ labs(x = "Time", y = "Wind Speed (m/s)")
p2 = ggplot() +
  geom_line(data = season2526, aes(x = date_time, y = wspd), color = "navy")+ 
  theme_bw() + labs(x = "Time", y = "Wind Speed (m/s)")
ggarrange(p1, p2, ncol = 1) 


avg_wspd2425 = season2425 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
avg_wspd2526 = season2526 %>% 
  group_by(date(date_time)) %>% 
  summarise(avwspd = mean(wspd),
            stdwspd = sd(wspd))
ggplot() +
  geom_line(data = avg_wspd2425, aes(x = `date(date_time)`, y = avwspd), color = "cornflowerblue")+ 
  geom_line(data = avg_wspd2526, aes(x = `date(date_time)`-365, y = avwspd), color = "navy")+ 
  theme_bw() + labs(x = "Time", y = "Wind Speed (m/s)")  
  
# Wind Rose
compass_directions <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE", "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
degrees_per_sector <- 22.5

wind_to_compass <- function(degrees) {
  degrees <- degrees %% 360  # Ensure degrees are within 0-359 range
  sector_index <- floor((degrees + 11.25) / 22.5) + 1
  #sector_index <- ceiling(degrees / degrees_per_sector)
  # Handle edge case where sector_index is 17 (should be 1)
  if (sector_index == 17) {sector_index <- 1}
  if (sector_index == 0) {sector_index <- 1}
  return(compass_directions[sector_index])
}

season2425$direction <- sapply(season2425$wdir, wind_to_compass)
season2425$direction <- factor(season2425$direction, 
                               levels = compass_directions)
season2526$direction <- sapply(season2526$wdir, wind_to_compass)
season2526$direction <- factor(season2526$direction, 
                               levels = compass_directions)

p1 = ggplot(data = season2425, aes(x = direction, fill = wspd)) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Nov. 24 - Mar. 25)",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  scale_fill_viridis_c()
p2 = ggplot(data = season2526, aes(x = direction, fill = wspd)) +
  geom_bar() +
  coord_polar()+#start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency (Nov. 25 - Mar. 26)",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  scale_fill_viridis_c()
ggarrange(p1, p2, ncol = 2)
          

compass_direction <- season2425 %>% 
  rowwise() %>% 
  mutate(heading = wind_to_compass(wdir))

sum_comp_dir = compass_direction %>% 
  group_by(heading, wdir, round(wspd)) %>% 
  summarise(n=n())

ggplot(data = sum_comp_dir, aes(x = heading, fill = `round(wspd)`)) +
  geom_bar() +
  coord_polar(start = -pi/16) + # -pi/16 aligns sectors properly
  theme_minimal() +
  labs(title = "Wind Direction Frequency",
       x = "Direction",
       y = "Frequency",
       fill = "Speed (m/s)") +
  scale_fill_viridis_c() # Adds nice color coding




plot.windrose <- function(data, spd, dir, spdres = 2, dirres = 30, spdmin = 2, spdmax = 20, spdseq = NULL, palette = "YlGnBu", countmax = NA) {
  if (is.numeric(spd) & is.numeric(dir)) {
    data <- data.frame(spd = spd, dir = dir)
    spd <- "spd"
    dir <- "dir"
  }
  
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  if (missing(spdseq)) {
    spdseq <- seq(spdmin, spdmax, spdres)
  }
  
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  spd.colors <- colorRampPalette(brewer.pal(min(max(3, n.colors.in.range), min(9, n.colors.in.range)), palette))(n.colors.in.range)
  
  if (max(data[[spd]], na.rm = TRUE) > spdmax) {
    spd.breaks <- c(spdseq, max(data[[spd]], na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq])), paste(spdmax, "-", max(data[[spd]], na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else {
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]), '-', c(spdseq[2:n.spd.seq]))
  }
  
  data$spd.binned <- cut(x = data[[spd]], breaks = spd.breaks, labels = spd.labels, ordered_result = TRUE)
  
  dir.breaks <- c(-dirres/2, seq(dirres/2, 360-dirres/2, by = dirres), 360+dirres/2)
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2), paste(seq(dirres/2, 360-3*dirres/2, by = dirres), "-", seq(3*dirres/2, 360-dirres/2, by = dirres)), paste(360-dirres/2,"-",dirres/2))
  dir.binned <- cut(data[[dir]], breaks = dir.breaks, ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  p.windrose <- ggplot(data = data, aes(x = dir.binned, fill = spd.binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", values = spd.colors, drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  if (!is.na(countmax)) {
    p.windrose <- p.windrose + ylim(c(0, countmax))
  }
  
  print(p.windrose)
  return(p.windrose)
}


p1 = plot.windrose(data = season2425, spd = season2425$wspd, dir = season2425$wdir)
p2 = plot.windrose(data = season2526, spd = season2526$wspd, dir = season2526$wdir)
ggarrange(p1, p2, 
             ncol = 2,
             labels = c("Nov. 2024 - Mar. 2025","Nov. 2025 - Mar. 2026"))

