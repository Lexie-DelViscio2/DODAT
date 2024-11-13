############
# Optimus Full Year Deployment April *th - USBL Telemetry Anaylsis
############

library(dplyr)
library(lubridate)
library(oce)
library(plotly)
library(ggforce)
library(geosphere)

setwd("/Users/andrewdavies/Library/CloudStorage/GoogleDrive-davies@uri.edu/Shared drives/Project - NOAA NRDA MDBC/Cruises/Current Cruises/NF2402/NF2402_URIALBEX01002/Processed/USBL_Telemetry")
lander <- read.csv("0067-0067-OptimusPrime Descent Log_Clean_All_Releases.csv")

lander_lr_despike <- lander %>%
                        mutate(Time_min = as.numeric(hms(paste0(TimeHr,":",TimeMin,":",TimeSec)), "minutes")) %>%
                        mutate(DateTime = as.POSIXct(paste0(Date," ", TimeHr,":",TimeMin,":",round(TimeSec,0)), format="%m/%d/%y %H:%M:%OS")) %>%
                        mutate(LoadRlseDepth_Despike = despike(OptimusLoadRlseDepth, reference = "smooth")*-1) %>%
                        mutate(LoadRlseLatitiude_Despike = despike(OptimusLoadRlseLat, reference = "smooth")) %>%
                        mutate(LoadRlseLongitude_Despike = despike(OptimusLoadRlseLon, reference = "smooth")) %>%
                        mutate(id = row_number()) %>%
                        select(id, DateTime, Time_min, LoadRlseLatitiude_Despike, LoadRlseLongitude_Despike, LoadRlseDepth_Despike)

lander_pr_despike <- lander %>%
                        mutate(OptimusPortRDepth_Despike = despike(OptimusPortRDepth, reference = "smooth")*-1) %>%
                        mutate(OptimusPortRLat_Despike = despike(OptimusPortRLat, reference = "smooth")) %>%
                        mutate(OptimusPortRLon_Despike = despike(OptimusPortRLon, reference = "smooth")) %>%
                        mutate(id = row_number()) %>%
                        select(OptimusPortRLat_Despike, OptimusPortRLon_Despike, OptimusPortRDepth_Despike)

lander_sr_despike <- lander %>%
                        mutate(OptimusStbdDepth_Despike = despike(OptimusStbdDepth, reference = "smooth")*-1) %>%
                        mutate(OptimusStbdLat_Despike = despike(OptimusStbdLat, reference = "smooth")) %>%
                        mutate(OptimusStbdLon_Despike = despike(OptimusStbdLon, reference = "smooth")) %>%
                        mutate(id = row_number()) %>%
                        select(OptimusStbdLat_Despike, OptimusStbdLon_Despike, OptimusStbdDepth_Despike)

ship_despike <- lander %>%
                        mutate(ShipPosLat_Despike = despike(ShipPosLat, reference = "smooth")) %>%
                        mutate(ShipPosLon_Despike = despike(ShipPosLon, reference = "smooth")) %>%
                        mutate(id = row_number()) %>%
                        select(ShipPosLat_Despike, ShipPosLon_Despike)

lander_despike <- cbind(lander_lr_despike, lander_pr_despike, lander_sr_despike, ship_despike)

# If you need to remove any errant spikes in the data, do so here:
figure <- plot_ly(lander_despike, x = ~DateTime, y = ~LoadRlseDepth_Despike, text = ~id, type = 'scatter', mode = 'lines', name = 'Heavy Release')
figure <- figure %>% add_trace(y = ~OptimusPortRDepth_Despike, name = 'Stbd Lander Release', mode = 'lines') 
figure
# spike_row <- c(859,772, 773)
# jframe <- jframe %>%
#             filter(!id %in% spike_row)

# Subset to different parts
cable_start <- 44
cable_end <- 1017

free_fall_start <- 1777
free_fall_end <- 1784

final_location_start <- 1785
final_location_end <- 1958

target_lat <- 28.3133100
target_lon <- -87.300674

lander_cable <- lander_despike %>%
                  filter(between(id, cable_start, cable_end))
                  
lander_freefall <- lander_despike %>%
                      filter(between(id, free_fall_start, free_fall_end))

lander_final <- lander_despike %>%
                    filter(between(id, final_location_start, final_location_end))    %>%
                    na.omit()

lander_cable_speed <- as.numeric(max(lander_cable$Time)- min(lander_cable$Time), units = "mins")
lander_cable_distance <- max(lander_cable$LoadRlseDepth_Despike)- min(lander_cable$LoadRlseDepth_Despike)
print(paste0("Cable descent speed: ", round(lander_cable_distance/lander_cable_speed,2), " m/min."))

lander_freefall_speed <- as.numeric(max(lander_freefall$Time)- min(lander_freefall$Time), units = "mins")
lander_freefall_distance <- max(lander_freefall$OptimusStbdDepth_Despike)- min(lander_freefall$OptimusStbdDepth_Despike)
print(paste0("Freefall descent speed: ", round(lander_freefall_distance/lander_freefall_speed,2), " m/min."))

final_location_longitude <- mean(lander_final$OptimusPortRLon_Despike, na.rm=TRUE)
final_location_longitude_sd <- sd(lander_final$OptimusPortRLon_Despike, na.rm=TRUE)
final_location_latitude <- mean(lander_final$OptimusPortRLat_Despike, na.rm=TRUE)
final_location_latitude_sd <- sd(lander_final$OptimusPortRLat_Despike, na.rm=TRUE)

final_location_error_radius <- mean(final_location_longitude_sd, final_location_latitude_sd)

final_location_error_radius_m <- final_location_error_radius * 111195

print(paste0("Final position error: ", round(final_location_error_radius_m, 2), " m."))

distance_to_target <- distm(c(final_location_longitude, final_location_latitude),
                            c(target_lon, target_lat), fun = distHaversine)

print(paste0("Final position discrepancy: ", round(distance_to_target, 2), " m."))

plot_ly(jframe, x=~Longitude, y=~Latitude, z=~Depth_Despike, type = 'scatter3d', mode = 'lines',
        opacity = 1, line = list(width = 2, reverscale = FALSE))

write.csv(lander, "USBL_Telemetry_Optimus_Deployment.csv")

# Plots
p_depth <- ggplot(lander_despike, aes(x = DateTime)) + 
  geom_line(aes(y = LoadRlseDepth_Despike), color = "#4494ad", alpha = 1) + 
  geom_line(aes(y = OptimusPortRDepth_Despike), color = "orange", alpha = 1) + 
  labs(x = "Time", y = "Depth (m)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none")
ggsave("Depth_Profile.pdf", p_depth)
p_depth

p_position <- ggplot(lander_despike) + 
  geom_point(aes(x = LoadRlseLongitude_Despike, y = LoadRlseLatitiude_Despike, color = DateTime), alpha = 0.3) + 
  geom_point(aes(x = final_location_longitude, y = final_location_latitude), color = "black") + 
  geom_point(aes(x = target_lon, y = target_lat), color = "gold") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_circle(aes(x0=final_location_longitude, y0=final_location_latitude, r=final_location_error_radius), inherit.aes=FALSE) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
p_position

ggsave("Position_Profile.pdf", p_position)

p_position_inset <- ggplot(lander_despike) + 
  geom_point(aes(x = LoadRlseLongitude_Despike, y = LoadRlseLatitiude_Despike, color = DateTime), alpha = 0.3) + 
  geom_point(aes(x = final_location_longitude, y = final_location_latitude), color = "black") + 
  geom_point(aes(x = target_lon, y = target_lat), color = "gold") +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  geom_circle(aes(x0=final_location_longitude, y0=final_location_latitude, r=final_location_error_radius), inherit.aes=FALSE) + 
  xlim(-87.301, -87.3005) + 
  ylim(28.313,28.3135) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
p_position_inset

ggsave("Position_Profile_Inset.pdf", p_position_inset)
