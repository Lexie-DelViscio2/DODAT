# OBIS Download - Number of species below 200m
require(robis, ggplot2, dplyr, gridExtra, raster)
data_cnidaria <- occurrence("Cnidaria", startdepth = 200)
data_porifera <- occurrence("Porifera", startdepth = 200)

plot1 <- ggplot(data, aes(x = yearcollected)) + geom_freqpoly(binwidth = 5) + xlab("Year Collected") + ylab("Frequency")
plot2 <- ggplot(data_porifera, aes(x = yearcollected)) + geom_freqpoly(binwidth = 5) + xlab("Year Collected") + ylab("Frequency")
grid.arrange(plot1, plot2, ncol=2)

# Plot on map
global_grid <- raster(nrows = 180/5, ncol = 360/5, xmn = -180, xmx = 180, ymn = -90, ymx = 90)
lonlat_sp <- cbind(x = data$decimalLongitude, y = data$decimalLatitude)
gridded_occs <- rasterize(x = lonlat_sp, y = global_grid, fun = "count")
obis.p <- data.frame(rasterToPoints(gridded_occs))
names(obis.p) <- c("longitude", "latitude", "OBIS")


obis.p$OBIS <- ifelse(obis.p$OBIS<10,NA,obis.p$OBIS)

world <- map_data("world")

worldmap <- ggplot(world, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  theme(panel.background = element_rect(fill = "gray")) +
  coord_equal()

colors <- colorRampPalette(c("blue", "yellow", "red"))(length(levels(obis.p$OBIS2)))

(worldmap + geom_raster(data = obis.p, aes(x = longitude, y = latitude, fill = OBIS)) + 
    scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                         breaks=c(10,10000),labels=c("10","10k"),
                         limits=c(10,10000))
)

# Porifera
global_grid <- raster(nrows = 180/5, ncol = 360/5, xmn = -180, xmx = 180, ymn = -90, ymx = 90)
lonlat_sp <- cbind(x = data_porifera$decimalLongitude, y = data_porifera$decimalLatitude)
gridded_occs <- rasterize(x = lonlat_sp, y = global_grid, fun = "count")
obis.p <- data.frame(rasterToPoints(gridded_occs))
names(obis.p) <- c("longitude", "latitude", "OBIS")


obis.p$OBIS <- ifelse(obis.p$OBIS<10,NA,obis.p$OBIS)

world <- map_data("world")

worldmap <- ggplot(world, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  theme(panel.background = element_rect(fill = "gray")) +
  coord_equal()

colors <- colorRampPalette(c("blue", "yellow", "red"))(length(levels(obis.p$OBIS2)))

(worldmap + geom_raster(data = obis.p, aes(x = longitude, y = latitude, fill = OBIS)) + 
    scale_fill_gradientn(colours=topo.colors(7),na.value = "transparent",
                         breaks=c(10,max(obis.p$OBIS)), labels=c("10","10k"),
                         limits=c(10,max(obis.p$OBIS)))
)


