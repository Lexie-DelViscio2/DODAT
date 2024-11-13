# dsdtools - Physical_Data - CTD_Data - CTD_Transect_PLot.R
# 
# Plot CTD transects rapidly from a series of CTD files. This has been written specifically
# for SEA-BIRD systems, and uses CNV files as input. You will need to amend areas of the script
# if using your own data.

# You may need to set a path to the working directory where your CTD files are
#setwd("YOUR USER DIRECTORY")

# Load libaries
packages <- c("oce", "ocedata", "ggplot2", "ggpmisc", "gtable", "gridExtra", "reshape2", "zoo", "akima")
sapply(packages, require, character.only = TRUE); rm(packages)
# Change to normal braket style in oce.
options(oceUnitBracket="(")

# Read CTD Data
# 
# Three sets of data, Reference/SVP, Transect 1 (across seamount).
# 
# CTD Stations
# List ID   Station name        Note                  Filename
# 1         GS16A-01-CTD-01     Reference and SVP     472.cnv
# 2         GS16A-14-CTD-02     Transect 1            473.cnv
# 3         GS16A-15-CTD-03     Transect 1            474.cnv
# 4         GS16A-16-CTD-04     Transect 1            475.cnv
# 5         GS16A-17-CTD-05     Transect 1            476.cnv
# 6         GS16A-18-CTD-06     Transect 1            477.cnv
# 7         GS16A-24-CTD-07     Transect 2            478.cnv
# 8         GS16A-25-CTD-08     Transect 2            479.cnv
# 9         GS16A-26-CTD-09     Transect 2            480.cnv
# 10        GS16A-27-CTD-10     Transect 2            481.cnv
# 11        GS16A-28-CTD-11     Transect 2            482.cnv
ctd <- read.ctd.sbe("./*.cnv")

# Check read is OK
for(i in c(1:length(ctd))) { str(ctd[[i]]@metadata$station, 1) }; rm(i)

# Modify station name to reflect our station name rather than filename for ease of use. These must 
# follow the read order.
stations <- c("GS16A-01-CTD-01","GS16A-14-CTD-02","GS16A-15-CTD-03","GS16A-16-CTD-04", 
              "GS16A-17-CTD-05", "GS16A-18-CTD-06", "GS16A-24-CTD-07", "GS16A-25-CTD-08",
              "GS16A-26-CTD-09", "GS16A-27-CTD-10", "GS16A-28-CTD-11")
n = 1; for(i in stations) { ctd[[n]]@metadata$station <- i; n = n+1}; rm(stations, n, i)

########
# Define PlotCTDProfile Function to create Profile
########
plotCTDProfile <- function(section.i, variable, zlab, col.break, col.ramp){
  section.i <- sectionGrid(section.i)
  nstation <- length(section.i[['station']])
  p <- unique(section.i[['pressure']])
  np <- length(p)
  T <-  array(NA, dim=c(nstation, np))
  for (i in 1:nstation) {
    T[i, ] <- section.i[["station"]][[i]][[variable]]
  }
  distance <- unique(section.i[['distance']])
  
  # Rearrange to dataframe
  T3 <- melt(T)
  test <- acast(T3, T3$Var2 ~ T3$Var1)
  row.names(test) <- p
  colnames(test) <- distance
  # Extract bottom polygon dimensions from the data.
  stationdepths <- matrix(ncol=2, nrow=ncol(test)+2); colnames(stationdepths) <- c("distance","depth")
  depths.list <- list(); for(i in 1:ncol(test)) 
  { depths.list[[i]] <- as.numeric(names(tail(na.omit(test[,i]), n=1))) }; rm(i)
  depths.list <- unlist(depths.list)
  # Construct Polygon as stationdepths
  for(i in 1:ncol(test)) {
    if(i == 1) {
      stationdepths[i,1] <- min(distance)
      stationdepths[i,2] <- max(depths.list)
    }
    stationdepths[i+1,1] <- distance[[i]]
    stationdepths[i+1,2] <- depths.list[[i]]
    if(i == max(ncol(test))) { 
      stationdepths[i+2,1] <- max(distance)
      stationdepths[i+2,2] <- max(depths.list)
    }
  }
  stationdepths <- as.data.frame(stationdepths)
  # Fill in zeros with drag down
  test <- na.locf(test)
  # Back to xyz
  test2 <- melt(test)
  colnames(test2) <- c("y","x","z")
  u <- interp(test2$x, test2$y, test2$z, xo=seq(min(test2$x), max(test2$x), length = 100), 
              yo=seq(min(test2$y), max(test2$y), length = 3000), linear = TRUE, extrap=FALSE, 
              duplicate = "error", dupfun = NULL, ncp = NULL)
  p1 <- u$y
  distance1 <- u$x
  var <- u$z
  
  # Plot
  col.break <- col.break
  imagep(distance1, p1, var, col=col.ramp, breaks=col.break, flipy=TRUE, 
         drawContours = FALSE, xlab='Geodesic distance (km)', ylab='Depth (m)', zlab=zlab, decimate=FALSE)
  oce.contour(distance1, p1, var, levels=col.break, drawlabels=TRUE, add=TRUE, labcex = 0.7)
  polygon(stationdepths$distance, stationdepths$depth, col="black")
  abline()
  for(i in 1:length(distance)) { abline(v=distance[i], col="white", lty="dotted") }
}
#### End Function

############
# CTD Transects
# ----

# Transect 1 - GS16A-14-CTD-02 to GS16A-14-CTD-06
# First convert to a CTD section object
transect1 <- vector("list", 5)
n = 1
# Change 2:6 to reflect the stations in the list that you wish to plot a transect for
for(i in c(2:6)) { transect1[[n]] <- ctd[[i]]; n=n+1} ; rm(n,i)
transect1<- as.section(transect1)
# Summarise transect1 section details
summary(transect1)
str(transect1@metadata$station, 1)

# Plot Transect 1 - in two parts due to size of output page constraints
bitmap(file=paste(getwd(),"GS16A-Transect1-part-1.tiff",sep=""), 
       width=8, height=9, units="in", res=600, type="tiff24nc")
par(mfrow=c(3,1), cex=1.1)
plotCTDProfile(transect1, "temperature", "Temperature (°C)", 
               seq(-2, 6, 0.25), colorRampPalette(c("blue", "red")))
plotCTDProfile(transect1, "salinity", "Salinity (°pss)", 
               seq(34.80, 35.20, 0.05), colorRampPalette(c("white", "blue")))
plotCTDProfile(transect1, "turbidity", "Turbidity (RU)", 
               seq(0, 0.8, 0.1), colorRampPalette(c("white", "brown")))
dev.off()

bitmap(file=paste(getwd(),"GS16A-Transect1-part-2.tiff",sep=""), 
       width=8, height=9, units="in", res=600, type="tiff24nc")
par(mfrow=c(3,1), cex=1.1)
plotCTDProfile(transect1, "fluorescence", "Fluoresence (RU)", 
               seq(0, 0.8, 0.1), colorRampPalette(c("yellow", "green")))
plotCTDProfile(transect1, "oxygen2", "Oxygen (ml/l)", 
               seq(6.6, 7.5, 0.1), colorRampPalette(c("blue", "orange")))
plotCTDProfile(transect1, "sigmaTheta", "Sigma Theta (kg m3)", 
               seq(27.5, 28.2, 0.05), colorRampPalette(c("gray", "black")))
dev.off()

# Transect 2 - GS16A-14-CTD-07 to GS16A-14-CTD-11 - See comments above
transect2 <- vector("list", 5)
n = 1
for(i in c(7:11)) { transect2[[n]] <- ctd[[i]]; n=n+1} ; rm(n,i)
transect2<- as.section(transect2)
summary(transect2)
str(transect2@metadata$station, 1)

# Plot Transect 2
bitmap(file=paste(getwd(),"GS16A-Transect2-part-1.tiff",sep=""), 
       width=8, height=9, units="in", res=600, type="tiff24nc")
par(mfrow=c(3,1), cex=1.1)
plotCTDProfile(transect2, "temperature", "Temperature (°C)", 
               seq(-2, 6, 0.25), colorRampPalette(c("blue", "red")))
plotCTDProfile(transect2, "salinity", "Salinity (°pss)", 
               seq(34.80, 35.20, 0.05), colorRampPalette(c("white", "blue")))
plotCTDProfile(transect2, "turbidity", "Turbidity (RU)", 
               seq(0, 0.8, 0.1), colorRampPalette(c("white", "brown")))
dev.off()

bitmap(file=paste(getwd(),"GS16A-Transect2-part-2.tiff",sep=""), 
       width=8, height=9, units="in", res=600, type="tiff24nc")
par(mfrow=c(3,1), cex=1.1)
plotCTDProfile(transect2, "fluorescence", "Fluoresence (RU)", 
               seq(0, 0.8, 0.1), colorRampPalette(c("yellow", "green")))
plotCTDProfile(transect2, "oxygen2", "Oxygen (ml/l)", 
               seq(6.6, 7.5, 0.1), colorRampPalette(c("blue", "orange")))
plotCTDProfile(transect2, "sigmaTheta", "Sigma Theta (kg m3)", 
               seq(27.5, 28.2, 0.05), colorRampPalette(c("gray", "black")))
dev.off()