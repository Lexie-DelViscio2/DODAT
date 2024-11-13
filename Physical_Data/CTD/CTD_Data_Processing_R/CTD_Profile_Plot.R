# dsdtools - Physical_Data - CTD_Data - CTD_Profile_Plot.R
# 
# Plot CTD profiles rapidly from a series of CTD files. This has been written specifically
# for SEA-BIRD systems, and uses CNV files as input. You will need to amend areas of the script
# if using your own data.

# You may need to set a path to the working directory where your CTD files are
#setwd("YOUR USER DIRECTORY")

# Load libaries
packages <- c("oce", "ocedata", "ggplot2", "ggpmisc", "gtable", "gridExtra")
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

# Modify station name to reflect our station name rather than filename
stations <- c("GS16A-01-CTD-01","GS16A-14-CTD-02","GS16A-15-CTD-03","GS16A-16-CTD-04", 
              "GS16A-17-CTD-05", "GS16A-18-CTD-06", "GS16A-24-CTD-07", "GS16A-25-CTD-08",
              "GS16A-26-CTD-09", "GS16A-27-CTD-10", "GS16A-28-CTD-11")
n = 1; for(i in stations) { ctd[[n]]@metadata$station <- i; n = n+1}; rm(stations, n, i)

############
# Plot each station's variables with nice plot
# ----
# GS16A-01-CTD-01
bitmap(file=paste(getwd(),"GS16A-01-CTD-01.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
maxD = 2405
# Fix Station number
ctd.i <- ctdDecimate(ctd[[1]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
#Problem with O2 sensor at db 1499-1501
ctd.i@data$oxygen2[1499] <- NA; ctd.i@data$oxygen2[1500] <- NA; ctd.i@data$oxygen2[1501] <- NA
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs, xlim=c(6.4,7.2))
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-14-CTD-02
bitmap(file=paste(getwd(),"GS16A-14-CTD-02.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
# Fix Station number
ctd.i <- ctdDecimate(ctd[[2]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-15-CTD-03
bitmap(file=paste(getwd(),"GS16A-15-CTD-03.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[3]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-16-CTD-04 
bitmap(file=paste(getwd(),"GS16A-16-CTD-04.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[4]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = 650 #ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
#Problem with O2 sensor at db 1499-1501
ctd.i@data$oxygen2[614] <- NA; ctd.i@data$oxygen2[615] <- NA; ctd.i@data$oxygen2[616] <- NA
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-17-CTD-05
bitmap(file=paste(getwd(),"GS16A-17-CTD-05.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[5]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-18-CTD-06
bitmap(file=paste(getwd(),"GS16A-18-CTD-06.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[6]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-24-CTD-07
bitmap(file=paste(getwd(),"GS16A-24-CTD-07.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[7]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab="Density (Sigma Theta)", cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-25-CTD-08
bitmap(file=paste(getwd(),"GS16A-25-CTD-08.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[8]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab=expression(paste("Density ( ", sigma, theta, " )")), cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-26-CTD-09
bitmap(file=paste(getwd(),"GS16A-26-CTD-09.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[9]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab=expression(paste("Density ( ", sigma, theta, " )")), cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-27-CTD-10
bitmap(file=paste(getwd(),"GS16A-27-CTD-10.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[10]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab=expression(paste("Density ( ", sigma, theta, " )")), cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

# GS16A-28-CTD-11
bitmap(file=paste(getwd(),"GS16A-28-CTD-11.tiff",sep=""), 
       width=8, height=11, units="in", res=600, type="tiff24nc")
par(mfrow=c(4,2), cex=1.1)
fs = 1
ctd.i <- ctdDecimate(ctd[[11]], p=1, method="boxcar")
ctd.i@metadata$startTime <- ""
maxD = ctd.i@metadata$waterDepth
plotProfile(ctd.i, xtype=c(ctd.i@data$temperature), col="red", ytype="depth", 
            ylim=c(maxD,0), xlab="Temperature (°C)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$salinity), col="blue", ytype="depth", 
            ylim=c(maxD,0), xlab="Salinity (pss)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$fluorescence), col="green", ytype="depth", 
            ylim=c(maxD,0), xlab="Fluorescence (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$turbidity), col="brown", ytype="depth", 
            ylim=c(maxD,0), xlab="Turbidity (RU)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$oxygen2), col="orange", ytype="depth", 
            ylim=c(maxD,0), xlab="Oxygen (ml/l)", cex.axis=fs)
plotProfile(ctd.i, xtype=c(ctd.i@data$sigmat), col="dark gray", ytype="depth", 
            ylim=c(maxD,0), xlab=expression(paste("Density ( ", sigma, theta, " )")), cex.axis=fs, xlim=c(27.5,28.2))
plotTS(subset(ctd.i, ctd.i@data$pressure < maxD), cex=0.5)
plot(ctd.i, which="map")
dev.off()

rm(ctd.i, fs, maxD)
# End
# ----
############
