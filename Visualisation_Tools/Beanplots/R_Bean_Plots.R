library("RColorBrewer")
library("beanplot")

setwd("/Users/andrewdavies/Andy's Documents/Research projects and data/Manuscripts/2013/GoM/BeanPlots")

beans <- read.csv("final_all_species_master.csv", header=T)

test <- beans[1000:2000,]

#Subsets
suborders <- beans[ which(beans$species=='Alcyoniina' | beans$species=='Antipatharia' | beans$species=='Calcaxonia' | beans$species=='Holaxonia' | beans$species=='Scleraxonia'), ]
suborders$species <- factor(suborders$species)
spp <- beans[ which(beans$species=='Enallopsammia' | beans$species=='lophelia' | beans$species=='Madrepora' | beans$species=='all_scler'), ]
spp$species <- factor(spp$species)

unique(beans$species)
unique(spp$species)
names(beans)

#Per page all variables

#Suborders 1 aspect to nitrate
pdf('suborders1.pdf')
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(aspect~species, xaxt="n", data=suborders, ll=0.04, log="",what=c(0,1,0,1), ylab="Aspect\n(degrees)",las=1)
par(mar=c(0,9,0,5))
beanplot(bathy~species, ylab="Depth\n(m)\n", data=suborders, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(diso2~species, ylab="Dissolved\noxygen\n(ml l-1)\n", data=suborders, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(eastness~species, ylab="Eastness\n", data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(northness~species, ylab="Northness\n",data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(nitrate~species, ylab="Nitrate\n(ml l-1)\n", data=suborders, ll=0.04, log="",what=c(0,1,0,1),las=1)

#Suborders 2 oa to plan_dem
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(oa~species, xaxt="n", data=suborders, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega aragonite\n(Orr)",las=1)
par(mar=c(0,9,0,5))
beanplot(stein_oa~species, ylab="Omega aragonite\n(Steinacher)", data=suborders, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(oc~species, ylab="Omega calcite\n(Orr)", data=suborders, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(stein_oc~species, ylab="Omega calcite\n(Steinacher)", data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(phosphate~species, ylab="Phosphate\n(ml l-1)\n",data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(plan_dem~species, ylab="Plan Curvature\n", data=suborders, ll=0.04, log="",what=c(0,1,0,1),las=1)

#Suborders 3 poc to salinity
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(poc~species, xaxt="n", data=suborders, ll=0.04, log="",what=c(0,1,0,1), ylab="Particulate\norganic\ncarbon\n",las=1)
par(mar=c(0,9,0,5))
beanplot(poc_nn~species, ylab="Particulate\norganic\ncarbon\nnn\n", data=suborders, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(profile_dem~species, ylab="Profile curvature\n", data=suborders, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(roughness~species, ylab="Roughness\n", data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(rugosity~species, ylab="Rugosity\n",data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(salinity~species, ylab="Salinity\n", data=suborders, ll=0.04, log="",what=c(0,1,0,1),las=1)

#Suborders 4 silicate to tri
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(silicate~species, xaxt="n", data=suborders, ll=0.04, log="",what=c(0,1,0,1), ylab="Silicate\n",las=1)
par(mar=c(0,9,0,5))
beanplot(slope~species, ylab="Slope\n(degrees)\n", data=suborders, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(tang_dem~species, ylab="Tangential curvature\n", data=suborders, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(temperatur~species, ylab="Temperature\n(C)\n", data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(tpi~species, ylab="Topographic\nposition\nindex\n",data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(tri~species, ylab="Topographic\nroughness\nindex\n", data=suborders, ll=0.04, log="",what=c(0,1,0,1),las=1)
dev.off()


#SPP
#spp 1 aspect to nitrate
pdf('spp1.pdf')
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(aspect~species, xaxt="n", data=spp, ll=0.04, log="",what=c(0,1,0,1), ylab="Aspect\n(degrees)",las=1)
par(mar=c(0,9,0,5))
beanplot(bathy~species, ylab="Depth\n(m)\n", data=spp, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(diso2~species, ylab="Dissolved\noxygen\n(ml l-1)\n", data=spp, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(eastness~species, ylab="Eastness\n", data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(northness~species, ylab="Northness\n",data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(nitrate~species, ylab="Nitrate\n(ml l-1)\n", data=spp, ll=0.04, log="",what=c(0,1,0,1),las=1)

#spp 2 oa to plan_dem
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(oa~species, xaxt="n", data=spp, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega aragonite\n(Orr)",las=1)
par(mar=c(0,9,0,5))
beanplot(stein_oa~species, ylab="Omega aragonite\n(Steinacher)", data=spp, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(oc~species, ylab="Omega calcite\n(Orr)", data=spp, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(stein_oc~species, ylab="Omega calcite\n(Steinacher)", data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(phosphate~species, ylab="Phosphate\n(ml l-1)\n",data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(plan_dem~species, ylab="Plan Curvature\n", data=spp, ll=0.04, log="",what=c(0,1,0,1),las=1)

#spp 3 poc to salinity
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(poc~species, xaxt="n", data=spp, ll=0.04, log="",what=c(0,1,0,1), ylab="Particulate\norganic\ncarbon\n",las=1)
par(mar=c(0,9,0,5))
beanplot(poc_nn~species, ylab="Particulate\norganic\ncarbon\nnn\n", data=spp, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(profile_dem~species, ylab="Profile curvature\n", data=spp, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(roughness~species, ylab="Roughness\n", data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(rugosity~species, ylab="Rugosity\n",data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(salinity~species, ylab="Salinity\n", data=spp, ll=0.04, log="",what=c(0,1,0,1),las=1)

#spp 4 silicate to tri
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(silicate~species, xaxt="n", data=spp, ll=0.04, log="",what=c(0,1,0,1), ylab="Silicate\n",las=1)
par(mar=c(0,9,0,5))
beanplot(slope~species, ylab="Slope\n(degrees)\n", data=spp, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(tang_dem~species, ylab="Tangential curvature\n", data=spp, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(temperatur~species, ylab="Temperature\n(C)\n", data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(tpi~species, ylab="Topographic\nposition\nindex\n",data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(tri~species, ylab="Topographic\nroughness\nindex\n", data=spp, ll=0.04, log="",what=c(0,1,0,1),las=1)
dev.off()






#Below is for selected variables only.

#Suborders for potential paper
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(stein_oa~species, xaxt="n", data=suborders, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega\n aragonite ()\n",las=1)
par(mar=c(0,9,0,5))
beanplot(bathy~species, ylab="Depth\n(m)\n", data=suborders, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(diso2~species, ylab="Dissolved\noxygen\n(ml l-1)\n", data=suborders, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(salinity~species, ylab="Salinity\n", data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(slope~species, ylab="Slope\n",data=suborders, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(temperatur~species, ylab="Temperature\n(°C)\n", data=suborders, ll=0.04, log="",what=c(0,1,0,1),las=1)
#SPP for potential paper
par(family="serif")
par(mfrow=c(7,1))
par(mar=c(0,9,1,5))
beanplot(stein_oa~species, xaxt="n", data=spp, ll=0.04, log="",what=c(0,1,0,1), ylab="Omega\n aragonite ()\n",las=1)
par(mar=c(0,9,0,5))
beanplot(bathy~species, ylab="Depth\n(m)\n", data=spp, xaxt="n", ll=0.04, log="",what=c(0,1,0,1),las=1)
beanplot(diso2~species, ylab="Dissolved\noxygen\n(ml l-1)\n", data=spp, ll=0.04, xaxt="n", log="",what=c(0,1,0,1),las=1)
beanplot(salinity~species, ylab="Salinity\n", data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1),las=1)
beanplot(slope~species, ylab="Slope\n",data=spp, ll=0.04, log="",xaxt="n",what=c(0,1,0,1), las=1)
par(mar=c(0,9,0,5))
beanplot(temperatur~species, ylab="Temperature\n(°C)\n", data=spp, ll=0.04, log="",what=c(0,1,0,1),las=1)