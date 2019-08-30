## plot results from the TREES simulation on Chinese Cabbage (CC)

####################################
## set your working directory and load functions
####################################

setwd("~/Dropbox/SHARED/Brassica/GROUP/TREES-workshop/TREES/")
source("plotting_scripts/Brassica_CC_functions.R")

####################################
## load your simulation results file
####################################

# for individual leaves
cc_ww_leaf <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_ww_drought_valid_expt.leaf")
cc_d1_leaf <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_d1_drought_valid_expt.leaf")
cc_d2_leaf <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_d2_drought_valid_expt.leaf")

# for all other output variables
cc_ww_sim <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_ww_drought_valid_expt.sim")
cc_d1_sim <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_d1_drought_valid_expt.sim")
cc_d2_sim <- read.delim("TREES_workshop_exercises/BRASSICA/outputs/cc_d2_drought_valid_expt.sim")

####################################
## get drivers and turn time into fraction jdays for plotting
####################################

cc.ww.drivers = read.delim("TREES_workshop_exercises/BRASSICA/drivers/cc_ww_drivers_drought_validation.txt")
cc.doy = cc.ww.drivers$jday + cc.ww.drivers$Time/24


####################################
## load the observations
####################################

# soil water content
swc <- read.csv("TREES_workshop_exercises/BRASSICA/experimental_data/SWC_drought_CC_R500.csv")

# leaf area 
CC.ELA = read.csv("TREES_workshop_exercises/BRASSICA/experimental_data/CC_ELA_TREES-workshop.csv")


####################################
## format data and make plots
####################################

# soil water content
sd.mean.ww = make.sd.mean.df("CC", "WW", swc)[[1]]; dat = make.sd.mean.df("CC", "WW", swc)[[2]]
sd.mean.d1 = make.sd.mean.df("CC", "D1", swc)[[1]]; dat = make.sd.mean.df("CC", "D1", swc)[[2]]
sd.mean.d2 = make.sd.mean.df("CC", "D2", swc)[[1]]; dat = make.sd.mean.df("CC", "D2", swc)[[2]]

sd.mean.ww[,1:2]=sd.mean.ww[,1:2]/100
sd.mean.d1[,1:2]=sd.mean.d1[,1:2]/100
sd.mean.d2[,1:2]=sd.mean.d2[,1:2]/100

pdf("TREES_workshop_exercises/BRASSICA/plots/CC_SWC_timeSeries_Plot.pdf", width = 5, height= 7, useDingbats = F)
layout(matrix(1:3, ncol = 1, nrow=3), widths = rep(1, 3), heights = c(1.8,1.2,1.6), respect = FALSE)
par(mar = c(0, 4.1, 4.1, 2.1))
plot( agg.sim(cc.doy, cc_ww_sim$thetaRoot), type="l", ylim=c(0.15, 0.45), lwd =2, xlim=c(128, 136), xaxt='n', cex.axis=1, ylab= "swc")
points(sd.mean.ww[,4]+ (17/24) ,  sd.mean.ww[,1] ,  type="p", lwd=2, cex =2)  ## plot at 5pm daily
add.se (x= sd.mean.ww[,4]+ (17/24) , avg= sd.mean.ww[,1], sdev = sd.mean.ww[,2]/sqrt(sd.mean.ww[,3]) )
Axis(side=1, labels=FALSE)

par(mar = c(0, 4.1, 0, 2.1))
plot( agg.sim(cc.doy, cc_d1_sim$thetaRoot), type="l", lwd=2, ylim=c(0.15, 0.45), xlim=c(128, 136), xaxt='n', cex.axis=1, ylab= "swc")
points( x= sort(unique(dat$DOY))+ (17/24) ,  sd.mean.d1[,1] , pch = 2, type="p", lwd=2, cex =2) 
add.se (x= sort(unique(dat$DOY))+ (17/24) , avg= sd.mean.d1[,1], sdev = sd.mean.d1[,2]/sqrt(sd.mean.d1[,3]) )
Axis(side=1, labels=FALSE)

par(mar = c(3, 4.1, 0, 2.1))
plot( agg.sim(cc.doy, cc_d2_sim$thetaRoot), type="l",  lwd=2, ylim=c(0.15, 0.45), xlim=c(128, 136), cex.axis=1 , ylab= "swc", xlab= "doy")
points( x= sort(unique(dat$DOY))+ (17/24) ,  sd.mean.d2[,1] , pch=0, type="p" , lwd=2, cex =2) 
add.se (x= sort(unique(dat$DOY))+ (17/24) , avg= sd.mean.d2[,1],  sdev = sd.mean.d2[,2]/sqrt(sd.mean.d2[,3]) )
dev.off()

# leaf area

days = c(128:140, 142:150, 153)

cc.ww.ela = CC.ELA[which(CC.ELA$Treatment == "WW"),]
cc.d1.ela = CC.ELA[which(CC.ELA$Treatment == "D1"),]
cc.d2.ela = CC.ELA[which(CC.ELA$Treatment == "D2"),]

pdf("TREES_workshop_exercises/BRASSICA/plots/CC_leafArea_timeSeries_Plot.pdf", width = 5, height= 7, useDingbats = F)
layout(matrix(1:3, ncol = 1, nrow=3), widths = rep(1, 3), heights = c(1.8,1.4,1.6), respect = FALSE)
par(mar = c(0, 4.1, 4.1, 2.1))
plot(cc.doy, rowSums(cc_ww_leaf[-1], na.rm=T) , type="l",  lwd =2, xlim=c(128, 136), xaxt='n', 
     ylim=c(0,70), cex.axis=1, ylab = "total leaf area")
add.lfarea.pts("WW", pch.in=1)
Axis(side=1, labels=FALSE)

par(mar = c(0, 4.1, 0, 2.1))
plot(cc.doy, rowSums(cc_d1_leaf[-1], na.rm=T) , type="l",  lwd =2, xlim=c(128, 136), xaxt='n', 
     ylim=c(0,70), cex.axis=1, pch = 2, ylab = "total leaf area")
add.lfarea.pts("D1", pch.in=2)
Axis(side=1, labels=FALSE)

par(mar = c(2, 4.1, 0, 2.1))
plot(cc.doy, rowSums(cc_d2_leaf[-1], na.rm=T) , type="l",  lwd =2, xlim=c(128, 136), 
     ylim=c(0,70), cex.axis=1, pch = 0, ylab = "total leaf area")
add.lfarea.pts("D2", pch.in=0)
dev.off()

