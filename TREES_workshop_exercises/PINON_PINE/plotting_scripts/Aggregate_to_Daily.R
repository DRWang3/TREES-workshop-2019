#
#TREES workshop plotting routines
#  This script is used first to convert half-hourly simulation output to daily
#

#
#run these lines to create daily files for the four half-hourly simulations
#
setwd(paste("~/Dropbox/GROUP/TREES-workshop/TREES/TREES_workshop_exercises/PINON_PINE",sep=""))
source("plotting_scripts/Pinon_aggregate_to_daily.R")
drivers<- read.table(paste("drivers/","sumo_pinon_ambient_march_to_august_2012",".txt",sep=""), header=TRUE)
Ksat <- 1.5/(-0.93+2.13) #whole-plant saturated hydraulic conductance, pinon
SLA <- 7.6 #specific leaf area, pinon
#ambient
fname <- "sumo_pine_ambient"
simulation<-read.table(paste("outputs/",fname,".sim",sep=""),header=TRUE)
computeDaily(fname, simulation, drivers, Ksat, SLA)
#drought
fname <- "sumo_pine_drought"
simulation<-read.table(paste("outputs/",fname,".sim",sep=""),header=TRUE)
computeDaily(fname, simulation, drivers, Ksat, SLA)
#ambient no bedrock groundwater 
fname <- "sumo_pine_ambient_noGW"
simulation<-read.table(paste("outputs/",fname,".sim",sep=""),header=TRUE)
computeDaily(fname, simulation, drivers, Ksat, SLA)
#ambient with modified vulnerability curves
fname <- "sumo_pine_ambient_run2"
simulation<-read.table(paste("outputs/",fname,".sim",sep=""),header=TRUE)
computeDaily(fname, simulation, drivers, Ksat, SLA)

