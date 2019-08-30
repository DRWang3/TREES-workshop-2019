#
#Use this script to plot predawn leaf water potential and transpiration
# from the pinon simulations
#
#Note: This script assumes you have already run Aggregate_to_Daily.R 
#

#
#Read in midday simulation results
#
setwd(paste("~/Dropbox/GROUP/TREES-workshop/TREES/TREES_workshop_exercises/PINON_PINE",sep=""))
ambient<-read.csv(paste("outputs/"," sumo_pine_ambient _midday",".csv",sep=""),header=TRUE)
drought<-read.csv(paste("outputs/"," sumo_pine_drought _midday",".csv",sep=""),header=TRUE)
ambient_noGW<-read.csv(paste("outputs/"," sumo_pine_ambient_noGW _midday",".csv",sep=""),header=TRUE)
ambient_run2<-read.csv(paste("outputs/"," sumo_pine_ambient_run2 _midday",".csv",sep=""),header=TRUE)
#
#Define some y-axis labels
#
exp_list <- c(as.expression(bquote(Psi[PD]~"(MPa)" )),
              as.expression(bquote(italic(E)[C]~"or"~italic(E)[Crit]~"("~"mmol" ~m^-2~s^-1~")" )),
              as.expression(bquote(Psi[PD]~"(MPa)" )),
              as.expression(bquote(Psi[PD]~"(MPa)" )))
#
#Plot predawn leaf water potential
#
pdf("plots/Pinon_predawn_leaf_water_potential.pdf", width = 5, height= 7, useDingbats = F)

layout(matrix(1:3, ncol = 1, nrow=3), widths = rep(1, 3), heights = c(1.2,1.2,1.2), respect = FALSE)
par(oma=c(6.1,1.1,1,.1))
par(mar = c(0, 4.8, 0.0, 2.1))
plot(ambient$DAY+60,ambient$YPD..MPa.,type="l",lwd=2, col="black", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(-6.5,0), ylab=exp_list[1], xaxt='n')
lines(drought$DAY+60,drought$YPD..MPa., lwd=2, col="blue")
box(lwd=2)
Axis(side=1, labels=FALSE)
plot(ambient$DAY+60,ambient$YPD..MPa.,type="l",lwd=2, col="black", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(-6.5,0), ylab=exp_list[1], xaxt='n')
lines(ambient_noGW$DAY+60,ambient_noGW$YPD..MPa., lwd=2, col="darkgreen")
legend("bottomleft",lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c("black","blue","darkgreen","darkred"),
       c("Ambient","Drought","No bedrock groundwater","Lower vulnerability"), cex=1.5, bty="n")
box(lwd=2)
Axis(side=1, labels=FALSE)
plot(ambient$DAY+60,ambient$YPD..MPa.,type="l",lwd=2, col="black", cex.axis=1.5, cex.lab=1.5, 
     ylim=c(-6.5,0), ylab=exp_list[1])
lines(ambient_run2$DAY+60,ambient_run2$YPD..MPa., lwd=2, col="darkred")
mtext("Day", side=1, line=3, cex=1.0)
box(lwd=2)

dev.off()

#
#Plot transpiration (Ec) and critical transpiration (Ecrit)
#
pdf("plots/Pinon_transpiration.pdf", width = 5, height= 7, useDingbats = F)

layout(matrix(1:3, ncol = 1, nrow=3), widths = rep(1, 3), heights = c(1.2,1.2,1.2), respect = FALSE)
par(oma=c(6.1,1.1,1,.1))
par(mar = c(0, 4.8, 0.0, 2.1))
plot(ambient$DAY+60,ambient$ECRIT..mmol.m.2.s.1.,type="l",lwd=2, lty= 2, 
     col="black", cex.axis=1.5, cex.lab=1.5, ylim=c(0,2.4), ylab=exp_list[2], xaxt='n')
lines(ambient$DAY+60,ambient$EC..mmol.m.2.s.1., lwd=2, col="black")
lines(drought$DAY+60,drought$ECRIT..mmol.m.2.s.1., lwd=2, lty=2, col="blue")
lines(drought$DAY+60,drought$EC..mmol.m.2.s.1., lwd=2, col="blue")
box(lwd=2)
Axis(side=1, labels=FALSE)
plot(ambient$DAY+60,ambient$ECRIT..mmol.m.2.s.1.,type="l",lwd=2, lty= 2, 
     col="black", cex.axis=1.5, cex.lab=1.5, ylim=c(0,2.4), ylab=exp_list[2], xaxt='n')
lines(ambient$DAY+60,ambient$EC..mmol.m.2.s.1., lwd=2, col="black")
lines(ambient_noGW$DAY+60,ambient_noGW$ECRIT..mmol.m.2.s.1., lty=2, lwd=2, col="darkgreen")
lines(ambient_noGW$DAY+60,ambient_noGW$EC..mmol.m.2.s.1., lwd=2, col="darkgreen")
legend("topright",lty=c(1,1,1,1),lwd=c(2,2,2,2),col=c("black","blue","darkgreen","darkred"),
       c("Ambient","Drought","No bedrock groundwater","Lower vulnerability"), cex=1.25, bty="n")
box(lwd=2)
Axis(side=1, labels=FALSE)
plot(ambient$DAY+60,ambient$ECRIT..mmol.m.2.s.1.,type="l",lwd=2, lty= 2, 
     col="black", cex.axis=1.5, cex.lab=1.5, ylim=c(0,8.4), ylab=exp_list[2])
lines(ambient$DAY+60,ambient$EC..mmol.m.2.s.1., lwd=2, col="black")
lines(ambient_run2$DAY+60,ambient_run2$ECRIT..mmol.m.2.s.1., lty=2, lwd=2, col="darkred")
lines(ambient_run2$DAY+60,ambient_run2$EC..mmol.m.2.s.1., lwd=2, col="darkred")
mtext("Day", side=1, line=3, cex=1.0)
box(lwd=2)

dev.off()
