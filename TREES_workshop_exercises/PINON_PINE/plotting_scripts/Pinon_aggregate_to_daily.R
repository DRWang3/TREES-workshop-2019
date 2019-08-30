#
#TREES workshop plotting routines
#  This script is used first to convert half-hourly simulation output to daily
#
#ti	simET	WPlant_K	Soil_Psi	Leaf_Psi	Psi_Crit	Ecrit	Ec	
#RhizFlux0	RhizFlux1	RhizFlux2	RhizFlux3	RhizFlux4	
#Gs	LAI	liveLAI	Rmaint	Rgrowth	leafNSC	stemNSC	rootNSC	
#chloroStarch	chloroSugar waterStress	litterH2O	
#theta0	theta1	theta2	theta3	theta4	thetaRoot	
#Can_Evap	Snowpack	SnowEdef	Vcmax25	Vcmax_sun	Vcmax_shd	
#Jmax25	J_sun	J_shd	Asun	Ashd	Lsun	Lshd	Tsun	Tshd	
#Dsun	Dshd	Ci_sun	Ci_shd	PARsun	PARshd	gs_sun	gs_shd	
#NEE	NPP	R_total	R_ag	R_bg	Rd_sun	Rd_shd	Csapwood	
#FibRootC0	FibRootC1	FibRootC2	FibRootC3	FibRootC4	
#FineRootC0 FineRootC1	FineRootC2	FineRootC3	FineRootC4	
#TotRootC0	TotRootC1	TotRootC2	TotRootC3	TotRootC4	
#FineRootCN0	FineRootCN1	FineRootCNFineRootCN3	FineRootCN4	
#LeafCN	humusC0	humusC1	humusC2	humusC3	humusC4	
#RhizCl0	RhizCl1	RhizCl2	RhizCl3	RhizCl4	
#RhizNl0	RhizNl1	RhizNl2	RhizNl3	RhizNl4	
#AAexudateC0	AAexudateC1	AAexudateC2	AAexudateC3	AAexudateC4	
#SugarExudateC0	SugarExudateC1	SugarExudateC2	SugarExudateC3	SugarExudateC4	
#MicrobC0	MicrobC1	MicrobC2	MicrobC3	MicrobC4	
#MicrobN0	MicrobN1	MicrobN2	MicrobN3	MicrobN4	
#RhizN-	RhizN+	PlantN	PlantNstat	RL ar0	ar1	ar2	ar3	ar4	
#
computeDaily = function(fname, simulation, drivers, Ksat, SLA)
{
  simulation$WPlant_K <- as.matrix((simulation$WPlant_K))
  simulation[simulation$WPlant_K >Ksat, "WPlant_K"] <- Ksat
  simulation$PLC <- as.matrix((100*(1-simulation$WPlant_K/Ksat)))
  simulation$L <- as.matrix((simulation$Lsun)+(simulation$Lshd))
  simulation$D1 <- as.matrix((simulation$Lsun)*(simulation$Dsun))
  simulation$D2 <- as.matrix((simulation$Lshd)*(simulation$Dshd))
  simulation$DCAN <- as.matrix(((simulation$D1)+(simulation$D2))/(simulation$L))
  simulation$YSOIL <- as.matrix((simulation$Soil_Psi))
  simulation$YPD <- as.matrix((simulation$Leaf_Psi))
  simulation$YMD <- as.matrix((simulation$Leaf_Psi))
  simulation$ECRIT <- as.matrix((simulation$Ecrit))
  simulation$NSC <- as.matrix((simulation$leafNSC)+(simulation$stemNSC)+(simulation$rootNSC)
                              +(simulation$chloroStarch)+(simulation$chloroSugar))
  simulation$NSCl <- as.matrix(((simulation$leafNSC)+(simulation$chloroStarch)+(simulation$chloroSugar))/
                                 (max((simulation$liveLAI),(simulation$LAI))/SLA*10000))
  simulation$NSCs <- as.matrix((simulation$stemNSC)/(simulation$Csapwood))
  simulation$GS <- as.matrix((simulation$Gs))
  simulation$EC <- as.matrix((simulation$Ec))
  simulation$GPP <- as.matrix((simulation$Asun)*(simulation$Lsun)+(simulation$Ashd)*(simulation$Lshd))
  simulation$SM01 <- as.matrix(((simulation$theta0)*(simulation$ar0)+(simulation$theta1)*(simulation$ar1))/
                                 ((simulation$ar0)+(simulation$ar1)))
  simulation$SM2 <- as.matrix((simulation$theta2))
  simulation$SM34 <- as.matrix(((simulation$theta3)*(simulation$ar3)+(simulation$theta4)*(simulation$ar4))/
                                 ((simulation$ar3)+(simulation$ar4)))
  simulation$RF01 <- as.matrix((simulation$RhizFlux0)+(simulation$RhizFlux1))
  simulation$RF2 <- as.matrix((simulation$RhizFlux2))
  simulation$RF34 <- as.matrix((simulation$RhizFlux3)+(simulation$RhizFlux4))
  simulation$FineRC01 <- as.matrix((simulation$FibRootC0)+(simulation$FibRootC1)+(simulation$FineRootC0)+(simulation$FineRootC1))
  simulation$FineRC2 <- as.matrix((simulation$FibRootC2)+(simulation$FineRootC2))
  simulation$FineRC34 <- as.matrix((simulation$FibRootC3)+(simulation$FibRootC4)+(simulation$FineRootC3)+(simulation$FineRootC4))
  simulation$TotalRC01 <- as.matrix((simulation$TotRootC0)+(simulation$TotRootC1))
  simulation$TotalRC2 <- as.matrix((simulation$TotRootC2)+(simulation$TotRootC2))
  simulation$TotalRC34 <- as.matrix((simulation$TotRootC3)+(simulation$TotRootC4))
  simulation$FineRootC <- as.matrix((simulation$FineRC01)+(simulation$FineRC2)+(simulation$FineRC34))
  simulation$TotalRootC <- as.matrix((simulation$TotalRC01)+(simulation$TotalRC2)+(simulation$TotalRC34))
  simulation$RootC <- as.matrix((simulation$TotalRootC))
  simulation$NSCr <- as.matrix((simulation$rootNSC)/(simulation$RootC))
  simulation$MicrobN01 <- as.matrix((simulation$MicrobN0)+(simulation$MicrobN1))
  simulation$MicrobN2 <- as.matrix((simulation$MicrobN2))
  simulation$MicrobN34 <- as.matrix((simulation$MicrobN3)+(simulation$MicrobN4))
  simulation$RhizNl01 <- as.matrix((simulation$RhizNl0)+(simulation$RhizNl1))
  simulation$RhizNl2 <- as.matrix((simulation$RhizNl2))
  simulation$RhizNl34 <- as.matrix((simulation$RhizNl3)+(simulation$RhizNl4))
  simulation$AR01 <- as.matrix((simulation$ar0)+(simulation$ar1))
  simulation$AR2 <- as.matrix((simulation$ar2))
  simulation$AR34 <- as.matrix((simulation$ar3)+(simulation$ar4))
  
  DCAN_conversion <- 1.0/4.0
  PLC_conversion <- 1.0/4.0
  YSOIL_conversion <- 1.0/4.0
  YPD_conversion <- 1.0/4.0
  YMD_conversion <- 1.0/4.0
  ECRIT_conversion <- 1.0/48.0
  leafNSC_conversion <- 1.0/4.0/10.0
  stemNSC_conversion <- 1.0/4.0/10.0
  rootNSC_conversion <- 1.0/4.0/10.0
  NSCr_conversion <- 1.0/4.0*100.0
  GS_conversion <- 1.0/4.0*1000
  EC_conversion <- 1.0/48.0
  GPP_conversion <- 1.0/48.0
  SM01_conversion <- 1.0/48.0
  SM2_conversion <- 1.0/48.0
  SM34_conversion <- 1.0/48.0
  RF01_conversion <- 1.0/48.0
  RF2_conversion <- 1.0/48.0
  RF34_conversion <- 1.0/48.0
  FineRC01_conversion <- 1.0/48.0/10.0
  FineRC2_conversion <- 1.0/48.0/10.0
  FineRC34_conversion <- 1.0/48.0/10.0
  MicrobN01_conversion <- 1.0/48.0/10.0
  MicrobN2_conversion <- 1.0/48.0/10.0
  MicrobN34_conversion <- 1.0/48.0/10.0
  RhizNl01_conversion <- 1.0/48.0/10.0
  RhizNl2_conversion <- 1.0/48.0/10.0
  RhizNl34_conversion <- 1.0/48.0/10.0
  PlantN_conversion <- 1.0/48.0/10.0
  LAI_conversion <- 1.0/48.0
  RLA_conversion <- 1.0/48.0
  AR01_conversion <- 1.0/48
  AR2_conversion <- 1.0/48.0
  AR34_conversion <- 1.0/48.0
  
  flux_list<-c("DCAN","PLC","YSOIL","YPD","YMD","ECRIT","leafNSC","stemNSC","rootNSC","GS","EC","GPP",
               "SM01","SM2","SM34","RF01","RF2","RF34","FineRC01","FineRC2","FineRC34",
               "MicrobN01","MicrobN2","MicrobN34","RhizNl01","RhizNl2","RhizNl34","PlantN",
               "LAI","RLA","AR01","AR2","AR34")
  
#start and end times
  daylow <- c(12,12,12,3.5,12,0,12,12,12,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  dayhigh <- c(13.5,13.5,13.5,5,13.5,23.5,13.5,13.5,13.5,13.5,23.5,23.5,
               23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5,
               23.5, 23.5, 23.5,23.5,23.5,23.5,23.5,23.5,23.5,23.5)
  
#allocate array for output
  nrows <- length(simulation[,1])/48
  result_array <- array(data=0,dim=c(nrows,34))
  for(i in 1:nrows)
  {
    result_array[i,1] <- i
  }
  
  for(j in 1:length(flux_list))
  {
#Binding together the driver file and simulation columns
    flux_bound<-cbind(drivers[,1:2],simulation[,flux_list[j]])
    colnames(flux_bound)<-c("jday","Hour","simulated")
    midday_flux<-subset(flux_bound,flux_bound$Hour>=daylow[j] & flux_bound$Hour <=dayhigh[j])
#Aggregating to a daily timestep
    flux_agg<-aggregate(midday_flux,by=list(midday_flux$jday),FUN=sum,na.rm=TRUE)
#Making the proper conversion 
    flux_agg$simulated<-flux_agg$simulated*get(paste(flux_list[j],"_conversion",sep=""))
    result_array[,j+1]<-as.vector(flux_agg$simulated)
  }
  
  colnames(result_array)<-c("DAY","DCAN (kPa)","PLC","YSOIL (MPa)","YPD (MPa)", "YMD (MPa)", 
                            "ECRIT (mmol m-2 s-1)","Leaf NSC (gC m-2)","Stem NSC (gC m-2)","Root NSC (gC m-2)",
                            "GS (mmol m-2 s-1)","EC (mmol m-2 s-1)","GPP (mmol m-2 s-1)",
                            "SMshall","SMdeep","SMrock",
                            "RHIZFshall (mmol m-2 s-1)","RHIZFdeep (mmol m-2 s-1)",
                            "RHIZFrock (mmol m-2 s-1)","FinRootCshall (gC m-2 grd)",
                            "FinRootCdeep (gC m-2 grd)","FinRootCrock (gC m-2 grd)",
                            "MicrobNshall (gN m-2 grd)","MicrobNdeep (gN m-2 grd)","MicrobNrock (gN m-2 grd)",
                            "LitterNshall (gN m-2 grd)","LitterNdeep (gN m-2 grd)","LitterNrock (gN m-2 grd)",
                            "PlantN (gN m-2 grd)", "LAI (m2 m-2)", "RLA (m2 m-2)",
                            "RA shall (m2 m-2","RA deep (m2 m-2)","RA rock (m2 m-2)")
  write.csv(result_array, file=paste("outputs/",fname,"_midday.csv"))
}

