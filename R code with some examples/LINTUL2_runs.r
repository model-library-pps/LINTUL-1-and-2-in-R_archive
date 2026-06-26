#INSTRUCTIONS:
#Ensure to set the working directory to the scripts folder, i.e the current location (under Session)
#Output is added to the results folder.

source('./Components/LINTUL_parameters.r')	
source('./Components/LINTUL_get_weather.r')	
source('./Components/LINTUL2_functions.r')	
source('./Components/lINTUL2_model.r')	
require('deSolve')    #used for solving ODEs

#Filenames of figures
FigA.PDF <- "./Figures/Weight of roots stem leaves storage organs.pdf"
FigA.TIF <- "./Figures/Weight of roots stem leaves storage organs.tif"
FigB.PDF <- "./Figures/LAI soil water cumulative rain and irrigation.pdf"
FigB.TIF <- "./Figures/LAI soil water cumulative rain and irrigation.tif"
FigC.PDF <- "./Figures/WUE and HI.pdf"
FigC.TIF <- "./Figures/WUE and HI.tif"

#---------------------------------Run control---------------------------------------#
#                                                                                   #
#-----------------------------------------------------------------------------------#
#GENERAL SETTINGS
wdirectory <- "./weather/"
country   <- "NLD"
station   <- "1"
SOILTYPE="sand" #only sand or clay can be selected at this moment
STTIME  <- 30        # d     :     start time of simulation
FINTIM  <- 300       # d     :     finish time 
DELT    <- 1         # d     :     time step 
YR      <- 1974      # y     :     year to simulate
#-----------------------------------------------------------------------------------#

#Load weather data for selected year
wdata <- get_weather(directory=wdirectory ,country=country ,station=station,year=substr(toString(YR),2,4))

#solve differential equations with EULER
#LINTUL with irrigation: POTENTIAL PRODUCTION
LintulParms <- LINTUL2_PARAMETERS(irri=TRUE,soiltype=SOILTYPE,crop="springwheat")
results_pot <- ode(y = LINTUL2_iniSTATES(LintulParms), 
                 time = seq(STTIME, FINTIM, by = DELT), 
                 func = LINTUL2, 
                 parms = LintulParms,
                 WDATA = wdata,
                 method = "euler")
#Write to file
write.csv(data.frame(results_pot),file=paste0("./Results/Run details for LINTUL2 with irrigation for ",YR, ".csv",sep = "", collapse = NULL))

#LINTUL without irrigation: water limited production
LintulParms <- LINTUL2_PARAMETERS(irri=FALSE,soiltype=SOILTYPE,crop="springwheat")
results_wlim <- ode(LINTUL2_iniSTATES(LintulParms), 
                 seq(STTIME, FINTIM, by = DELT), 
                 LINTUL2, 
                 parms = LintulParms,
                 WDATA = wdata,
                 method = "euler")
write.csv(data.frame(results_wlim),file=paste0("./Results/Run details for LINTUL2 without irrigation for ",YR, ".csv",sep = "", collapse = NULL))

FigureA <- function(results_pot,results_wlim){
  par(mfrow=c(2,2),mar=c(2,4,1,1))
  plot(results_pot,results_wlim,select=c("WRT","WST","WLV","WSO"),
       ylab ="gDM m-2",xlim=c(30,300),col=c("black","red"),lwd=c(1.5,2),lty=c(1,2))
  legend("topleft",legend=c("Irrigated","Rainfed"),col=c("black","red"),lwd=c(1.5,2),lty=c(1,2),bty="n")
}
FigureB <- function(results_pot,results_wlim){
  par(mfrow=c(2,2),mar=c(2,4,1,1))
  plot(results_pot,results_wlim,select=c("LAI","TRAIN","TRAN","EVAP"),xlim=c(30,300),
       col=c("black","red"),lwd=c(1.5,2),lty=c(1,2))
  legend("topleft",legend=c("Irrigated","Rainfed"),col=c("black","red"),lwd=c(1.5,2),lty=c(1,2),bty="n")
}
FigureC <- function(results_pot,results_wlim){
  par(mfrow=c(2,2),mar=c(2,4,1,1))
  #kg/m3 is the same as g/l or g m-2 * m2 l-2
  plot(results_pot[,"time"], results_pot[,"WSO"]/(results_pot[,"EVAP"]+results_pot[,"TRAN"]),ylim=c(0,3),
       type="l",ylab = "WUE, kg yield/m3",xlab = "Time, d", col="black",lwd=1.5,lty=1)
  lines(results_wlim[,"time"], results_wlim[,"WSO"]/(results_wlim[,"EVAP"]+results_wlim[,"TRAN"]),
       col="red",lwd=2,lty=2)
  plot(results_pot[,"time"],results_pot[,"HI"],ylim=c(0,0.8),
       type="l",ylab = "HI, kg yield/kg biomass",xlab = "Time, d", col="black",lwd=1.5,lty=1)
  lines(results_wlim[,"time"],results_wlim[,"HI"], col="red",lwd=2,lty=2)
  legend("topleft",legend=c("Irrigated","Rainfed"),col=c("black","red"),lwd=c(1.5,2),lty=c(1,2),bty="n")
}

#plot state variables
windows(xpos=-25)
FigureA(results_pot,results_wlim)
windows(xpos=-525)
FigureB(results_pot,results_wlim)
windows(xpos=-1025)
FigureC(results_pot,results_wlim)


#Save as PDF and TIF
pdf(FigA.PDF)
  FigureA(results_pot,results_wlim)
dev.off()

tiff(FigA.TIF, height = 16, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
  FigureA(results_pot,results_wlim)
dev.off()

pdf(FigB.PDF)
  FigureB(results_pot,results_wlim)
dev.off()

tiff(FigB.TIF, height = 16, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
  FigureB(results_pot,results_wlim)
dev.off()

pdf(FigC.PDF)
  FigureC(results_pot,results_wlim)
dev.off()

tiff(FigC.TIF, height = 13, width = 16, units = 'cm', 
     compression = "lzw", res = 600)
  FigureC(results_pot,results_wlim)
dev.off()

