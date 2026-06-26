########################################################################################
#Comparison between LINTUL1 and 2 as programmed in FST (as on models.pps.wur.nl website)
#
#Author: AGT Schut, March 2021
#PLant Production Systems group, Wageningen University
#
#FST: Note that LINTUL1 and LINTUL2 with irrigation are NOT exactly the same
#     if PTRAN = 0, then growth is also 0. In LINTUL growth can be > 0.
#     This occurs on very wet days when 0.25 * intercepted rain > PTRAN.
#
#The comparison is made for clay soil with below given WC and RD of 1.2 m.
#The compared years vary from wet, normal to very dry (2018)
#
#Yields in t/ha for irrigated (IRR)and rainfed (RF) conditions for LINTUL1 and LINTUL2
#models as implemented in the Fortran Simulation Translator (FST).
#
#------FST------------------  R-version-----
#Year  L1     L2-IRR  L2-RF   L2-IRR  L2-RF
#2018: 9.2508 9.2508  4.4072  9.2508  4.4072
#2011: 8.7316 8.6191  6.1895  8.6191  8.1895
#2005: 8.5193 8.4867  8.3788  8.4867  8.3788
#1999: 9.4480 9.3842  8.5957  9.3842  8.5957
#
######################################################################################

library('this.path')

#Get path of current script
wd <- here()
#Set working directory to script path
setwd(wd)


FST_WSOTHA <- data.frame(year       = c(  2018,  2011,  2005,  1999),
                         LINTUL1    = c(9.2505,8.7316,8.5193,9.4480),
                         LINTUL2IRR = c(9.2508,8.6191,8.4867,9.3842),
                         LINTUL2RF  = c(4.4072,6.1895,8.3788,8.5957))


source('./Components/LINTUL_parameters.r')	
source('./Components/LINTUL_get_weather.r')	
source('./Components/lINTUL1_model.r')	
source('./Components/LINTUL2_functions.r')	
source('./Components/lINTUL2_model.r')	
require('deSolve')    #used for solving ODEs

#---------------------------------Run control---------------------------------------#
#                                                                                   #
#-----------------------------------------------------------------------------------#
wdirectory <- "./weather/"
country   <- "NLD"
station   <- "1"
STTIME  <- 30         # d     :     start time of simulation
FINTIM  <- 300       # d     :     finish time 
DELT    <- 1         # d     :     time step 
##=====================================================================================##

COM_RESULTS_POT=NULL
COM_RESULTS_IRRI_CLAY=NULL
COM_RESULTS_WLIM_CLAY=NULL

#Get the parameters for springwheat and default soil with 1.2m rooting depth
pclay <- LINTUL2_PARAMETERS(irri=FALSE,soiltype="default",crop="springwheat")
pclayIRR <- LINTUL2_PARAMETERS(irri=TRUE,soiltype="default",crop="springwheat")


for(year in c(2018,2011,2005,1999)){
  yr=toString(year)
  #Add reference value as original FST version does not have CO2 adjusted RUE
  pclay_RUE <- ADD_LUE_TO_PARAMETER_LIST_CO2VALUE(Pars = pclay, ppmCO2 = 350)
  pclayIRR_RUE <- ADD_LUE_TO_PARAMETER_LIST_CO2VALUE(Pars = pclayIRR, ppmCO2 = 350)

  #RESET TO REFERENCE VALUE FOR COMPARISONS  
  #pclay_RUE$LUE <- pclay_RUE$LUEREF
  #pclayIRR_RUE$LUE <- pclayIRR_RUE$LUEREF
  
    
  #Load weather data
  wdata <- get_weather(directory=wdirectory ,country=country ,station=station,year=substr(yr,2,4))
  #LINTUL 1: POTENTIAL PRODUCTION
  results_pot <- ode(LINTUL1_iniSTATES(pclay), 
                   seq(STTIME, FINTIM, by = DELT), 
                   LINTUL1, 
                   pclayIRR_RUE,
                   WDATA = wdata,
                   method = "euler")
  results_pot<-as.data.frame(results_pot)
  
  #LINTUL2 with irrigation
  state_irrig_clay <- ode(LINTUL2_iniSTATES(pclay), 
                   seq(STTIME, FINTIM, by = DELT), 
                   LINTUL2, 
                   pclayIRR_RUE,
                   WDATA = wdata,
                   method = "euler")
  state_irrig_clay<-as.data.frame(state_irrig_clay)
  
  #LINTUL2 without irrigation: water limited
  state_wlim_clay <- ode(LINTUL2_iniSTATES(pclay), 
                    seq(STTIME, FINTIM, by = DELT), 
                    LINTUL2, 
                    pclay_RUE,
                    WDATA = wdata,
                    method = "euler")
  state_wlim_clay<-as.data.frame(state_wlim_clay)
  #Add year
  results_pot[,"year"] = year
  state_irrig_clay[,"year"] = year
  state_wlim_clay[,"year"] = year
  
  #Write results to file
  write.csv(results_pot,file=paste0("./Results/Run details for LINTUL2 with irrigation for ",yr, ".csv",sep = "", collapse = NULL))
  write.csv(state_wlim_clay,file=paste0("./Results/Run details for LINTUL2 without irrigation for clay for ",yr, ".csv",sep = "", collapse = NULL))
  #add these to the data frame with multi-year values
  COM_RESULTS_POT <-rbind(COM_RESULTS_POT, results_pot)
  COM_RESULTS_IRRI_CLAY <- rbind(COM_RESULTS_IRRI_CLAY,state_irrig_clay)
  COM_RESULTS_WLIM_CLAY <-rbind(COM_RESULTS_WLIM_CLAY, state_wlim_clay)
}
#Compare yields for years at DOY 300
fdCOM_RESULTS_POT       <-subset(COM_RESULTS_POT      , time==300, select = c("WSOTHA"))
fdCOM_RESULTS_IRRI_CLAY <-subset(COM_RESULTS_IRRI_CLAY, time==300, select = c("WSOTHA"))
fdCOM_RESULTS_WLIM_CLAY <-subset(COM_RESULTS_WLIM_CLAY, time==300, select = c("WSOTHA"))

#Show the results in the console
print(rbind(c("","FST L1", "FST L2", "FST L2", "R-L1", "R-L2-IRRI", "R-L2-RF"),
      cbind(FST_WSOTHA,
            round(fdCOM_RESULTS_POT,4), 
            round(fdCOM_RESULTS_IRRI_CLAY,4), 
            round(fdCOM_RESULTS_WLIM_CLAY,4))
      ))


