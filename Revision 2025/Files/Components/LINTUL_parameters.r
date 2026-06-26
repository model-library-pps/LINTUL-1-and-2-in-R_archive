#---------------------------------------------------------------------#
# FUNCTION LINTUL1_PARAMETERS_SPRINGWHEAT                             #
# Purpose: Listing the default input parameters for Lintul1           #
# Spring Wheat, based on report from Spitters, 1990                   #
#---------------------------------------------------------------------#
LINTUL1_PARAMETERS_SPRINGWHEAT <-function() {
  #All defaults
  return(c( DELT    = 1.0,    # Day          
            LAII    = 0.012,  # m2 m-2      :     initial leaf area index 
            LAIJUV  = 0.75,   # m2 m-2      :     LAI threshold for LAI growth determined by temperature
            SLA     = 0.022,  # m2 g-1      :     specific leaf area 
            TBASE   = 0,      # deg. C      :     base temperature 
            LUEREF  = 3.0,    # g MJ-1      :     reference light use efficiency
            CO2REF  = 350,    # ppm         :     reference CO2 level where LUE equals LUEREF 
            K 	    = 0.6,    # (-)         :     extinction coefficient for PAR
            DOYEM   = 32,     # d           :     day number at crop emergence. 
            RGRL    = 0.009,  # 1/(deg. C d):     relative growth rate of LAI during exponential growth
            TSUMJUV = 330,    # deg. C      :     temperature sum threshold for leaf growth determined by temperature
            TSUMAN  = 1110,   # deg. C      :     temperature sum at anthesis
            FINTSUM = 2080,   # deg. C      :     temperature sum at which simulation stops
            LAICR   = 4,      # m2 m-2      :     critical LAI
            RDRSHM  = 0.03,   # d-1         :     max relative death rate of leaves due to shading
            FPAR    = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
            
            #Filename of file with CO2 concentrations per month
            FILENAME_CO2 = './ppmCO2/Measured CO2 at Mauna Loa in Hawai.csv',
            
            #Look-up tables   oC,  RDR
            RDRT    = data.frame(TEMP=c(  -1,   10,   15,   30,   50),
                                 RDR=c(0.03, 0.03, 0.04, 0.09, 0.09)),
            #Partitioning    TSUM,  fraction of daily growth to roots: FRT as a function of TSUM
            FRTTB = data.frame(TSUM=c(   0,  110,  275,  555,  780, 1055, 1160, 1305, 2080, 2500),
                               DVS =c(   0, 0.10, 0.25, 0.50, 0.70, 0.95, 1.05, 1.20,  2.0,  2.0),
                               FRT =c(0.50, 0.50, 0.34, 0.12, 0.07, 0.03, 0.02,  0.0,  0.0,  0.0)),  
            #Partitioning    TSUM,  fraction of daily growth to leaves
            FLVTB = data.frame(TSUM=c(   0,  110,  275,  555,  780, 1055, 1160, 1305, 2080, 2500),
                               DVS =c(   0, 0.10, 0.25, 0.50, 0.70, 0.95, 1.05, 1.20,  2.0,  2.0),
                               FLV =c(0.33, 0.33,0.46,  0.44, 0.14,  0.0,  0.0,  0.0, 0.0,   0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to stem
            FSTTB = data.frame(TSUM=c(   0,  110,  275,  555,  780, 1055, 1160, 1305, 2080, 2500),
                               DVS =c(   0, 0.10, 0.25, 0.50, 0.70, 0.95, 1.05, 1.20,  2.0,  2.0),
                               FST =c(0.17, 0.17, 0.20, 0.44, 0.79, 0.97,  0.0,  0.0,  0.0,  0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to storage organs
            FSOTB = data.frame(TSUM=c(   0,  110,  275,  555,  780, 1055, 1160, 1305, 2080, 2500),
                               DVS =c(   0, 0.10, 0.25, 0.50, 0.70, 0.95, 1.05, 1.20,  2.0,  2.0),
                               FSO =c( 0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 0.98,  1.0,  1.0,  1.0)) 
  ))
}

ADD_LUE_TO_PARAMETER_LIST_CO2VALUE <- function(Pars, ppmCO2){
  LUE <- Pars$LUEREF
  if(length(ppmCO2) == 1)  {
    A <- -1.7 / (Pars$CO2REF * (1-1.7))
    fCO2 <- (A * ppmCO2 * 1.7) / (A* ppmCO2 +1.7)
    LUE = Pars$LUEREF * fCO2
  }else if(is.null(ppmCO2)){
    print("No ambient CO2 available, RUE = RUEREF")
    print("LUE is not adjusted to CO2 levels")
  }
  
  Pars$LUE <- LUE
  
  return(Pars)
}

ADD_LUE_TO_PARAMETER_LIST <- function(Pars, YEAR, NRMONTH, Silent = FALSE){
  ppmCO2 = NULL
  #Read in measured CO2 concentrations for all years
  CO2 <- read.csv(Pars$FILENAME_CO2)
  
  LUE <- Pars$LUEREF
  CO2 <- subset(CO2,Year == as.numeric(YEAR) & Month == as.numeric(NRMONTH))
  if(nrow(CO2) == 0){
    print("No ambient CO2 available, RUE = RUEREF")
    print("LUE is not adjusted to CO2 levels")
  }else if(nrow(CO2) == 1){
    ppmCO2 <- CO2[1,"ppmCO2"]
  }
  if(length(ppmCO2) == 1){
    A <- -1.7 / (Pars$CO2REF * (1-1.7))
    fCO2 <- (A * ppmCO2 * 1.7) / (A* ppmCO2 +1.7)
    LUE = Pars$LUEREF * fCO2
  }
  if(!Silent){
    print(paste0("ppmCO2 and RUE value for year: ", YEAR, " in month nr: ", NRMONTH))
    print(c(ppmCO2, LUE))
  }
  
  Pars$LUE <- LUE
  
  return(Pars)
}

#---------------------------------------------------------------------#
# FUNCTION LINTUL2_PARAMETERS_SPRINGWHEAT                             #
# Purpose: Listing the default input parameters for Lintul2           #
# Spring Wheat, based on report from Spitters, 1990                   #
#---------------------------------------------------------------------#
LINTUL2_PARAMETERS_SPRINGWHEAT <-function() {
    #Additional parameters for root growth
    return(c(ROOTDI  = 0.1,    # M           :    initial rooting depth (at crop emergence)
             ROOTDM  = 1.2,    # m           :     maximum rooting depth
             RRDMAX  = 0.012,  # m  d-1      :     max rate increase of rooting depth
             TRANCO  = 8)      # mm d-1      :     transpiration constant (indicating level of drought tolerance)
          )
}

#---------------------------------------------------------------------#
# FUNCTION LINTUL1_PARAMETERS_MAIZE                                   #
# Based on:                                                           #
# Farre, I., M. v. Oijen, P. A. Leffelaar and J. M. Faci (2000).      #
#       "Analysis of maize growth for different irrigation strategies #
#        in northeastern Spain."                                      #
#      European Journal of Agronomy 2000: 225-238.                    #
#---------------------------------------------------------------------#
LINTUL1_PARAMETERS_MAIZE <-function() {
  #All defaults based on Farre et al.
  return(c( DELT    = 1.0,    # Day 
            LAII    = 0.012,  # m2 m-2      :     initial leaf area index 
            SLA     = 0.016,  # m2 g-1      :     specific leaf area 
            TBASE   = 8.,     # deg. C      :     base temperature 
            LUE     = 4.6,    # g MJ-1      :     light use efficiency
            K 	    = 0.6,    # (-)         :     extinction coefficient for PAR
            DOYEM   = 145,    # d           :     day number at crop emergence. 
            RGRL    = 0.009,  # 1/(deg. C d):     relative growth rate of LAI during exponential growth
            TSUMJUV = 0,      # deg. C      :     temperature sum threshold for leaf growth determined by temperature
            TSUMAN  = 1000,   # deg. C      :     temperature sum at anthesis
            FINTSUM = 1750,   # deg. C      :     temperature sum at which simulation stops
            LAICR   = 4,      # m2 m-2      :     critical LAI
            RDRSHM  = 0.03,   # d-1         :     max relative death rate of leaves due to shading
            FPAR    = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
            MMWET   = 0.25,   # mm          :     Rain intercepted per leaf layer
            
            #Look-up tables   oC,  RDR
            RDRT    = data.frame(TEMP=c(-10,   10,   15,   30,   50),
                                 RDR=c(0.00, 0.02, 0.03, 0.05, 0.09)),
            #Partitioning    TSUM,  fraction of daily growth to roots: FRT as a function of TSUM
            FRTTB = data.frame(TSUM=c(   0,  120,  250,  500,  700,  825, 1038, 1375, 1750, 2500),
                               DVS =c(   0, 0.12, 0.25, 0.50, 0.70, 0.83, 1.05, 1.54,  2.0,  2.0),
                               FRT =c(0.50, 0.46, 0.40, 0.30, 0.18, 0.11,  0.0,  0.0,  0.0,  0.0)),  
            #Partitioning    TSUM,  fraction of daily growth to leaves
            FLVTB = data.frame(TSUM=c(   0,  120,  250,  500,  700,  825, 1038, 1375, 1750, 2500),
                               DVS =c(   0, 0.12, 0.25, 0.50, 0.70, 0.83, 1.05, 1.54,  2.0,  2.0),
                               FLV =c(0.31, 0.32, 0.37, 0.45, 0.39, 0.30,  0.0,  0.0, 0.0,   0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to stem
            FSTTB = data.frame(TSUM=c(   0,  120,  250,  500,  700,  825, 1038, 1375, 1750, 2500),
                               DVS =c(   0, 0.12, 0.25, 0.50, 0.70, 0.83, 1.05, 1.54,  2.0,  2.0),
                               FST =c(0.19, 0.22, 0.23, 0.25, 0.40, 0.54, 0.25,  0.0,  0.0,  0.0)), 
            #Partitioning    TSUM,  fraction of daily growth to storage organs
            FSOTB = data.frame(TSUM=c(   0,  120,  250,  500,  700,  825, 1038, 1375, 1750, 2500),
                               DVS =c(   0, 0.12, 0.25, 0.50, 0.70, 0.83, 1.05, 1.54,  2.0,  2.0),
                               FSO =c( 0.0,  0.0,  0.0,  0.0, 0.03, 0.05, 0.75,  1.0,  1.0,  1.0)) 
  ))
}

#---------------------------------------------------------------------#
# FUNCTION LINTUL2_PARAMETERS_MAIZE                                   #
# Purpose: Listing the default input parameters for Lintul2           #
# Grain maize, based on input data MAG202.DATp from LINTUL5           #
# See also report from Spitters and Schapendonk, 1990                 #
#---------------------------------------------------------------------#
LINTUL2_PARAMETERS_MAIZE <-function() {
  #All defaults
  return(c( ROOTDI  = 0.1,    # M           :     initial rooting depth (at crop emergence)
            ROOTDM  = 1.2,    # m           :     maximum rooting depth
            RRDMAX  = 0.05,  # m  d-1       :     max rate increase of rooting depth
            TRANCO  = 1.8)    # mm d-1      :     transpiration constant (indicating level of drought tolerance)
  )
}

LINTUL2_PARAMETERS_SOIL <-function() {
  #All defaults
  return(c( ROOTDI  = 0.1,    # M           :    initial rooting depth (at crop emergence)
            WCI     = 0.36,   # m3 m-3      :     initial soil water content 
            WCAD    = 0.08,   # m3 m-3      :     soil water content at air dryness 
            WCWP    = 0.23,   # m3 m-3      :     soil water content at wilting point
            WCFC    = 0.36,   # m3 m-3      :     soil water content at field capacity 
            WCWET   = 0.48,   # m3 m-3      :     critical soil water content for transpiration reduction due to waterlogging
            WCST    = 0.55,   # m3 m-3      :     soil water content at full saturation 
            DRATE   = 50,     # mm d-1      :     max drainage rate
            IRRIGF  = 0,      # (-)         :     irrigation rate relative to the rate required to keep soil water status at field capacity
            FPAR    = 0.5,    # (-)         :     Fraction PAR, MJ PAR/MJ DTR
            MMWET   = 0.25    # mm         :     Rain intercepted per leaf layer
  ))
}


#---------------------------------------------------------------------#
# FUNCTION adapted parameters                                         #
# Purpose: Listing the input parameters for Lintul2                   #
#---------------------------------------------------------------------#
LINTUL2_PARAMETERS <-function(irri=FALSE,crop="springwheat",soiltype="clay") {
  #get the true defaults
  if(crop == "springwheat"){
    CROP_PARAM_LINTUL1 <- LINTUL1_PARAMETERS_SPRINGWHEAT() 
    CROP_PARAM_LINTUL2 <- LINTUL2_PARAMETERS_SPRINGWHEAT() 
  }
  else if(crop == "maize"){
    CROP_PARAM_LINTUL1 <- LINTUL1_PARAMETERS_MAIZE() 
    CROP_PARAM_LINTUL2 <- LINTUL2_PARAMETERS_MAIZE() 
  }
  else(
    stop("Provide valid crop, choose from springwheat or maize")
  )
  
  SOIL_PARAM <- LINTUL2_PARAMETERS_SOIL()
  
  PARAM <- c(CROP_PARAM_LINTUL1, CROP_PARAM_LINTUL2, SOIL_PARAM)
  
  if(irri==TRUE){ PARAM[["IRRIGF"]] <- 1 }
  
  if(soiltype=="clay"){
    PARAM[["ROOTDM"]]  <- 1.2    # m           :     maximum rooting depth
    PARAM[["WCAD"]]    <- 0.08   # m3 m-3      :     soil water content at air dryness 
    PARAM[["WCWP"]]    <- 0.20   # m3 m-3      :     soil water content at wilting point
    PARAM[["WCFC"]]    <- 0.46   # m3 m-3      :     soil water content at field capacity 
    PARAM[["WCWET"]]   <- 0.49   # m3 m-3      :     critical soil water content for transpiration reduction due to water logging
    PARAM[["WCST"]]    <- 0.52   # m3 m-3      :     soil water content at full saturation 
    PARAM[["DRATE"]]   <- 50     # mm d-1      :     max drainage rate
  }else  if( soiltype=="sand"){
    PARAM[["ROOTDM"]]  <- 0.6    # m           :     maximum rooting depth
    PARAM[["WCAD"]]    <- 0.05   # m3 m-3      :     soil water content at air dryness 
    PARAM[["WCWP"]]    <- 0.08   # m3 m-3      :     soil water content at wilting point
    PARAM[["WCFC"]]    <- 0.16   # m3 m-3      :     soil water content at field capacity 
    PARAM[["WCWET"]]   <- 0.40   # m3 m-3      :     critical soil water content for transpiration reduction due to water logging
    PARAM[["WCST"]]    <- 0.42   # m3 m-3      :     soil water content at full saturation 
    PARAM[["DRATE"]]   <- 50     # mm d-1      :     max drainage rate
  }
  
  return(PARAM)  
}
LINTUL2_PARAMETERS_LUE <-function(irri=FALSE,crop="springwheat",soiltype="clay", YEAR, NRMONTH){
  #pass on parameters
  return(ADD_LUE_TO_PARAMETER_LIST(Pars = LINTUL2_PARAMETERS(irri=irri,crop=crop,soiltype=soiltype),
                                   YEAR = YEAR, NRMONTH = NRMONTH, Silent = FALSE))
}
  

