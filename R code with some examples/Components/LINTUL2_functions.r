#---------------------------------------------------------------------#
#This file contains functions: 
# PENMAN,                     computation of the PENMAN EQUATION 
# EVAPTR,                     to compute actual rates of evaporation and transpiration
# GLA,                        to computes daily increase of leaf area index  
# DRUNIR,                     to compute rates of drainage, runoff and irrigation
# Translated from FST code by R. van den Beuken
#
# Author:       AGT Schut, R van den Beuken
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         10-02-2020
#---------------------------------------------------------------------#

#---------------------------------------------------------------------#
# SUBROUTINE PENMAN                                                   #
# Purpose: Computation of the PENMAN EQUATION                         #
#---------------------------------------------------------------------#

penman <-function(DAVTMP,VP,DTR,LAI,WN,RNINTC) {
  
  DTRJM2 <-DTR * 1E6        # J m-2 d-1    :    Daily radiation in Joules 
  BOLTZM <-5.668E-8 	      # J m-1 s-1 K-4:    Stefan-Boltzmann constant 
  LHVAP  <-2.4E6            # J kg-1       :    Latent heat of vaporization 
  PSYCH  <-0.067            # kPa deg. C-1 :    Psychrometric constant
  
  BBRAD  <-BOLTZM * (DAVTMP+273)^4 * 86400                 # J m-2 d-1   :     Black body radiation 
  SVP    <-0.611 * exp(17.4 * DAVTMP / (DAVTMP + 239))     # kPa         :     Saturation vapour pressure
  SLOPE  <-4158.6 * SVP / (DAVTMP + 239)^2                 # kPa dec. C-1:     Change of SVP per degree C
  RLWN   <-BBRAD * pmax(0, 0.55 * (1 - VP / SVP))     # J m-2 d-1   :     Net outgoing long-wave radiation
  WDF    <-2.63 * (1.0 + 0.54 * WN)                  # kg m-2 d-1  :     Wind function in the Penman equation
  
  # Net radiation (J m-2 d-1) for soil (1) and crop (2)
  NRADS  <-DTRJM2 * (1 - 0.15) - RLWN     # (1)
  NRADC  <-DTRJM2 * (1 - 0.25) - RLWN     # (2)
  
  # Radiation terms (J m-2 d-1) of the Penman equation for soil (1) and crop (2)
  PENMRS <-NRADS * SLOPE / (SLOPE + PSYCH)    # (1)
  PENMRC <-NRADC * SLOPE / (SLOPE + PSYCH)    # (2)
  
  # Drying power term (J m-2 d-1) of the Penman equation
  PENMD  <-LHVAP * WDF * (SVP - VP) * PSYCH / (SLOPE + PSYCH)
  
  # Potential evaporation and transpiration are weighed by a factor representing the plant canopy (exp(-0.5 * LAI)).
  PEVAP  <-exp(-0.5 * LAI)  * (PENMRS + PENMD) / LHVAP
  PTRAN  <-(1 - exp(-0.5 * LAI)) * (PENMRC + PENMD) / LHVAP
  PTRAN  <-pmax(0, PTRAN - 0.5 * RNINTC)                        # Potential transpiration is corrected for leaf wetness. The value of 0.5 the amount of rain intercepted is taken from Singh & Sceicz (1979).
  
  
  PENM = data.frame(cbind(PEVAP,PTRAN))
  
  return(PENM)
}

#---------------------------------------------------------------------#
# SUBROUTINE EVAPTR                                                   #
# Purpose: To compute actual rates of evaporation and transpiration   #
#---------------------------------------------------------------------#

evaptr <-function(PEVAP,PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,TRANCO,DELT) {
  
  # Soil water content (m3 m-3) and the amount of soil water (mm) at air dryness (AD) and field capacity (FC).     
  WC   <- 0.001 * WA / ROOTD
  WAAD <- 1000 * WCAD * ROOTD
  WAFC <- 1000 * WCFC * ROOTD
  
  # Evaporation is decreased when water content is below field capacity, but continues until WC = WCAD.
  #Ensure to stay within 0-1 range
  EVAP <- PEVAP * pmin(1, pmax(0, (WC-WCAD) / (WCFC-WCAD) ))
  
  # A critical soil water content (m3 m-3) is calculated below which transpiration is reduced.
  WCCR <- WCWP + PTRAN/(PTRAN+TRANCO) * (WCFC-WCWP)
  
  # If water content is below the critical soil water content a correction factor is calculated that reduces 
  # transpiration until it stops at WC == WCWP.
  FR <- ifelse((WCCR - WCWP) == 0, 1, (WC-WCWP) / (WCCR - WCWP))
  
  # If water content is above the critical soil water content a correction factor is calculated that reduces 
  # transpiration when the crop is hampered by waterlogging (WC > WCWET).
  FRW <- (WCST-WC) / (WCST - WCWET)
  
  #Replace values for wet days with high WC values, above WCCR 
  FR[WC > WCCR] <- FRW[WC > WCCR]
  
  #Ensure to stay within the 0-1 range
  FR=pmin(1,pmax(0,FR))
  
  TRAN <- PTRAN * FR
  
  aux <- EVAP+TRAN
  aux[aux <= 0] <- 1
  
  # A final correction term is calculated to reduce evaporation and transpiration when evapotranspiration exceeds 
  # the amount of water in soil present in excess of air dryness.
  AVAILF <- pmin(1, (WA-WAAD)/(DELT*aux))
  
  EVA <- data.frame(EVAP = EVAP * AVAILF,
                    TRAN = TRAN * AVAILF)
  return(EVA)
}     

# ---------------------------------------------------------------------#
#  SUBROUTINE GLAL2                                                      #
#  Purpose: This subroutine computes daily increase of leaf area index #
#           (ha leaf/ ha ground/ d)                                    #
# ---------------------------------------------------------------------#

glaL2 <-function(TIME,DTEFF,TSUM,LAI,RWLV,TRANRF,WC, WCWP, PARS) {
  with(as.list(PARS),{
    
    # Growth during maturation stage:
    GLAI <-SLA * RWLV
    
    # Growth during juvenile stage:
    if(TSUM < TSUMJUV && LAI < LAIJUV) {
      GLAI <- LAI * (exp(RGRL * DTEFF * DELT) - 1) / DELT * TRANRF
    }
    
    # Growth at day of seedling emergence:
    if(TIME >= DOYEM && LAI == 0 && WC > WCWP) {
      GLAI <- LAII / DELT
    }
    
    # Growth before seedling emergence:
    if(TIME < DOYEM) {
      GLAI <-0
    }
    
    return(GLAI)
  })
}


#-------------------------------------------------------------------------------------------------#
# FUNCTION DRUNIR
# Purpose: To compute rates of drainage, runoff and irrigation
#-------------------------------------------------------------------------------------------------#

drunir <- function(RAIN,RNINTC,EVAP,TRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST) {
  
  # Soil water content (m3 m-3) and the amount of soil water (mm) at field capacity (FC) and full saturation (ST).     
  WC   <- 0.001 * WA / ROOTD
  WAFC <- 1000 * WCFC * ROOTD
  WAST <- 1000 * WCST * ROOTD
  
  # Drainage below the root zone occurs when the amount of water in the soil exceeds field capacity or when the amount of rainfall 
  # in excess of interception and evapotranspiration fills up soil water above field capacity.
  DRAIN <-(WA-WAFC)/DELT + (RAIN - (RNINTC + EVAP + TRAN))
  if(DRAIN < 0) {
    DRAIN <-0
  } else if(DRAIN >= DRATE) {
    DRAIN <-DRATE
  }
  # Surface runoff occurs when the amount of soil water exceeds total saturation or when the amount of rainfall 
  # in excess of interception, evapotranspiration and drainage fills up soil water above total saturation.
  RUNOFF = max(0, (WA - WAST) / DELT + (RAIN - (RNINTC + EVAP + TRAN + DRAIN)))
  
  
  # The irrigation rate is the extra amount of water that is needed to keep soil water 
  # at a fraction of field capacity that is defined by setting the parameter IRRIGF. 
  # If IRRIGF is set to 1, the soil will be irrigated every timestep to keep the 
  # amount of water in the soil at field capacity. IRRIGF = 0 implies rainfed conditions.
  IRRIG  = IRRIGF * max(0, (WAFC - WA) / DELT - (RAIN - (RNINTC + EVAP + TRAN + DRAIN + RUNOFF)))
  
  
  DRUNIR <- data.frame(DRAIN = DRAIN, RUNOFF=RUNOFF, IRRIG=IRRIG)
  
  return(DRUNIR)
}
