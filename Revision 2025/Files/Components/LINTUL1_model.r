#This file contains functions: 
#
# LINTUL1_iniSTATES,          providing intial state values
# LINTUL1,                    containing the LINTUL code 
#
# Part of this code was developed by Mink Zijlstra and was adapted to work with the deSolve package.
#
# Tom Schut, 2016

#-----------------------------Initial conditions------------------------------------#
#                                                                                   #
# A number of variables need to be given an initial value. These variables are used #
# by the model before they are calculated during the first time step. During all    #
# subsequent time steps the updated values will be used.                            # 
#                                                                                   #
#-----------------------------------------------------------------------------------#
LINTUL1_iniSTATES <-function(listParam){
  with(as.list(listParam),{
    return( c(TSUM    = 0,                 # deg. C:    temperature sum 
              PAR     = 0,                 # MJ m-2:    PAR sum
              LAI     = 0,                 # m2 m-2:    leaf area index 
              WLV     = LAII / SLA,        # g m-2 :    initial green leaf dry weigth (at crop emergence)
              WLVG    = LAII / SLA,        # g m-2 :    dry weight of green leaves 
              WLVD    = 0,                 # g m-2 :    dry weight of dead leaves
              WST     = 0,                 # g m-2 :    dry weight of stems
              WSO     = 0,                 # g m-2 :    dry weight of storage organs
              WRT     = 0))                # g m-2 :    dry weight of roots
  })
}


#--------------------------------------Lintul1--------------------------------------#
#                                                                                   # 
#-----------------------------------------------------------------------------------#
LINTUL1 <-function(Time, State, Pars, WDATA){
  with(as.list(c(State, Pars)), {
    #Get the weather data for the day
    WDATA <- subset(WDATA, DOYS == floor(Time))
    if(nrow(WDATA) != 1){
      print(paste0("ERROR in weather data file: no or more than one record for day nr: ",Time))
      print(WDATA)
      stop()
    }
    
    DTEFF  <- max(0,  WDATA[,"DAVTMP"] - TBASE)     # effective daily temperature (for crop development a treshold temperature (TBASE) needs to be exceeded)
    RPAR <- FPAR * WDATA[,"DTR"]                    # PAR MJ m-2 d-1
    
    #determine rates when crop is still growing
    if(TSUM < FINTSUM){
      # Once the emergence date is reached and enough water is available the crop emerges (1), once the crop is established is does not disappear again (2)
      emerg1 <- ifelse((Time - DOYEM) >= 0, 1, 0)
      emerg2 <- ifelse(LAI > 0, 1, 0)
      # Emergence of the crop is used to calculate the accumulated temperature sum.
      EMERG  <- max(emerg1,emerg2)
      RTSUM  <- DTEFF * EMERG
      
      # Light interception (MJ m-2 d-1) and total crop growth rate (g m-2 d-1)
      PARINT <- RPAR * (1 - exp(-K * LAI))
      GTOTAL <- LUE * PARINT
      
      # Relative death rate (d-1) due to aging
      RDRDV <- ifelse((TSUM-TSUMAN) < 0, 0, approx(RDRT.TEMP,RDRT.RDR,WDATA[,"DAVTMP"])$y)
      
      # Relative death rate (d-1) due to self shading
      RDRSH <- pmax(0, pmin( RDRSHM * (LAI-LAICR) / LAICR, RDRSHM))
      
      # Effective relative death rate (1; d-1) and the resulting decrease in LAI (2; m2 m-2 d-1) and leaf weight (3; g m-2 d-1)
      RDR   <- max(RDRDV, RDRSH) 	# (1)
      DLAI  <- LAI * RDR  			  # (2)
      RWLVD <- WLVG * RDR	        # (3)
      
      # Allocation to roots (2), leaves (4), stems (5) and storage organs (6)
      FRT    <- approx(x = FRTTB.TSUM,y = FRTTB.FRT,xout = TSUM)$y 		# (2)
      FLV    <- approx(x = FLVTB.TSUM,y = FLVTB.FLV,xout = TSUM)$y  	# (4)
      FST    <- approx(x = FSTTB.TSUM,y = FSTTB.FST,xout = TSUM)$y   	# (5)
      FSO    <- approx(x = FSOTB.TSUM,y = FSOTB.FSO,xout = TSUM)$y 		# (6)

      # Change in biomass (g m-2 d-1) for leaves (1), green leaves (2), stems (3), storage organs (4) and roots (5)
      RWLV   <- GTOTAL * FLV 			    # (1)
      RWLVG  <- GTOTAL * FLV - RWLVD 	# (2)
      RWST   <- GTOTAL * FST			    # (3)
      RWSO   <- GTOTAL * FSO			    # (4)
      RWRT   <- GTOTAL * FRT			    # (5)
      
      GLAI <- gla(TIME = Time, DTEFF = DTEFF, TSUM = TSUM, LAI = LAI, RWLV = RWLV, PARS = Pars)
      # Change in LAI (m2 m-2 d-1) due to new growth of leaves
      RLAI <- GLAI - DLAI
      

      RATES <- c(RTSUM = RTSUM, RPAR = RPAR, RLAI = RLAI,
                 RWLV = RWLV, RWLVG = RWLVG, RWLVD = RWLVD,
                 RWST = RWST, RWSO = RWSO, RWRT = RWRT) 
      AUX <- c(WBIOMTHA = (WSO + WLV + WST + WRT) * 0.01,
                 WSOTHA = WSO * 0.01, 
               HI = WSO /(WSO + WLV + WST + WRT),
               PARINT = PARINT)
      
    }
    else{
      #all plant related rates are set to 0
      RATES <- c(RTSUM = 0, RPAR = 0, RLAI = 0,
                 RWLV = 0, RWLVG = 0, RWLVD = 0,
                 RWST = 0, RWSO = 0, RWRT = 0) 
      AUX <- c(WBIOMTHA = (WSO + WLV + WST + WRT) * 0.01,
               WSOTHA = WSO * 0.01, 
               HI = ifelse((WSO + WLV + WST + WRT)==0, 0, WSO /(WSO + WLV + WST + WRT)),
               PARINT = 0)
    }
    return(list(RATES,c(AUX, RATES) )) 
    
  })
}


# ---------------------------------------------------------------------#
#  SUBROUTINE GLA                                                      #
#  Purpose: This subroutine computes daily increase of leaf area index #
#           (ha leaf/ ha ground/ d)                                    #
# ---------------------------------------------------------------------#

gla <-function(TIME,DTEFF,TSUM,LAI,RWLV,PARS) {
  with(as.list(PARS),{
    # Growth rate during maturation stage:
    GLAI <-SLA * RWLV
    
    # Growth during juvenile stage:
    if(TSUM < TSUMJUV & LAI < LAIJUV) {
      GLAI <- LAI * (exp(RGRL * DTEFF * DELT) - 1) / DELT
    }
    
    # Growth rate of LAY at day of seedling emergence:
    if(TIME >= DOYEM & LAI == 0) {
      GLAI <- LAII / DELT
    }
    
    # Growth before seedling emergence:
    if(TIME < DOYEM) {
      GLAI <- 0
    }
    
    return(GLAI)
  })
}
