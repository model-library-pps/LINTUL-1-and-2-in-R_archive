#This file contains functions: 
#
# LINTUL2_iniSTATES:  inital values of all state variables 
# LINTUL2:            containing the LINTUL code 
#
# Part of this code was developed by Mink Zijlstra and adapted to work with the deSolve package.
#
# Tom Schut, 2016

#-----------------------------Initial conditions------------------------------------#
#                                                                                   #
# A number of variables need to be given an initial value. These variables are used #
# by the model before they are calculated during the first time step. During all    #
# subsequent time steps the updated values will be used.                            # 
#                                                                                   #
#-----------------------------------------------------------------------------------#
LINTUL2_iniSTATES <-function(listParam){
  with(as.list( listParam ),{
    
    return( c(ROOTD   = ROOTDI,              # m     :    initial rooting depth (at crop emergence)
              WA      = 1000 * ROOTDI * WCI, # mm    :    initial soil water amount
              TSUM    = 0,                   # deg. C:    temperature sum 
              TRAIN   = 0,                   # mm   :     rain sum
              PAR     = 0,                   # MJ m-2:    PAR sum
              LAI     = 0,                   # m2 m-2:    leaf area index 
              WLV     = LAII / SLA,          # g m-2 :    initial green leaf dry weigth (at crop emergence)
              WLVG    = LAII / SLA,          # g m-2 :    dry weight of green leaves 
              WLVD    = 0,                   # g m-2 :    dry weight of dead leaves
              WST     = 0,                   # g m-2 :    dry weight of stems
              WSO     = 0,                   # g m-2 :    dry weight of storage organs
              WRT     = 0,                   # g m-2 :    dry weight of roots
              TRAN    = 0,                   # mm    :    actual transpiration
              EVAP    = 0,                   # mm    :    actual evaporation
              PTRAN   = 0,                   # mm    :    potential transpiration
              PEVAP   = 0,                   # mm    :    potential evaporation
              IRRIG   = 0                    # mm    :    irrigation sum
    )
    )
  })
}

#--------------------------------------Lintul2--------------------------------------#
#                                                                                   # 
#-----------------------------------------------------------------------------------#
LINTUL2 <-function(Time, State, Pars, WDATA){
  #intergration with delay for RROOTD
  with(as.list(c(State, Pars)), {
    #Get the weather data for the day
    WDATA <- subset(WDATA, DOYS == floor(Time))
    if(nrow(WDATA) != 1){
      print(paste0("ERROR in weather data file: no or more than one record for day nr: ",Time))
      print(WDATA)
      stop()
    }
    
    RTRAIN <- WDATA[,"RAIN"]                       # rain rate, mm d-1
    DTEFF  <- max(0,  WDATA[,"DAVTMP"] - TBASE)     # effective daily temperature (for crop development a treshold temperature (TBASE) needs to be exceeded)
    RPAR <- FPAR * WDATA[,"DTR"]                    # PAR MJ m-2 d-1

    #Determine rates when crop is still growing
    if(TSUM < FINTSUM){
      # Determine water content of rooted soil
      WC  <- 0.001 * WA/ROOTD
      # Once the emergence date is reached and enough water is available the crop emerges (1), once the crop is established is does not disappear again (2)
      emerg1 <- ifelse((Time - DOYEM + 1) > 0 && (WC-WCWP) > 0, 1, 0)
      emerg2 <- ifelse(LAI > 0, 1, 0)
      # Emergence of the crop is used to calculate the accumulated temperature sum.
      EMERG  <- max(emerg1,emerg2)
      RTSUM  <- DTEFF * EMERG

      # If soil water content drops to, or below, wilting point fibrous root growth stops.
      # As long as the crop has not reached its maximum rooting depth and has not started flowering yet, fibrous root growth continues.
      # The rooting depth (m) is calculated from a maximum rate of change in rooting depth, the emergence of the crop and the constraints mentioned above.
      RROOTD <- ifelse((ROOTDM-ROOTD) > 0 && (TSUMAN-TSUM) > 0 && (WC-WCWP) >= 0, RRDMAX * EMERG, 0)

      EXPLOR <- 1000 * RROOTD * WCFC                   # exploration rate of new soil water layers by root depth growth (mm d-1)
      RNINTC <- min(RTRAIN,MMWET * LAI)                # interception of rain by the canopy (mm d-1)

      # Potential evaporation (mm d-1) and transpiration (mm d-1) are calculated according to Penman-Monteith
      PENM   <- penman(WDATA[,"DAVTMP"],WDATA[,"VP"],WDATA[,"DTR"],LAI,WDATA[,"WN"],RNINTC)

      # Actual evaporation (mm d-1) and transpiration (mm d-1)
      EVA  <- evaptr(PENM$PEVAP,PENM$PTRAN,ROOTD,WA,WCAD,WCWP,WCFC,WCWET,WCST,TRANCO,DELT)

      # The transpiration reduction factor is defined as the ratio between actual and potential transpiration
      TRANRF <- ifelse(PENM$PTRAN == 0, 0, EVA$TRAN / PENM$PTRAN)

      # Drainage (below the root zone; mm d-1), surface water runoff (mm d-1) and irrigation rate (mm d-1)
      DRUNIR    <- drunir(RTRAIN,RNINTC,EVA$EVAP,EVA$TRAN,IRRIGF,DRATE,DELT,WA,ROOTD,WCFC,WCST)

      # Rate of change of soil water amount (mm d-1)
      RWA <- (RTRAIN + EXPLOR + DRUNIR$IRRIG) - (RNINTC + DRUNIR$RUNOFF + EVA$TRAN + EVA$EVAP + DRUNIR$DRAIN)
      
      # Light interception (MJ m-2 d-1) and total crop growth rate (g m-2 d-1)
      PARINT <- RPAR * (1 - exp(-K * LAI))
      GTOTAL <- LUE * PARINT * TRANRF
      
      # Relative death rate (d-1) due to aging
      RDRDV <- ifelse((TSUM-TSUMAN) < 0, 0, approx(RDRT.TEMP,RDRT.RDR,WDATA[,"DAVTMP"])$y)

      # Relative death rate (d-1) due to self shading
      RDRSH <- pmax(0, pmin( RDRSHM * (LAI-LAICR) / LAICR, RDRSHM))

      # Effective relative death rate (1; d-1) and the resulting decrease in LAI (2; m2 m-2 d-1) and leaf weight (3; g m-2 d-1)
      RDR   <- max(RDRDV, RDRSH) 	# (1)
      DLAI  <- LAI * RDR  			  # (2)
      RWLVD   <- WLVG * RDR	      # (3)
      
      # Allocation to roots (2), leaves (4), stems (5) and storage organs (6)
      # fractions allocated are modified for water availability (1 and 3)
      FRTMOD <- max(1, 1/(TRANRF+0.5))						    # (1)
      FRT    <- approx(FRTTB.TSUM,FRTTB.FRT,TSUM)$y * FRTMOD		# (2)
      FSHMOD <- (1 - FRT) / (1 - FRT / FRTMOD)				          # (3)
      FLV    <- approx(FLVTB.TSUM,FLVTB.FLV,TSUM)$y * FSHMOD		# (4)
      FST    <- approx(FSTTB.TSUM,FSTTB.FST,TSUM)$y * FSHMOD		# (5)
      FSO    <- approx(FSOTB.TSUM,FSOTB.FSO,TSUM)$y * FSHMOD		# (6)
      
      # Change in biomass (g m-2 d-1) for leaves (1), green leaves (2), stems (3), storage organs (4) and roots (5)
      RWLV   <- GTOTAL * FLV 			    # (1)
      RWLVG  <- GTOTAL * FLV - RWLVD 	# (2)
      RWST   <- GTOTAL * FST			    # (3)
      RWSO   <- GTOTAL * FSO			    # (4)
      RWRT   <- GTOTAL * FRT			    # (5)
      
      GLAI <- glaL2(TIME = Time,DTEFF = DTEFF, TSUM = TSUM, 
                    LAI = LAI, RWLV = RWLV, TRANRF = TRANRF, 
                    WC = WC, WCWP = WCWP, PARS = Pars)

      # Change in LAI (m2 m-2 d-1) due to new growth of leaves
      RLAI <- GLAI - DLAI
      RATES <- c(RROOTD = RROOTD, RWA = RWA,
                 RTSUM = RTSUM, RTRAIN = RTRAIN,  RPAR = RPAR,
                 RLAI = RLAI,
                 RWLV = RWLV, RWLVG = RWLVG, RWLVD = RWLVD,
                 RWST = RWST, RWSO = RWSO, RWRT = RWRT,
                 RTRAN = EVA$TRAN, REVAP = EVA$EVAP, 
                 RPTRAN = PENM$PTRAN, RPEVAP = PENM$PEVAP, RIRRIG = DRUNIR$IRRIG) 
      AUX <- c(WSOTHA = WSO * 0.01, HI = WSO /(WSO + WLV + WST + WRT))
    }
    else{
      #all plant related rates are set to 0
      RATES <- c(RROOTD = 0, RWA = 0,
                 RTSUM = 0, RTRAIN = 0,  RPAR = 0,
                 RLAI = 0,
                 RWLV = 0, RWLVG = 0, RWLVD = 0,
                 RWST = 0, RWSO = 0, RWRT = 0,
                 RTRAN = 0, REVAP = 0, RPTRAN = 0, RPEVAP = 0, RIRRIG = 0) 
      AUX <- c(WSOTHA = WSO * 0.01, 
               HI = ifelse((WSO + WLV + WST + WRT)==0, 0, WSO /(WSO + WLV + WST + WRT)))
    }
    return(list(RATES,c(AUX, RATES) )) 
    
  })
}






