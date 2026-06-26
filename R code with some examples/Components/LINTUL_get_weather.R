#-------------------------------------------------------------------------------------------------#
# FUNCTION get_weather
#
# This function reads in CABO format weather files. If endtime is >365 or >366 days, two years are combined.
#
# Author:       AGT Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         10-02-2020
#
#--------------------------------------------------------------------------------------------------#
get_weather <- function(directory="./Weather/",country="NLD",station="1",year=substr('2016', 2,4), endtime = 365){
  # CABO files have a fixed format:
  # *  Columns:
  # *  ========
  # *  station number
  # *  year
  # *  day
  # *  irradiation (kJ m-2 d-1)
  # *  minimum temperature (degrees Celsius)
  # *  maximum temperature (degrees Celsius)
  # *  vapour pressure (kPa)
  # *  mean wind speed (m s-1)
  # *  precipitation (mm d-1)
  #
  # The first row contains site information (LON, LAT, ELEVATION, and 2 Anstrom parameters)
  weather1   <- matrix(data=as.numeric(unlist(scan(file=paste(directory,country,station,".",year,sep=""),
                                                  what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
  #Cut off site information (lat, lon etc) 
  weather1 <- weather1[-c(1),]
  
  # If the growth of the crop covers two years, weather data of two years is needed. 
  if ( (as.numeric(year) %% 4 == 0 && endtime > 366) || (endtime > 365) ){
    year2 = paste0(as.numeric(year) + 1)
    year2 = paste0('00', year2)
    year2 = substr(year2, nchar(year2)-2,nchar(year2))
    weather2 <- matrix(data=as.numeric(unlist(scan(file=paste(directory,country,station,".",year2,sep=""),
                                                   what=list("","","","","","","","",""),comment.char='*',fill=TRUE,quiet=TRUE))),ncol=9)
    #Cut off site information (lat, lon etc) 
    weather2 <- weather2[-c(1),]
    
    if (as.numeric(year) %% 4 == 0){
      weather2[,3] <- weather2[,3] + 366
    } else {
      weather2[,3] <- weather2[,3] + 365
    }
    weather <- rbind(weather1, weather2)
  } else {
      weather = weather1
  }


  RDD   = as.vector(weather[,4])    # kJ m-2 d-1:     daily global radiation     
  TMMN  = as.vector(weather[,5])    # deg. C   :     daily minimum temperature  
  TMMX  = as.vector(weather[,6])    # deg. C   :     daily maximum temperature  
  
  SatVP_TMMN    = 0.611 * exp(17.4 * weather[,5] / (weather[,5] + 239))     # kPa         :     Saturation vapour pressure
  SatVP_TMMX    = 0.611 * exp(17.4 * weather[,6] / (weather[,6] + 239))     # kPa         :     Saturation vapour pressure
  VPD = pmax(0, 0.5 * (SatVP_TMMN + SatVP_TMMX) - weather[,7]) 

  WDATA <- data.frame(
    STAT 	 = as.vector(weather[,1]),   # station code
    YEAR 	 = as.vector(weather[,2]),   # year           
    DOYS 	 = as.vector(weather[,3]),   # day number since 1 Jan in the year of sowing / planting         
    DTR    = RDD / 1e+03,              # MJ m-2 d-1 :     incoming radiation (converted from kJ to MJ)
    TMMN   = as.vector(weather[,5]),   # deg. C   :     daily minimum temperature  
    TMMX   = as.vector(weather[,6]),   # deg. C   :     daily maximum temperature  
    VP 	   = as.vector(weather[,7]),   # kPa        :     vapour pressure            
    WN 	   = as.vector(weather[,8]),   # m s-1      :     wind speed                 
    RAIN   = as.vector(weather[,9]),   # mm         :     precipitation              
    DAVTMP = 0.5 * (TMMN + TMMX),      # Deg. C     :     daily average temperature
    VPD    = as.vector(VPD)            # kPa        :     average vapour pressure deficit  
  )
  
  #Remove all lines with a -999 station code indicating errors  
  WDATA <- WDATA[which(WDATA[,"STAT"] >= 0),]
  
  return(WDATA)
}
