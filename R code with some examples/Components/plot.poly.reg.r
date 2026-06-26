#
require('ZeBook') #this package provides the goodness.of.fit function
require('ggplot2')

# plot polynomial regression
plot.poly.reg <- function(x,y,interval="none",add.1_1.line=FALSE,pos="topleft",order="linear",...){
  #ensure numbers are doubles to prevent stack overflow
  x=as.double(x)
  y=as.double(y)
  # put x and y in a data frame
  df <- data.frame(x=x,y=y,x2=x*x,x3=x*x*x)
  
  # omit rows with NA
  df <- na.omit(df)
  
  # perform lm(), summary(lm())
  #By default, always linear
  reg  <- lm(y~x,data=df)
  if(order == "quadratic"){
    reg  <- lm(y~x+x2,data=df)
  }
  if(order == "cubic"){
    reg  <- lm(y~x+x2+x3,data=df)
  }
  sum  <- summary(reg)
  
  # create a new dataset for regression line and confidence interval
  lowlimit <- range(na.omit(x))[1]
  uplimit <- range(na.omit(x))[2]
  xnew=seq(lowlimit,uplimit,length.out=50)
  newdat <- data.frame(x=xnew,x2=xnew*xnew,x3=xnew*xnew*xnew)
  
  # plot y~x
    
    # without confidence interval 
    if (interval=="none") {
      plot(df$x,df$y,...)
      pred <- data.frame(fit=predict.lm(reg,newdata=newdat,interval=interval))
    } else {
    
    # with confidence interval
      plot(NA,NA,...)
      pred <- data.frame(predict.lm(reg,newdata=newdat,interval=interval))
      polygon(c(newdat$x,rev(newdat$x)),c(pred$lwr,rev(pred$upr)),col="grey",border=NA)
      lines(df$x,df$y,type="p",...)
    }
  
    # add the 1:1 line if required
    if (add.1_1.line) {abline(0,1,lty=3)}
  
    # add the regression line
    lines(newdat$x,pred$fit,type="l")
  
  # retrieve parameters values
  r.squared <- round(sum$r.squared,2)
  rrmse <- round(goodness.of.fit(df$x,predict(reg))$relative.RMSE,2)
  p.value   <- round(pf(sum$fstatistic[1], sum$fstatistic[2], sum$fstatistic[3], lower.tail=FALSE) ,2)
  if (p.value >0.1)   p   <- " "
  if (p.value <0.1)   p   <- "."
  if (p.value <0.05)  p   <- "*"  
  if (p.value <0.01)  p   <- "**"
  if (p.value <0.001) p   <- "***"
  intercept <- round(sum$coefficients[1,1],2)
  if (sum$coefficients[1,4]>0.1) p.int   <- " "
  if (sum$coefficients[1,4]<0.1) p.int   <- "."
  if (sum$coefficients[1,4]<0.5) p.int   <- "*"
  if (sum$coefficients[1,4]<0.01) p.int  <- "**"
  if (sum$coefficients[1,4]<0.001) p.int <- "***"
  slope     <- round(sum$coefficients[2,1],3)
  if (sum$coefficients[2,4]>0.1) p.slope   <- " "
  if (sum$coefficients[2,4]<0.1) p.slope   <- "."
  if (sum$coefficients[2,4]<0.5) p.slope   <- "*"
  if (sum$coefficients[2,4]<0.01) p.slope  <- "**"
  if (sum$coefficients[2,4]<0.001) p.slope <- "***"
  # add legend to the plot
  legend = vector('expression',4)
  legend[1] = substitute(expression(RRMSE == MYVALUE),           list(MYVALUE = rrmse))[2]
  legend[2] = substitute(expression(paste(italic(R)^2 == MYVALUE,sign)), list(MYVALUE = r.squared, sign=p ))[2]
  legend[3] = substitute(expression(paste(intercept == MYVALUE,sign)),   list(MYVALUE = intercept, sign=p.int))[2]
  legend[4] = substitute(expression(paste(slope == MYVALUE,sign)),       list(MYVALUE = slope,     sign=p.slope))[2]
  
  if(order == "quadratic"){
    quad     <- round(sum$coefficients[3,1],4)
    if (sum$coefficients[3,4]>0.1) p.quad   <- " "
    if (sum$coefficients[3,4]<0.1) p.quad   <- "."
    if (sum$coefficients[3,4]<0.5) p.quad   <- "*"
    if (sum$coefficients[3,4]<0.01) p.quad  <- "**"
    if (sum$coefficients[3,4]<0.001) p.quad <- "***"
    legend[5] = substitute(expression(paste(quad == MYVALUE,sign)),       list(MYVALUE = quad,     sign=p.quad))[2]
  }
  if(order == "cubic"){
    cub     <- round(sum$coefficients[3,1],4)
    if (sum$coefficients[4,4]>0.1) p.cub   <- " "
    if (sum$coefficients[4,4]<0.1) p.cub   <- "."
    if (sum$coefficients[4,4]<0.5) p.cub   <- "*"
    if (sum$coefficients[4,4]<0.01) p.cub  <- "**"
    if (sum$coefficients[4,4]<0.001) p.cub <- "***"
    legend[6] = substitute(expression(paste(cub == MYVALUE,sign)),       list(MYVALUE = cub,     sign=p.cub))[2]
  }
  
  legend(x=pos, legend = legend, bty = 'n')
    
}

#data should be a dataframe, FYR is the first year and LYR the last year              
#the columns with names "year" and " time" must exist
#VAR should have the name of the column name to plot
#example call:
#plot_years(data=VAR_WLIM, FYR=FYR, LYR=LYR, VAR="WSO", YTITLE="Grain yield, g dry matter m-2")
#
plot_years <- function(data=VAR_WLIM, FYR=FYR, LYR=LYR, VAR="WSO", YTITLE="Grain yield, g dry matter m-2"){
  yrs<-FYR:LYR
  colrs<-rainbow(length(yrs))
  
  nr=1
  ii<-which(VAR_WLIM[,"year"]==FYR)
  plot(VAR_WLIM[ii,"time"],VAR_WLIM[ii,VAR],type="l",ylab=YTITLE,xlab="Time",col=colrs[nr])
  for(yr in (FYR+1):LYR){
    nr=nr+1
    ii<-which(VAR_WLIM[,"year"]==yr)
    lines(VAR_WLIM[ii,"time"],VAR_WLIM[ii,VAR],col=colrs[nr])
  }
}
