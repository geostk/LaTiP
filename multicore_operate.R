
# Allows functions to be applied to raster objects with multicore support. 
# Including some basic calculators of regression analysis. This module is 
# extendable, users can add calculators according to their researches.

# Jingge Xiao
# August 2017


multicore_operate <- function(indexsStack, i_opera, series_cores = 1) {    
  
  xaxis <- julian(as.Date(getZ(indexsStack)))
  
  # Each function needs a vector containing time series data in per pixel
  
  # Calculating mean value --------------------------------------------------
  .stack_mean <- function(indivVector){
    rline <- mean(x = indivVector, na.rm = TRUE)
    return(rline)
  }
  
  # Fitting a trigonometric model ------------------------------------------
  .fit_tri <- function(indivVector){
    rline<-list(NA,NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      yaxis <- indivVector
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + 
                     I(sinpi((2/365.256363004)*xaxis)), data = fdata)
      b0 <- summary(fresult)$coefficients[1]
      b1 <- summary(fresult)$coefficients[2]
      b2 <- summary(fresult)$coefficients[3]
      b3 <- summary(fresult)$coefficients[4]
      r2 <- summary(fresult)$r.squared
      rline <- c(b0, b1, b2, b3, r2)
    }
    return(rline)
  }
  
  # Fitting a trigonometric model and enhance some parameters---------------
  .fit_tri_en <- function(indivVector){
    rline<-list(NA,NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      yaxis <- indivVector
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + 
                     I(sinpi((2/365.256363004)*xaxis)), data = fdata)
      b0 <- summary(fresult)$coefficients[1]
      b1 <- summary(fresult)$coefficients[2]
      b2 <- summary(fresult)$coefficients[3]
      b3 <- summary(fresult)$coefficients[4]
      r2 <- summary(fresult)$r.squared
      rline <- c(b0, b1*b0, b2*b0, b3*b0, r2)
    }
    return(rline)
  }
  
  # Fitting a 2 order polynomial model -------------------------------------
  .fit_poly2 <- function(indivVector){
    rline<-list(NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      yaxis <- indivVector
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(yaxis ~ xaxis + I(xaxis^2), data = fdata)
      b0 <- summary(fresult)$coefficients[1]
      b1 <- summary(fresult)$coefficients[2]
      b2 <- summary(fresult)$coefficients[3]
      r2 <- summary(fresult)$r.squared
      rline <- c(b0, b1, b2, r2)
    }
    return(rline)
  }
  
  # Fitting a 3 order polynomial model -------------------------------------
  .fit_poly3 <- function(indivVector){
    rline<-list(NA,NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      yaxis <- indivVector
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(yaxis ~ xaxis + I(xaxis^2) + I(xaxis^3), data = fdata)
      b0 <- summary(fresult)$coefficients[1]
      b1 <- summary(fresult)$coefficients[2]
      b2 <- summary(fresult)$coefficients[3]
      b3 <- summary(fresult)$coefficients[4]
      r2 <- summary(fresult)$r.squared
      rline <- c(b0, b1, b2, b3, r2)
    }
    return(rline)
  }
  
  # Fitting a 4 order polynomial model -------------------------------------
  .fit_poly4 <- function(indivVector){
    rline<-list(NA,NA,NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      yaxis <- indivVector
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(yaxis ~ xaxis + I(xaxis^2) + I(xaxis^3) + I(xaxis^4), data = fdata)
      b0 <- summary(fresult)$coefficients[1]
      b1 <- summary(fresult)$coefficients[2]
      b2 <- summary(fresult)$coefficients[3]
      b3 <- summary(fresult)$coefficients[4]
      b4 <- summary(fresult)$coefficients[5]
      r2 <- summary(fresult)$r.squared
      rline <- c(b0, b1, b2, b3, b4, r2)
    }
    return(rline)
  }
  
  if(i_opera == 'stack_mean') {
    core_fun <- .stack_mean
    
  } else if(i_opera == 'fit_tri') {
    core_fun <- .fit_tri
    
  } else if(i_opera == 'fit_tri_en') {
    core_fun <- .fit_tri_en
    
  } else if(i_opera == 'fit_poly2') {
    core_fun <- .fit_poly2
    
  } else if(i_opera == 'fit_poly3') {
    core_fun <- .fit_poly3
    
  } else if(i_opera == 'fit_poly4') {
    core_fun <- .fit_poly4
    
  } else {
    stop("Unsupported operation")
    return()
  }
  
  
  
  if(series_cores == 1) { # Normal calc
    out <- calc(x=indexsStack, fun=core_fun, ...)
    return(out)
  } else {
    
    # Create blocks and run the function on that bloc
    s <- blockSize(indexsStack, minblocks=series_cores)
    blocs <- seq(1, s$n)
    fun2X <- function(i) {
      e <- extent(indexsStack, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
      Xb <- crop(indexsStack, e)
      out <- calc(x=Xb, fun=core_fun)
      return(out)
    }
    listOut <- mclapply(X=blocs, FUN=fun2X, mc.cores=series_cores)
    listOut$fun <- max
    resultBrick <- do.call(mosaic, listOut)
    return(resultBrick)
  }
}