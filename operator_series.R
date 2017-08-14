# Jingge Xiao
# August 2017

# Each function needs a vector containing time series data in per pixel

# Calculating mean value --------------------------------------------------
.stack_mean <- function(indivVector){
  rline <- mean(x = indivVector, na.rm = TRUE)
  return(rline)
}

# Fitting a trigonometric model ------------------------------------------
.fit_tri <- function(indivVector){
  xaxis <- julian(as.Date(getZ(indexsStack)))
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

# Fitting a 2 order polynomial model -------------------------------------
.fit_poly2 <- function(indivVector){
  xaxis <- julian(as.Date(getZ(indexsStack)))
  rline<-list(NA,NA,NA,NA)
  if (sum(indivVector, na.rm = TRUE) != 0) {
    yaxis <- indivVector
    fdata <- data.frame(xaxis, indivVector)
    fresult = lm(yaxis ~ xaxis + I(xaxis^2), data = fdata)
    b0 <- summary(fresult)$coefficients[1]
    b1 <- summary(fresult)$coefficients[2]
    b2 <- summary(fresult)$coefficients[4]
    r2 <- summary(fresult)$r.squared
    rline <- c(b0, b1, b2, r2)
  }
  return(rline)
}

# Fitting a 3 order polynomial model -------------------------------------
.fit_poly2 <- function(indivVector){
  xaxis <- julian(as.Date(getZ(indexsStack)))
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