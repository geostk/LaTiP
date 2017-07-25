
fit_multiple <- function(indexsStack, num_cores) {    
  
  indexsBrick <- brick(indexsStack)

  #拟合子函数
  xaxis <- julian(as.Date(getZ(indexsBrick)))
  fit_coreX <- function(indivVector){
    rline<-list(NA,NA,NA,NA)
    if (sum(indivVector, na.rm = TRUE) != 0) {
      fdata <- data.frame(xaxis, indivVector)
      fresult = lm(indivVector ~ I(cospi((2 / 365.25) * xaxis)) + I(sinpi((2 / 365.25) * xaxis)) + xaxis, data = fdata)
      a <- summary(fresult)$coefficients[1]
      b <- summary(fresult)$coefficients[2]
      c <- summary(fresult)$coefficients[3]
      d <- summary(fresult)$coefficients[4]
      rline <- c(a, b, c, d)
    }
    return(rline)
  }
  
  # 创建 blocks 并运行拟合子函数
  s <- blockSize(indexsBrick, minblocks=num_cores)
  blocs <- seq(1, s$n)
  fun2X <- function(i) {
    e <- extent(indexsBrick, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
    Xb <- crop(indexsBrick, e)
    out <- calc(x=Xb, fun=fit_coreX)
    return(out)
  }
  
  #并发运行
  listOut <- mclapply(X=blocs, FUN=fun2X, num_cores=num_cores)
  
  #合并结果
  listOut$fun <- max
  resultBrick <- do.call(mosaic, listOut)
  
  return(resultBrick)
}