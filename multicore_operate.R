
multicore_operate <- function(raster_brick, i_opera, num_cores=1) {
  
  if(i_opera == 'stack_mean') {
    core_fun <- .stack_mean()
    
  } else if(i_opera == 'fit_tri') {
    core_fun <- .fit_tri()
    
  } else if(i_opera == 'fit_poly2') {
    core_fun <- .fit_poly2()
    
  } else if(i_opera == 'fit_poly3') {
    core_fun <- .fit_poly3()
    
  } else {
    stop("Unsupported operation")
    return() 
  }
  

  if(num_cores == 1) { # Normal calc
    out <- calc(x=raster_brick, fun=core_fun, ...)
    return(out)
  } else {
    
    s <- blockSize(raster_brick, minblocks=num_cores)
    blocs <- seq(1, s$n)
    
    
    # Create blocks and run the function on that bloc
    fun2 <- function(i) {
      e <- extent(raster_brick, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
      # tmp <- rasterTmpFile()
      b <- crop(raster_brick, e)
      out <- calc(x=b, fun=core_fun) # Does this line need an elipsis
      return(out)
    }
    
    listOut <- mclapply(X=blocs, FUN=fun2, num_cores=num_cores)
    
    out <- do.call(mosaic, listOut)
    
    return(out)
  }
  
}