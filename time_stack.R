
# A function to create raster stacks of spectral index layers, with time written in the object properties.

# Jingge Xiao
# August 2017

time_stack <- function(x, dtype, pattern=NULL, orderChrono = TRUE, ...) {
    
    if(!is.character(x)){
        stop('x must be a character (directory) or a list of characters')
    }
    if (length(x) == 1){
        x <- list.files(x, pattern=pattern, full.names=TRUE)
    }
    
    orderChronoFun <- function(list) {
      if(dtype == 'lpc'){
        list2 <- list[order(substr(str_extract(string=basename(list), '(LT4|LT5|LE7|LC8)\\S{13}'), 4, 16))] 
      } else if(dtype == 'lc') {
        list2 <- list[order(substr(str_extract(string=basename(list), '(LT04|LT05|LE07|LC08)\\S{21}'), 5, 25))] 
      } else {
        stop('unsupported data type')
      }
      return(list2)        
    }
    
    if(orderChrono){
        x <- orderChronoFun(x)
    }
    
    s <- stack(x)
    names(s) <- row.names(get_scene_info(x, dtype))
    
    time <- get_scene_info(x, dtype)$date
    s <- setZ(x=s, z=time)
    
    if(hasArg(filename)) {
        out <- writeRaster(s, ...)
        return(out)
    }
    return(s)
        
}
