
# A function to create raster stacks of spectral index layers, with time written in the object properties.

# Jingge Xiao
# August 2017

time_stack <- function(x, pattern=NULL, orderChrono = TRUE, ...) {
    
    if(!is.character(x)){
        stop('x must be a character (directory) or a list of characters')
    }
    if (length(x) == 1){
        x <- list.files(x, pattern=pattern, full.names=TRUE)
    }
    
    orderChronoFun <- function(list) {
        list2 <- list[order(substr(str_extract(string=basename(list), '(LT4|LT5|LE7|LC8)\\d{13}'), 4, 16))] 
        return(list2)        
    }
    
    if(orderChrono){
        x <- orderChronoFun(x)
    }
    
    s <- stack(x)
    names(s) <- row.names(get_scene_info(x))
    
    time <- get_scene_info(x)$date
    s <- setZ(x=s, z=time)
    
    if(hasArg(filename)) {
        out <- writeRaster(s, ...)
        return(out)
    }
    return(s)
        
}
