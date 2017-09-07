
# Calculate Vegetation Index from Landsat surface reflectance data file.
# Extract layers, apply mask, crop, calculate spectral indices and write output to file.

# Jingge Xiao
# August 2017

sr_to_vi <- function(x, typeCode, vi='ndvi', e=NULL, mask=NULL, keep=c(0,1), L=0.5, ...) {      

    clean <- function(x,y) {
        x[!(y %in% keep)] <- NA
        return(x)
    }
    
    if(extension(x[1]) == '.hdf') { 
        x <- unlist(sapply(FUN=function(x){try(get_subdatasets(x), silent=TRUE)}, X=x), use.names=FALSE)
    }
    
    
    
    # When the indices are already pre-processed
    if(any(grepl(pattern=sprintf("^.*%s($|\\.tif)", vi), x=x, ignore.case=TRUE))) { 
        vi <- raster(grep(pattern=sprintf("^.*_%s($|\\.tif)", vi), x=x, value=TRUE, ignore.case=TRUE))
        if(!is.null(mask)) {
            mask <- raster(grep(pattern=sprintf("^.*%s($|\\.tif|_band$)", mask), x=x, value=TRUE))
        }
        
        if(!is.null(e)) {
            if(class(e) != 'extent') {
                e <- extent(e)
            }
            vi <- crop(vi, e)
            if(!is.null(mask)) {
                mask <- crop(mask, e)
            }
        }
        
        if(!is.null(mask)) {
            vi <- overlay(x=vi, y=mask, fun=clean, ...)
        }
        return(vi)
        
        
    } else {

    # vi needs to be processed
      if(vi == 'ndvi') {
        viFormula <- .ndvi()
      } else if(vi == 'ndmi') {
        viFormula <- .ndmi(typeCode)
      } else if(vi == 'ndwi') {
        viFormula <- .ndwi(typeCode)
      } else if(vi == 'ndbi') {
        viFormula <- .ndbi(typeCode)
      } else if(vi == 'bsi') {
        viFormula <- .bsi(typeCode)
      } else if(vi == 'npcri') {
        viFormula <- .npcri(typeCode)
      } else {
        stop("sr2vi Unsupported vi")
      }
        
        ind <- viFormula$ind
        x0 <- grep(pattern=sprintf("^.*_(%s)($|\\.tif)", paste(ind, collapse='|')), x=x, value=TRUE)
        
        bands <- lapply(X=x0, FUN=raster)
        
        
        if(!is.null(mask)) {
            mask <- raster(grep(pattern=sprintf("^.*%s($|\\.tif|_band$)", mask), x=x, value=TRUE))
        }
        
        
        if(!is.null(e)) {
            if(class(e) != 'extent') {
                e <- extent(e)
            }
            bands <- lapply(X=bands, FUN=crop, y=e)
            if(!is.null(mask)) {
                mask <- crop(mask, e)
            }
        }
        
        # VI function
        fun <- viFormula$fun
        
        
        # Prepare the list to be passed to do.call-overlay
        dots <- list(...)
        doListDots <- c(bands, fun=fun, dots)
        doList <- c(bands, fun=fun)
        
        
        if(is.null(mask)) {
            vi <- do.call(what=raster::overlay, args=doListDots)
        } else {
            previ <- do.call(what=raster::overlay, args=doList)
            vi <- overlay(x=previ, y=mask, fun=clean, ...)
        }
        
        return(vi)
    }
}

