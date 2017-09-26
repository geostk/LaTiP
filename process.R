
# The base function that brings a single landsat scene from its archive to a spectral index raster layer, 
# cropped to a certain extent and with clouds filtered out.

# Jingge Xiao
# August 2017

process <- function(x, dtype, vi='ndvi', srdir, outdir, untar=TRUE, delete=FALSE, mask=NULL, ...) {

  if (dtype == 'lc'){
    type_loc <- 42
    str_name <- '(LT04|LT05|LE07|LC08)\\S{21}'
  } else if (dtype == 'lpc'){
    type_loc <- 37
    str_name <- '(LT4|LT5|LE7|LC8)\\S{13}'
  } else {
    stop("unsupported input data type")
  }
  
  indexLoc=nchar(x)-type_loc
  typeCode=as.numeric(substr(x,indexLoc,indexLoc))
  print(typeCode)

   if(untar){
        ex <- extension(x)
        if(ex == '.gz') {
            tarlist <- untar(x, list=TRUE)
        } else if(ex == '.zip') {
            tarlist <- unzip(x, list=TRUE)$Name
        } else {
            stop('The archive is neither tar.gz nor .zip; we don\'t know what to do with that.')
        }
        
        
        if(any(grepl(pattern="^.*\\.hdf$", x=tarlist))) {
            x0 <- grep(pattern="^.*\\.hdf$", x=tarlist, value=TRUE)
        } else if (any(grepl(pattern="^.*\\.tif$", x=tarlist))) {
            if(any(grepl(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist))) {
                x0 <- grep(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist, value=TRUE)
            } else {
              if(vi == 'sr_blue') {
                viFormula <- .sr_blue(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'sr_green') {
                viFormula <- .sr_green(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'sr_red') {
                viFormula <- .sr_red(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'sr_nir') {
                viFormula <- .sr_nir(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'sr_swir1') {
                viFormula <- .sr_swir1(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'sr_swir2') {
                viFormula <- .sr_swir2(typeCode)
                vi <- viFormula$ind
              } else if(vi == 'ndvi') {
                viFormula <- .ndvi(typeCode)
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
              } else if(vi == 'tcbright') {
                viFormula <- .tcbright(typeCode)
              } else if(vi == 'tcgreen') {
                viFormula <- .tcgreen(typeCode)
              } else if(vi == 'tcwet') {
                viFormula <- .tcwet(typeCode)
              } else {
                stop("untar Unsupported vi")
              }
              x0 <- grep(pattern=sprintf("^.*_(%s)\\.tif$", paste(viFormula$ind, collapse='|')), x=tarlist, value=TRUE)
            }
            
        } else {
            stop("Did not find any .tif or .hdf files in the archive")
        }
        if (!is.null(mask)) {
            x0 <- c(x0, grep(pattern=sprintf("^.*%s\\.tif$", mask), x=tarlist, value=TRUE))
        }
        if(ex == '.gz') {
            untar(x, files=x0, exdir=srdir)
        } else if(ex == '.zip') {
            unzip(x, files=x0, exdir=srdir)
        }
        
        x <- file.path(srdir, x0)
   }
  
    name <- str_extract(string=basename(x[1]), str_name)   

    sr_to_vi(x=x, typeCode=typeCode, vi=vi, filename=sprintf('%s/%s.%s.grd', outdir, vi, name), datatype='INT2S', mask=mask, ...)
    if(delete) {
        file.remove(x)
    } 
}