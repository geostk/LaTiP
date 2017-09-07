
# The base function that brings a single landsat scene from its archive to a spectral index raster layer, 
# cropped to a certain extent and with clouds filtered out.

# Jingge Xiao
# August 2017

process <- function(x, vi='ndvi', srdir, outdir, untar=TRUE, delete=FALSE, mask=NULL, L=0.5, ...) {
  indexLoc=nchar(x)-37
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
    name <- str_extract(string=basename(x[1]), '(LT4|LT5|LE7|LC8)\\d{13}')   

    sr_to_vi(x=x, typeCode=typeCode, vi=vi, filename=sprintf('%s/%s.%s.grd', outdir, vi, name), datatype='INT2S', mask=mask, ...)
    if(delete) {
        file.remove(x)
    } 
}