
#' @title Wrapper function to process Landsat data
#'
#' @description Processes a single Landsat scene, from tarball or zip archive (or hdf/tiff if untar is set to FALSE) to vegetation index. Easy to batch using sapply or mclapply for parallel implementation. Data obtained from espi may already contained pre-processed indices layers, in which case they are directly used.
#' @param x Character. filename of the tarball or zip archive of the hdf/tiff file.
#' @param vi Character. Vegetation index to be computed or extracted from the archive. Can be either 'ndvi', 'evi', 'savi', 'ndmi'*, 'nbr', 'nbr2'* or 'msavi'*. Indices with * need to be present in the archive. Note that it is also possible to extract single bands using the \code{vi=} argument. \code{vi='sr_band1'} for instance will extract surface reflectance band 1 from the archive and perform the same pre-processing steps as if it was a vegetation index layer.
#' @param tmp_dir Character. _directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param step_dir Character. _directory where the vegetation index rasterLayer should be written.
#' @param untar Logical. Is there a need to untar data, or have they been previously unpacked.
#' @param delete Logical. Should surface reflectance files (hdf/tiff) be deleted after vegetation index calculated? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
#' @param mask Character or NULL. The name of the mask to be applied to the bands (e.g.: \code{mask = 'fmask'})
#' @param L Numeric. Soil-adjustment factor for SAVI (ignored if vi != 'savi'). L can take on values between 0 and 1, and a default of 0.5 is typically used.
#' @param ... Arguments to be passed to \link{sr2vi}. Do not specify \code{filename} since it is automatically generated
#' @author Loic Dutrieux
#' @return rasterLayer Object also written to file (in \code{step_dir}) with an automatically generated filename
#' @seealso \link{sr2vi}
#'
#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#'
#' @examples
#' # Get list of test data files
#' dir <- system.file('external', package='bfastSpatial')
#' list <- list.files(dir, full.names=TRUE)
#'
#' # Set the location of output and intermediary directories (everything in tmpdir in that case)
#' tmp_dir <- dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
#' dir.create(dirout, showWarning=FALSE)
#' # Generate (or extract, depending on whether the layer is already in the archive or not) NDVI for the first archive file
#' processLandsat(x=list[1], vi='ndvi', step_dir=dirout, tmp_dir=tmp_dir, delete=TRUE, mask='fmask', keep=0, overwrite=TRUE)
#'
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' plot(r <- raster(list[1]))
#'
#' @export
#'

preprocess <- function(x, vi='ndvi', work_str, untar=TRUE, delete=FALSE, mask=NULL, L=0.5, ...) {
    # x is the full path of a tarball containing the Landsat data or the path of a hdf file
    # hdf dir is where the hdf files are extracted
    # Output layers (NDVI for example) are generated in step_dir
    # ... arguments to be passed to hdf2ndvi (filename is automatically generated and therefore does not need to be passed)

    # Although x can be a zip archive, Names are untar, tarlist, etc, since the function was first developped to deal with tar.gz compressed Landsat data
  tmp_dir <- file.path(work_str, 'temp')
  step_dir <- file.path(work_str, 'step')

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


        if(any(grepl(pattern="^.*\\.hdf$", x=tarlist))) { # are there any hdf files
            x0 <- grep(pattern="^.*\\.hdf$", x=tarlist, value=TRUE)
        } else if (any(grepl(pattern="^.*\\.tif$", x=tarlist))) { # Contains tiff
            if(any(grepl(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist))) { # Contains pre-processed vi
                x0 <- grep(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist, value=TRUE)
            } else { # extract the bands needed to process vi
                # Get viFormula object (in order to know which bands to extract)
                if(vi == 'ndvi') {
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
                    stop("Unsupported vi")
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
            untar(x, files=x0, exdir=tmp_dir)
        } else if(ex == '.zip') {
            unzip(x, files=x0, exdir=tmp_dir)
        }

        x <- file.path(tmp_dir, x0)
    }
    name <- str_extract(string=basename(x[1]), '(LT4|LT5|LE7|LC8)\\d{13}')
    # Filename generation (below) will have to be edited when dynamic indices will be implemented
    # Also note that in case of geotiff length(x)>1
    sr2vi(x=x, typeCode=typeCode, vi=vi, filename=sprintf('%s/%s.%s.grd', step_dir, vi, name), datatype='INT2S', mask=mask, ...)
    if(delete) {
        file.remove(x)
    }
}
