
#' @title Process Landsat data in batch mode
#'
#' @description Batcher to process Landsat data from tarball or hdf to a list of Vegetation Index files. Runs \link{processLandsat} sequentially or in parallel
#'
#'
#' @param x Character. _directory where the data is located. Data can be of the following formats: hdf, geoTiff, tar.gz or zip)
#' @param pattern Only useful if x if of length 1. See \link{list.files} for more details
#' @param out_dir Character. _directory where the vegetation index rasterLayer should be written.
#' @param srdir Character. _directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param num_cores Numeric. For multicore implementation only. See \link{mclapply}
#' @param ... Arguments to be passed to \link{processLandsat} (\code{untar}, \code{delete}) or to \link{sr2vi} (\code{e}, \code{mask}, \code{keep}, \code{vi})
#' @author Loic Dutrieux
#' @return Function is used for its side effect of calculating in batch mode Vegetation indices from surface reflectance Landsat data.
#' @seealso \link{processLandsat} and \link{sr2vi}
#' @examples
#' # Get the directory where the Landsat archives are stored
#' dir <- system.file('external', package='bfastSpatial')
#'
#' # Set the location of output and intermediary directories (everything in tmpdir in that case)
#' srdir <- dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
#' dir.create(dirout, showWarning=FALSE)
#' processLandsatBatch(x=dir, pattern=glob2rx('*.zip'), out_dir=dirout, srdir=srdir, delete=TRUE, vi='ndvi', mask='fmask', keep=0, overwrite=TRUE)
#'
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' plot(r <- raster(list[1]))
#'
#'
#'
#'
#'
#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#' @import parallel
#' @export
#'


setup_workspace <- function(work_str) {


  in_dir <- file.path(work_str, 'in')
  tmp_dir <- file.path(work_str, 'temp')
  step_dir <- file.path(work_str, 'step')
  out_dir <- file.path(work_str, 'out')

  # 创建目录
  for (i in c(in_dir, tmp_dir, step_dir, out_dir)) {
    dir.create(i, showWarnings = FALSE)
  }

}
