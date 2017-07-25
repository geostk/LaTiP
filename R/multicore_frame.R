
#' @title Multicore implementation of the raster::\code{\link{calc}} function.
#'
#' @description Allows functions to be applied to raster objects, with multicore support.
#'
#' @param x Raster* object
#' @param fun Function to be applied to the raster object.
#' @param ... Arguments to be passed to \code{link{writeRaster}}.
#' @details For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
#' @return a Raster* object
#' @author Loic Dutrieux
#' @seealso \code{\link{calc}}
#' @import raster
#' @import parallel
#' @export
#'

# Author: Loic Dutrieux
# June 2013
# loic.dutrieux@wur.nl

multicore_frame <- function(raster_brick, core_fun, num_cores=1) {

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
