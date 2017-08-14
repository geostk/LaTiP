
process_batch <- function(x, pattern=NULL, outdir, srdir, mc.cores=1, ...) {
    
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
    
    mclapply(X=x, FUN=process, outdir=outdir, srdir=srdir, mc.cores=mc.cores, ...)

}