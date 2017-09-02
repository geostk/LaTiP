
# import packages
library(raster)
library(parallel)
library(rgdal)
library(gdalUtils)
library(stringr)
library(zoo)

# set root paths for data and code
path <- '/home/xiao/XWorkSpace/qianshan/Data2'
codePath <- '/home/xiao/XWorkSpace/qianshan/Program/LaTiP'

# set the number of CPU cores to analyze series data
series_cores <- 14

# set the number of CPU cores to preprocess image files
files_pro_cores <- 12


# import function files
source(file.path(codePath,'calculator_vis.R'))
source(file.path(codePath,'multicore_operate.R'))
source(file.path(codePath,'process.R'))
source(file.path(codePath,'process_batch.R'))
source(file.path(codePath,'sr_to_vi.R'))
source(file.path(codePath,'time_stack.R'))
source(file.path(codePath,'get_scene_info.R'))

# define work directories
inDir <- file.path(path, 'in')
tmpDir <- file.path(path, 'temp')
stepDir <- file.path(path, 'step')
userDir <- file.path(path, 'user')
outDir <- file.path(path, 'out')
test_r <- raster(file.path(userDir, 'l8_targetarea.tif'))
test_e <- extent(test_r)

# define operators
# opera_str <- c('stack_mean', 'fit_tri', 'fit_poly2', 'fit_poly3')
opera_str <- c('fit_tri_en')

itemList <- c('sr_blue','sr_green','sr_red','sr_nir','sr_swir1','sr_swir2')

for (file_str in itemList){
  
  t1 <- Sys.time()
  
  print(paste('begin: ',file_str, sep = ""))
  
  indexsDir <- file.path(stepDir, file_str)
  
  # create work directories
  for (i in c(tmpDir, inDir, stepDir, indexsDir, outDir)) {
    dir.create(i, showWarnings = FALSE)
  }
  
  # output path of intermediate results
  stack_str<- paste(file_str,'_stack.grd', sep = "")

  # batch processing
  process_batch(x = inDir, outdir = indexsDir, srdir = tmpDir, delete = TRUE, 
                mask = 'fmask', vi = file_str, e=test_e, mc.cores=files_pro_cores)
  
  # generating the time series .grd file
  indexsStack <- time_stack(x = indexsDir, pattern = '^.*\\.grd$', 
                            filename = file.path(stepDir, stack_str), datatype = 'INT2S')
  
  # analyze time series data
  for (i_opera in opera_str) {

    tifDir <- file.path(outDir, paste(file_str, '_', i_opera, '.tif', sep = ""))
    
    t2 <- Sys.time()
    
    # perform analysis in a parallel manner
    resultBrick <- multicore_operate(indexsStack, i_opera, series_cores)
      
    t3 <- Sys.time()
    t_3_2 <- t3 - t2
    
    # output final result
    writeRaster(resultBrick, filename=tifDir, format="GTiff", overwrite=TRUE, bandorder='BIL')
    
    print(paste('....',i_opera, ' cost time ', as.character(t_3_2), sep = ""))
  }


  
  
  
  
  
  # # output path of fitting failed data
  # csvPath <- file.path(outDir, paste(file_str,'_failedPos.tif', sep = ""))
  
  # # check points where the fitting fails
  # fitNAlist <- list()
  # for (i in 1:dim(fittedBrick)[1]){
  #   for (j in 1:dim(fittedBrick)[2]){
  #     fitvector <- fittedBrick[i,j,]
  #     if(sum(is.na(fitvector)) > 0){
  #       fitNAlist <- c(fitNAlist,list(c(i,j)))
  #     }
  #   }
  # }
  
  # # output locations of fitting failed points
  # if (length(fitNAlist)>0){
  #   failedPosMatrix <- matrix(nrow=length(fitNAlist), ncol=length(fitNAlist[[1]]), byrow = TRUE)
  #   for (i in 1:length(fitNAlist)){
  #     for (j in 1:length(fitNAlist[[1]])){
  #       failedPosMatrix[i,j] <- fitNAlist[[i]][j]
  #     }
  #   }
  #   write.table(failedPosMatrix, file = csvPath, col.names = FALSE, row.names = FALSE, sep = ',')
  # }
  
  t4 <- Sys.time()
  t_4_1 <- t4 - t1
  
  print(paste(file_str, ' total time ', as.character(t_4_1), sep = ""))
  
}
