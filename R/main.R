
# 设置
file_str<-'band1'
stack_str<-'band1_stack.grd'
path <- 'E:/Research/Experiment/qianshan/features/final'
codePath <- 'E:/MyProgram/R/BandMath'
fit_cores <- 14
files_pro_cores <- 12


#导入包和函数文件
library(raster)
library(parallel)
library(gdalUtils)
library(rgdal)
source(file.path(codePath,'LandsatVIs.R'))
source(file.path(codePath,'sr2vi.R'))
source(file.path(codePath,'processLandsat.R'))
source(file.path(codePath,'processLandsatBatch.R'))
source(file.path(codePath,'timeStack.R'))
source(file.path(codePath,'fit_multiple.R'))


inDir <- file.path(path, 'in')
tmpDir <- file.path(path, 'temp')
stepDir <- file.path(path, 'step')
userDir <- file.path(path, 'user')
indexsDir <- file.path(stepDir, file_str)
test_r <- raster(file.path(userDir, 'targetarea.tif'))
test_e <- extent(test_r)

# 创建目录
for (i in c(tmpDir, inDir, stepDir, indexsDir)) {
  dir.create(i, showWarnings = FALSE)
}

# 进行批量预处理
processLandsatBatch(x = inDir, outdir = indexsDir, srdir = tmpDir, delete = FALSE, mask = 'fmask', vi = file_str, e=test_e, mc.cores=files_pro_cores)

#生成时间序列grd文件
indexsStack <- timeStack(x = indexsDir, pattern = '^.*\\.grd$', filename = file.path(stepDir, stack_str), datatype = 'INT2S')

#并发进行时间序列拟合
fittedBrick <- fit_multiple(indexsStack, fit_cores)

#输出最终结果
names(fittedBrick) <- c("alayer", "blayer", "clayer", "dlayer")
setZ(x=fittedBrick, z=c("a", "b", "c", "d"))
writeRaster(fittedBrick, filename=paraTifDir, format="GTiff", overwrite=TRUE, bandorder='BIL')