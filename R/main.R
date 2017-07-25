
# 设置
file_str<-'band1'
stack_str<-'band1_stack.grd'
path <- 'E:/Research/Experiment/qianshan/features/final'
codePath <- 'E:/MyProgram/R/BandMath'
num_cores <- 14
files_pro_cores <- 12
e_str <- ''
core_fun_name <- 'fit'

# 进行批量预处理
preprocess_batch(work_str = work_str, delete = FALSE, mask = 'fmask', vi = file_str, e=e_str, num_cores=files_pro_cores)

#生成时间序列grd文件
stacked_data <- timeStack(x = indexs_dir, pattern = '^.*\\.grd$', filename = file.path(step_dir, stack_str), datatype = 'INT2S')

#并发进行时间序列拟合
processed_data <- multicore_frame(raster_brick = stack_data, core_fun = core_fun_name, num_cores=1)

#输出最终结果
names(fittedBrick) <- c("alayer", "blayer", "clayer", "dlayer")
setZ(x=fittedBrick, z=c("a", "b", "c", "d"))
writeRaster(fittedBrick, filename=paraTif_dir, format="GTiff", overwrite=TRUE, bandorder='BIL')


# 生成测试数据
testData <- brick('E:/Research/basic/github/LaTiP/data/Red.grd')
xe <- extent(testData, r1=2, r2=11, c1 = 880, c2 = 889)
xb <- crop(testData, xe)
writeRaster(xb, 'E:/Research/basic/github/LaTiP/data/test')
