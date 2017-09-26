library(raster)
library(ggplot2)
library(extrafont)
library(reshape2)
library(Cairo)

#font_import()
loadfonts(device="win")

# barron land: 505,1771
# cultivated crops: 1441,1284
# deciduous forest: 6,884
# developed land: 1701,1280
# evergreen forest: 1211,880
# mixed forest: 284,1762
# open water: 2158,158


# For open water
list_poi = c(2158,158)
str_pic_points_name <- 'six_points_water.png'
str_pic_curves_name <- 'six_curves_water.png'
inter_y = 200
limit_y = 800

# # For deciduous forest
# list_poi = c(6,884)
# str_pic_points_name <- 'six_points_deciduous.png'
# str_pic_curves_name <- 'six_curves_deciduous.png'
# inter_y = 2000
# limit_y = 6000


# basic path
basic_path <- 'E:/Research/LandCoverMapping/Experiment/qianshan/Final/TimeSeriesRdata'
xnew <- c(julian(as.Date("2013-12-31")):julian(as.Date("2014-12-31")))

#####################################################################################
# This part is for points 

path <- file.path(basic_path,'sr_blue_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
byaxis <- mmatrix[1,]
xaxis <- julian(as.Date(getZ(bandBrick)))

#Green
path <- file.path(basic_path,'sr_green_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
gyaxis <- mmatrix[1,]

#Red
path <- file.path(basic_path,'sr_red_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
ryaxis <- mmatrix[1,]

#NIR
path <- file.path(basic_path,'sr_nir_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
nyaxis <- mmatrix[1,]

#SWIR1
path <- file.path(basic_path,'sr_swir1_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
s1yaxis <- mmatrix[1,]

#SWIR2
path <- file.path(basic_path,'sr_swir2_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
s2yaxis <- mmatrix[1,]

fitFrame <- data.frame(xaxis, byaxis, gyaxis, ryaxis, nyaxis, s1yaxis, s2yaxis)

# Output pictures
Cairo(width = 15, height = 9, file=file.path(basic_path, str_pic_points_name),
      type="png", pointsize=12, bg = "transparent", canvas = "white", units = "cm", dpi = 300)

xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = inter_y)
bandsData<-melt(fitFrame, id.vars = 'xaxis')
pp <- ggplot(data = bandsData, mapping = aes(x=xaxis, y=value))

pp+geom_point(mapping = aes(shape = variable), size = 1.5, na.rm = TRUE)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,limit_y), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 18, base_family = 'Times New Roman')+
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))+
  scale_shape_discrete(labels=c('Blue',
                                'Green',
                                'Red',
                                'NIR',
                                'SWIR 1',
                                'SWIR 2'))+
  labs(shape='Bands')

dev.off()


###########################################################################
# This part is for line

#Blue
path <- file.path(basic_path,'sr_blue_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]
xaxis <- julian(as.Date(getZ(bandBrick)))

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
blr2 <- summary(fresult)$r.squared
blynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

#Green
path <- file.path(basic_path,'sr_green_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
ccr2 <- summary(fresult)$r.squared
ccynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

#Red
path <- file.path(basic_path,'sr_red_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
dfr2 <- summary(fresult)$r.squared
dfynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

#NIR
path <- file.path(basic_path,'sr_nir_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
dr2 <- summary(fresult)$r.squared
dynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

#SWIR1
path <- file.path(basic_path,'sr_swir1_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
efr2 <- summary(fresult)$r.squared
efynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

#SWIR2
path <- file.path(basic_path,'sr_swir2_stack.grd')
bandBrick <- brick(path)
mmatrix <- bandBrick[list_poi[1],list_poi[2],]

yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
mfr2 <- summary(fresult)$r.squared
mfynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

fitFrame <- data.frame(xnew, blynew, ccynew, dfynew, dynew, efynew, mfynew)

Cairo(width = 15, height = 9, file=file.path(basic_path, str_pic_curves_name),
      type="png", pointsize=12, bg = "transparent", canvas = "white", units = "cm", dpi = 300)

xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = inter_y)
bandsData<-melt(fitFrame, id.vars = 'xnew')
pc <- ggplot(data = bandsData, mapping = aes(x=xnew, y=value))
pc+geom_line(mapping = aes(linetype = variable), size = 0.5)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,limit_y), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 18, base_family = 'Times New Roman')+
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))+
  scale_linetype_discrete(labels=c('Blue',
                                   'Green',
                                   'Red',
                                   'NIR',
                                   'SWIR 1',
                                   'SWIR 2'))+
  labs(linetype='Bands')

dev.off()

