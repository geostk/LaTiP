library(raster)
library(ggplot2)
library(extrafont)
library(reshape2)
library(Cairo)
# font_import()
loadfonts(device="win")


inter_y = 2000
limit_y = 6000
str_pic_points_name <- 'seven_class_points.png'
str_pic_curves_name <- 'seven_class_curves.png'

basic_path <- 'E:/Research/LandCoverMapping/Experiment/qianshan/Final/TimeSeriesRdata'
path_nir <- file.path(basic_path,'sr_nir_stack.grd')
bandBrick <- brick(path_nir)
xaxis <- julian(as.Date(getZ(bandBrick)))
xnew <- c(julian(as.Date("2013-12-31")):julian(as.Date("2014-12-31")))

#####################################################################################
# This part is for points

# Barron Land
mmatrix <- bandBrick[505,1771,]
blyaxis <- mmatrix[1,]

# Cultivated Crops
mmatrix <- bandBrick[1441,1284,]
ccyaxis <- mmatrix[1,]

# Deciduous Forest
mmatrix <- bandBrick[6,884,]
dfyaxis <- mmatrix[1,]

# Developed
mmatrix <- bandBrick[1701,1280,]
dyaxis <- mmatrix[1,]

# Evergreen Forest
mmatrix <- bandBrick[1211,880,]
efyaxis <- mmatrix[1,]

# Mixed Forest
mmatrix <- bandBrick[284,1762,]
mfyaxis <- mmatrix[1,]

# Open Water
mmatrix <- bandBrick[2158,158,]
owyaxis <- mmatrix[1,]

sixPointsFrame <- data.frame(xaxis, blyaxis, ccyaxis, dfyaxis, dyaxis, efyaxis, mfyaxis, owyaxis)

# Output pictures
Cairo(width = 18, height = 9, file=file.path(basic_path, str_pic_points_name),
      type="png", pointsize=12, bg = "transparent", canvas = "white", units = "cm", dpi = 300)

xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = inter_y)
colorTable <- c('#B22222','#EEAD0E','#7CFC00','#778899','#228B22','#9ACD32','#0000CD')
bandsData<-melt(sixPointsFrame, id.vars = 'xaxis')

pp <- ggplot(data = bandsData, mapping = aes(x=xaxis, y=value))
pp+geom_point(mapping = aes(colour = variable), size = 1.5, na.rm = TRUE)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,limit_y), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 18, base_family = 'Times New Roman')+
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))+
  scale_colour_manual(name = 'Class', values=colorTable, labels=c('Barren Land',
                                                                  'Cultivated Crops',
                                                                  'Deciduous Forest',
                                                                  'Developed',
                                                                  'Evergreen Forest',
                                                                  'Mixed Forest',
                                                                  'Open Water'))

  labs(shape='Bands')

dev.off() 

#####################################################################################
# This part is for curves
# Barron Land
mmatrix <- bandBrick[505,1771,]
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
blr2 <- summary(fresult)$r.squared
blynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Cultivated Crops
mmatrix <- bandBrick[1441,1284,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
ccr2 <- summary(fresult)$r.squared
ccynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Deciduous Forest
mmatrix <- bandBrick[6,884,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
dfr2 <- summary(fresult)$r.squared
dfynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Developed
mmatrix <- bandBrick[1701,1280,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
dr2 <- summary(fresult)$r.squared
dynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Evergreen Forest
mmatrix <- bandBrick[1211,880,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
efr2 <- summary(fresult)$r.squared
efynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Mixed Forest
mmatrix <- bandBrick[284,1762,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
mfr2 <- summary(fresult)$r.squared
mfynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

# Open Water
mmatrix <- bandBrick[2158,158,]
xaxis <- julian(as.Date(getZ(bandBrick)))
yaxis <- mmatrix[1,]
fdata <- data.frame(xaxis,yaxis)
fresult = lm(yaxis ~ xaxis + I(cospi((2/365.256363004)*xaxis)) + I(sinpi((2/365.256363004)*xaxis)), data = fdata)
b0 <- summary(fresult)$coefficients[1]
b1 <- summary(fresult)$coefficients[2]
b2 <- summary(fresult)$coefficients[3]
b3 <- summary(fresult)$coefficients[4]
owr2 <- summary(fresult)$r.squared
owynew <- b0 + b1*xnew + b2 * cospi((2/365.25) * xnew) + b3 * sinpi((2/365.25) * xnew)

fitFrame <- data.frame(xnew, blynew, ccynew, dfynew, dynew, efynew, mfynew, owynew)

# Output pictures
Cairo(width = 18, height = 9, file=file.path(basic_path, str_pic_curves_name),
      type="png", pointsize=12, bg = "transparent", canvas = "white", units = "cm", dpi = 300)

xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = inter_y)
colorTable <- c('#B22222','#EEAD0E','#7CFC00','#778899','#228B22','#9ACD32','#0000CD')
bandsData<-melt(fitFrame, id.vars = 'xnew')
pc <- ggplot(data = bandsData, mapping = aes(x=xnew, y=value))
pc+geom_line(mapping = aes(colour = variable), size = 1)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,limit_y), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 20, base_family = 'Times New Roman')+
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))+
  scale_colour_manual(name = 'Class', values=colorTable, labels=c('Barren Land',
                                                                  'Cultivated Crops',
                                                                  'Deciduous Forest',
                                                                  'Developed',
                                                                  'Evergreen Forest',
                                                                  'Mixed Forest',
                                                                  'Open Water'))
dev.off()
  