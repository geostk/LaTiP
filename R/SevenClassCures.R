library(raster)
library(ggplot2)
library(extrafont)
library(reshape2)
#font_import()
loadfonts(device="win")

path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_nir_stack.grd'
bandBrick <- brick(path)
xaxis <- julian(as.Date(getZ(bandBrick)))
xnew <- c(julian(as.Date("2013-12-31")):julian(as.Date("2014-12-31")))


#Barron Land
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

#Cultivated Crops
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

#Deciduous Forest
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

#Developed
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

#Evergreen Forest
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

#Mixed Forest
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

#Open Water
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


r2list <- c(blr2, ccr2, dfr2, dr2, efr2, mfr2, owr2)
print(r2list)
fitFrame <- data.frame(xnew, blynew, ccynew, dfynew, dynew, efynew, mfynew, owynew)



xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = 2000)
colorTable <- c('#B22222','#EEAD0E','#7CFC00','#778899','#228B22','#9ACD32','#0000CD')
bandsData<-melt(fitFrame, id.vars = 'xnew')
p <- ggplot(data = bandsData, mapping = aes(x=xnew, y=value))
p+geom_line(mapping = aes(colour = variable), size = 1)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,6000), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 20, base_family = 'Times New Roman')+
  scale_colour_manual(name = 'Class', values=colorTable, labels=c('Barren Land',
                                                                  'Cultivated Crops',
                                                                  'Deciduous Forest',
                                                                  'Developed',
                                                                  'Evergreen Forest',
                                                                  'Mixed Forest',
                                                                  'Open Water'))









  

# p <- ggplot(data = fitFrame)
# p+geom_line(mapping = aes(xnew,blynew), size = 1, colour='#B22222')+
#   geom_line(mapping = aes(xnew,ccynew), size = 1, colour='#EEAD0E')+
#   geom_line(mapping = aes(xnew,dfynew), size = 1, colour='#7CFC00')+
#   geom_line(mapping = aes(xnew,dynew), size = 1, colour='#778899')+
#   geom_line(mapping = aes(xnew,efynew), size = 1, colour='#228B22')+
#   geom_line(mapping = aes(xnew,mfynew), size = 1, colour='#9ACD32')+
#   geom_line(mapping = aes(xnew,owynew), size = 1, colour='#0000CD')+
#   scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq) +
#   scale_y_continuous(name = 'Reflectance', limits = c(0,6000), breaks=yseq, labels=yseq) + 
#   
#   theme_classic(base_size = 20, base_family = 'Times New Roman')+
#   legend("bottomright", legend=c(1:7), col=c(1:7), lty=1)
  
  
  # theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(),axis.line = element_line(colour = "black"),
  #       axis.text.x = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.title.x = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.text.y = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.title.y = element_text(family="Times New Roman", size=rel(1.5)),)


# barron: 505,1771
# cultivated: 1441,1284
# deciduous: 6,884
# developed: 1701,1280
# evergreen: 1211,880
# mixed: 284,1762
# open: 2158,158


# row.names =c('Julian Data',
#              'Barren Land',
#              'Cultivated Crops',
#              'Deciduous Forest',
#              'Developed',
#              'Evergreen Forest',
#              'Mixed Forest',
#              'Open Water')










  # theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(),axis.line = element_line(colour = "black"),
  #       axis.text.x = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.title.x = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.text.y = element_text(family="Times New Roman", size=rel(1.5)),
  #       axis.title.y = element_text(family="Times New Roman", size=rel(1.5)))

