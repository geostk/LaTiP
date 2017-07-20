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
blyaxis <- mmatrix[1,]

#Cultivated Crops
mmatrix <- bandBrick[1441,1284,]
ccyaxis <- mmatrix[1,]

#Deciduous Forest
mmatrix <- bandBrick[6,884,]
dfyaxis <- mmatrix[1,]

#Developed
mmatrix <- bandBrick[1701,1280,]
dyaxis <- mmatrix[1,]

#Evergreen Forest
mmatrix <- bandBrick[1211,880,]
efyaxis <- mmatrix[1,]

#Mixed Forest
mmatrix <- bandBrick[284,1762,]
mfyaxis <- mmatrix[1,]

#Open Water
mmatrix <- bandBrick[2158,158,]
owyaxis <- mmatrix[1,]

sixPointsFrame <- data.frame(xaxis, blyaxis, ccyaxis, dfyaxis, dyaxis, efyaxis, mfyaxis, owyaxis)

xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = 2000)
colorTable <- c('#B22222','#EEAD0E','#7CFC00','#778899','#228B22','#9ACD32','#0000CD')
bandsData<-melt(sixPointsFrame, id.vars = 'xaxis')
p <- ggplot(data = bandsData, mapping = aes(x=xaxis, y=value))
p+geom_point(mapping = aes(colour = variable), size = 1.5, na.rm = TRUE)+
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

