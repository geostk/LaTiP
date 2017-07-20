library(raster)
library(ggplot2)
library(extrafont)
#font_import()
loadfonts(device="win")

#Blue
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_blue_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
xaxis <- julian(as.Date(getZ(bandBrick)))
byaxis <- mmatrix[1,]

#Green
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_green_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
gyaxis <- mmatrix[1,]

#Red
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_red_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
ryaxis <- mmatrix[1,]

#NIR
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_nir_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
nyaxis <- mmatrix[1,]

#SWIR1
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_swir1_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
s1yaxis <- mmatrix[1,]

#SWIR2
path <- 'E:/Research/Land Cover Mapping/Experiment/qianshan/Final/Time Series/sr_swir2_stack.grd'
bandBrick <- brick(path)
mmatrix <- bandBrick[6,884,]
s2yaxis <- mmatrix[1,]


bandsFrame <- data.frame(xaxis, byaxis, gyaxis, ryaxis, nyaxis, s1yaxis, s2yaxis)


xnew <- c(julian(as.Date("2013-12-31")):julian(as.Date("2014-12-31")))
xseq <- seq(from = 16100, to = 16400, by = 100)
yseq <- seq(from = 0, to = 10000, by = 2000)
colorTable <- c('#0000FF','#00CD00','#FF0000','#778899','#EE7600','#A020F0')
bandsData<-melt(bandsFrame, id.vars = 'xaxis')
p <- ggplot(data = bandsData, mapping = aes(x=xaxis, y=value))
p+geom_point(mapping = aes(colour = variable), size = 1.5, na.rm = TRUE)+
  scale_x_continuous(name = 'Julian date', limits = c(xnew[1],xnew[length(xnew)]), breaks=xseq, labels=xseq)+
  scale_y_continuous(name = expression(paste('Reflectance(', 1%*%10^4, ')', sep = '')), limits = c(0,6000), breaks=yseq, labels=yseq)+
  theme_classic(base_size = 20, base_family = 'Times New Roman')+
  scale_colour_manual(name = 'Band', values=colorTable, labels=c('Blue',
                                                                 'Green',
                                                                 'Red',
                                                                 'NIR',
                                                                 'SWIR 1',
                                                                 'SWIR 2'))
  
  
  
  
  

# barron: 505,1771
# cultivated: 1441,1284
# deciduous: 6,884
# developed: 1701,1280
# evergreen: 1211,880
# mixed: 284,1762
# open: 2158,158


# theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),
#       panel.background = element_blank(),axis.line = element_line(colour = "black"),
#       axis.text.x = element_text(family="Times New Roman", size=rel(1.5)),
#       axis.title.x = element_text(family="Times New Roman", size=rel(1.5)),
#       axis.text.y = element_text(family="Times New Roman", size=rel(1.5)),
#       axis.title.y = element_text(family="Times New Roman", size=rel(1.5)))

