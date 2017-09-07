
# Including basic spectral indexs calculator. This module is extendable, if users want to calculate more indexs.

# Jingge Xiao
# August 2017


# BLUE --------------------------------------------------------------------

.sr_blue <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band2')
  }else{
    ind <- c('sr_band1')
  }
  return(list(ind=ind))
}

# GREEN --------------------------------------------------------------------

.sr_green <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band3')
  }else{
    ind <- c('sr_band2')
  }
  return(list(ind=ind))
}

# RED --------------------------------------------------------------------

.sr_red <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band4')
  }else{
    ind <- c('sr_band3')
  }
  return(list(ind=ind))
}

# NIR --------------------------------------------------------------------

.sr_nir <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band5')
  }else{
    ind <- c('sr_band4')
  }
  return(list(ind=ind))
}

# SWIR1 --------------------------------------------------------------------

.sr_swir1 <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band6')
  }else{
    ind <- c('sr_band5')
  }
  return(list(ind=ind))
}

# SWIR2 --------------------------------------------------------------------

.sr_swir2 <- function(typeCode) {
  if(typeCode==8){
    ind <- c('sr_band7')
  }else{
    ind <- c('sr_band7')
  }
  return(list(ind=ind))
}

# NDVI --------------------------------------------------------------------

.ndvi <- function(typeCode) {
  if(typeCode==7){
    ind <- c('band3','band4')
  }else{
    ind <- c('band4','band5')
  }
  fun <- function(x1, x2) {
    ndvi <- 10000 * (x2 - x1)/(x1 + x2)
    return(ndvi)
  }
  return(list(ind=ind,
              fun=fun))
}

# NDMI ---------------------------------------------------------------------

.ndmi <- function(typeCode) {
  if(typeCode==7){
    ind <- c('sr_band4','sr_band5')
  }else{
    ind <- c('sr_band5','sr_band6')
  }
  fun <- function(x1, x2) {
    ndmi <- 10000 * (x1 - x2)/(x1 + x2)
    return(ndmi)
  }
  return(list(ind=ind,
              fun=fun))
}

# BSI ---------------------------------------------------------------------

.bsi <- function(typeCode) {
  if(typeCode==7){
    ind <- c('sr_band1','sr_band3','sr_band4','sr_band5')
  }else{
    ind <- c('sr_band2','sr_band4','sr_band5','sr_band6')
  }
  fun <- function(x1, x2, x3, x4) {
    bsi <- 10000 * ((x4 + x2)-(x3 + x1))/((x4 + x2)+(x3 + x1))
    return(bsi)
  }
  return(list(ind=ind,
              fun=fun))
}

# NDWI ---------------------------------------------------------------------

.ndwi <- function(typeCode) {
  if(typeCode==7){
    ind <- c('sr_band2','sr_band4')
  }else{
    ind <- c('sr_band3','sr_band5')
  }
  fun <- function(x1, x2) {
    ndwi <- 10000 * (x1 - x2)/(x1 + x2)
    return(ndwi)
  }
  return(list(ind=ind,
              fun=fun))
}

# NPCRI ---------------------------------------------------------------------

.npcri <- function(typeCode) {
  if(typeCode==7){
    ind <- c('sr_band1','sr_band3')
  }else{
    ind <- c('sr_band2','sr_band4')
  }
  fun <- function(x1, x2) {
    .npcri <- 10000 * (x2 - x1)/(x2 + x1)
    return(.npcri)
  }
  return(list(ind=ind,
              fun=fun))
}

# NDBI ---------------------------------------------------------------------

.ndbi <- function(typeCode) {
  if(typeCode==7){
    ind <- c('sr_band4','sr_band5')
  }else{
    ind <- c('sr_band5','sr_band6')
  }
  fun <- function(x1, x2) {
    ndbi <- 10000 * (x2 - x1)/(x1 + x2)
    return(ndbi)
  }
  return(list(ind=ind,
              fun=fun))
}

# Tasseled Cap Components ------------------------------------------------- 

.tcbright <- function(sensor) { 
    ind <- c('band1','band2','band3','band4','band5','band7')  
    # make compatible with getSceneinfo() output 
    if(sensor %in% c("ETM+", "ETM+ SLC-on", "ETM+ SLC-off")) 
        sensor <- 7 
    if(sensor == "TM") 
        sensor <- 5 
    
    if(sensor == 5) { 
        tc_coef <- c(0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303) 
    } else if (sensor == 7) { 
        tc_coef <- c(0.3561, 0.3972, 0.3904, 0.6966, 0.2286, 0.1596)
    } 
    
    fun <- function(x1, x2, x3, x4, x5, x7) { 
        tcbright <- sum(c(x1, x2, x3, x4, x5, x7) * tc_coef) 
    } 
    
    return(list(ind=ind, 
                fun=fun)) 
} 


.tcgreen <- function(sensor) { 
    ind <- c('band1','band2','band3','band4','band5','band7') 
    # make compatible with getSceneinfo() output 
    if(sensor %in% c("ETM+", "ETM+ SLC-on", "ETM+ SLC-off")) 
        sensor <- 7 
    if(sensor == "TM") 
        sensor <- 5 
    
    if(sensor == 5) { 
        tc_coef <- c(-0.1603, -0.2819, -0.4934,  0.7940, 0.0002, -0.1446) 
    } else if (sensor == 7) { 
        tc_coef <- c(-0.3344, -0.3544, -0.4556,  0.6966, -0.0242,-0.2630)
    } 
    
    fun <- function(x1, x2, x3, x4, x5, x7) { 
        tcbright <- sum(c(x1, x2, x3, x4, x5, x7) * tc_coef) 
    } 
    
    return(list(ind=ind, 
                fun=fun)) 
} 


.tcwet <- function(sensor) { 
    ind <- c('band1','band2','band3','band4','band5','band7') 
    # make compatible with getSceneinfo() output 
    if(sensor %in% c("ETM+", "ETM+ SLC-on", "ETM+ SLC-off")) 
        sensor <- 7 
    if(sensor == "TM") 
        sensor <- 5 
    
    if(sensor == 5) { 
        tc_coef <- c(0.0315,  0.2021,  0.3102,  0.1594, 0.6806, -0.6109) 
    } else if (sensor == 7) { 
        tc_coef <- c(0.2626, 0.2141, 0.0926, 0.0656, -0.7629, -0.5388) 
    } 
    
    fun <- function(x1, x2, x3, x4, x5, x7) { 
        tcbright <- sum(c(x1, x2, x3, x4, x5, x7) * tc_coef) 
    } 
    
    return(list(ind=ind, 
                fun=fun)) 
} 