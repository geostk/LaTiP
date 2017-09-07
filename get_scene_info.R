
# Retrieve Landsat info from filenames. Parses through typical Landsat filenames and retrieves 
# information on sensor and acquisition date.

# Jingge Xiao
# August 2017

get_scene_info <- function(sourcefile, ...)
{
  if(!all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=sourcefile)))
      warning('Some of the characters provided do not contain recognized Landsat5/7/8 scene ID')
    
  sourcefile <- str_extract(sourcefile, '(LT4|LT5|LE7|LC8)\\d{13}') 

  dates <- as.Date(substr(sourcefile, 10, 16), format="%Y%j")
  
  # identify the sensor
  sensor <- as.character(mapply(sourcefile, dates, FUN=function(x,y){
    sen <- substr(x, 1, 3)
    if(is.na(sen)) NA 
    else if(sen == "LE7" & y <= "2003-03-31")
      "ETM+ SLC-on"
    else if(sen == "LE7" & y > "2003-03-31")
      "ETM+ SLC-off"
    else if(sen == "LT5" | sen == "LT4") 
      "TM" 
    else if(sen == "LC8")
      "OLI"      
  }))
  # extract path, row
  path <- as.numeric(substr(sourcefile, 4, 6))
  row <- as.numeric(substr(sourcefile, 7, 9))
  
  # throw all attributes into a data.frame
  info <- data.frame(sensor = sensor, path = path, row = row, date = dates)
  sourcefile[is.na(sourcefile)] <- 'Not recognised'
  row.names(info) <- sourcefile
  
  # optional: print to .csv for future reference
  if(hasArg(file)) 
    write.csv(info, ...)
  
  return(info)
}