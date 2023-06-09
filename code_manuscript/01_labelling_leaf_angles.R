
# -------- Description
# the script enables to label vertical angles for individual leaves from imagery.
# The script will iterate over all images in the target folder (randomly)
# After selecting a leaf, one can enter the estimated angle and a rolling index (the latter has not been used yet).
# The red marks are meant to help extracting a balanced samples over the image area by taking the closest leaf for each mark. This is not always possible (depends on the image)
# the samples are exported to a .csv file after no_samples has been reached (amount of samples) and the next image will be plotted.

require(raster)
require(scales) # for easy scaling of coordinate values
require(gstat)
require(dplyr)
require(ggplot2)
require(fitdistrplus)

# set working directory
setwd("INSERT DIR")

# settings
no_samples = 20 # how many samples per pic=?

# load imagery
pics = list.files(pattern=".png"); pics
#pics = pics[c(which(grepl("kranzer", pics)), which(grepl("raw", pics)))]
pics = pics[c(which(grepl("raw", pics)))]

# start labelling procedure from here.
# Stop the labelling procedure with ESC (press 3x)
# The labelling procedure can be restarted from here (without running the code above)

for(ii in sample(1:length(pics))){
  
  if(file.exists(paste0(sub('\\.jpg$', '', pics[ii]), ".csv") ) == F){
    pic = stack(pics[ii])

    ysamp = seq(0+100, dim(pic)[1]-100, length.out = 5)
    xsamp = seq(0+100, dim(pic)[2]-100, length.out = 6)
    samp = expand.grid(xsamp, ysamp)
    
    #x11()
    plotRGB(pic)
    points(samp, cex = 5, col="red", pch=3)
    obs_all = data.frame(id = NA, x=NA, Y=NA, angle = NA, rolling = NA)
    
    i = 1
    while(i <= no_samples){
      obs = locator(1, type = "p", pch=15, col = "pink")
      obs = unlist(obs)
      points(obs[1], obs[2], pch=19, col="turquoise", cex=2.5)
      obs = c(obs, as.numeric(readline("Enter estimated leaf angle:")))
      obs = c(obs, as.numeric(readline("Enter estimated leaf rolling:")))
      points(obs[1], obs[2], pch=19, col="yellow", cex=2.5)
      obs = c(i, obs) # add ID
      obs_all = rbind(obs_all, obs)
      print(paste0("Sample ", i, " of ", no_samples))
      i = i+1
    }
    
    obs_all = obs_all[complete.cases(obs_all),]
    write.csv(obs_all, row.names = F, file = paste0(sub('\\.png$', '', pics[ii]), ".csv"))
    print(paste0("finished picture ", ii, "; ", pics[ii]))
    dev.off()
  }
}

