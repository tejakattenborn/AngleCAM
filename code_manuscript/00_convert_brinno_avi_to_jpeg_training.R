
# -------- Description
# This script converts the .avi files from the Brinno-TLC200Pro to image frames in .png.
# the frames per second (fps) define how many images should be converted (NULL --> all frames in the .avi-file)
# the time stamp will be extracted from the imagery footer and the latter will be removed.
# The script will query a  xlsx file with two columns, which indicate if a file is usable (should be converted) and if the camera was installed upside-down

require(abind)
require(av)
library(magick)
require(ggplot2)
library(tesseract)
require(xlsx)

dir = "INSERT DIR"
setwd(dir)

#fps = 0.5 # 1 pic every 10 seconds of the video. framerate of video = 1 pic per minute
#fps = NULL #NULL to get all images
fps = 0.3
paste0("approx. pics per avi-file: ", (12*60) / (15 / fps)) # 15 = framerate in avi-file

# initialize individual folders where the .avi files are located in
treeindivids = list.dirs(getwd(), recursive = F); treeindivids

for(treeindivid in 1:length(treeindivids)){
  setwd(treeindivids[treeindivid])
  seqs = list.files(pattern = "AVI")  # load sequences
  meta = read.xlsx("1_manual_meta.xlsx", sheetIndex = 1)
  
  for(sequence in which(meta$usable == 1)){  # only apply loop for those videos that are "usable"
    
    # create folders
    destdir = substr(seqs[sequence], 1, nchar(seqs[sequence])-4)
    unlink(destdir, recursive=TRUE) # clean target directory (if existent already)
    #dir.create(destdir) # create target directory # av_video_images will create a folder on its own
    
    # convert avi to jpeg 
    av_video_images(seqs[sequence], destdir = destdir, format = "png", fps = fps) # to png, jpeg induces artefacts
    file.rename(list.files(destdir, full.names = T), paste0(destdir, "/", basename(getwd()), "_", destdir, "_", list.files(destdir)))
    
    # read time stamp from jpeg
    lim_top = 690
    lim_bottom = 720
    lim_left = 395
    lim_right = 900
    
    eng = tesseract(options = list(tessedit_char_whitelist = "0123456789/: "), language = "eng") 
    
    all_pics = list.files(destdir, full.names = T, pattern = "png")
    date_time = list()
    
    for(pic in 1:length(all_pics)){
      input <- image_read(all_pics[pic]) %>%# image_convert(type = 'Grayscale') %>%
        .[[1]] %>% 
        as.numeric()
      input_cut = input[c(lim_top:lim_bottom),c(lim_left:lim_right),] # crop time stamp section
      input_cut = (abs(input_cut-1)) # convert to black font on white background
      input_cut[input_cut <= 0.3] = 0 # set threshold
      input_cut[input_cut > 0.3] = 1 # set threshold
      input_cut[1:15,,] = 1 # insert upper margin; enhances character detection
      input_cut = abind(input_cut, input_cut[1:12,,], along=1)  # insert lower margin; enhances character detection
      
      date_time[[pic]] = input_cut  %>% image_read() %>% image_resize("280x") %>% tesseract::ocr(engine = eng) # resizing enahnces character detection
    }
    
    date_time = do.call(rbind, date_time)
    # filter frequent wrong data identification
    err = which(substr(date_time, 5,14) == "2021/89/89" | substr(date_time, 5,14) ==  "2021/89/09" | substr(date_time, 5,14) ==  "2021/09/89")
    date_time[err,] = paste0(substr(date_time[err,], 1,4),"2021/09/09", substr(date_time[err,], 15, nchar(date_time[1,])))
    # remove unecessary chars
    date_time = apply(date_time, 2, substr, start=5, stop=23)
    
    # date / time in R: https://www.stat.berkeley.edu/~s133/dates.html
    date_time =  as.POSIXct(date_time, tz = "Europe/Berlin", format = "%Y/%m/%d %H:%M:%OS") # check summer / winter time
    # remove outliers due to wrong date/time estimation
    outliers = which(is.na(date_time)) # in case PSIXct cannot reveal date time format (e.g., if estimated day in month exceeds total days of that month)
    outliers = c(outliers, which(abs(scale(date_time[-outliers])) > 2))
    if(length(outliers) > 0)
    {
      date_time = date_time[-outliers]
      file.remove(all_pics[outliers])
      all_pics = all_pics[-outliers]
    }
    
    # export date / time files
    save(date_time, file = paste0(destdir,"/", "1_", basename(getwd()), "_", destdir, "_1_date_time.Rdata"))
    pdf(file = paste0(destdir,"/", "1_", basename(getwd()), "_", destdir, "_1_date_time_check.pdf"))
    plot(date_time, xlab="pic index")
    dev.off()
    
    # -------- cut date / time area and rotate if necessary
    
    for(pic in 1:length(all_pics)){
      input <- image_read(all_pics[pic]) %>%# image_convert(type = 'Grayscale') %>%
        .[[1]] %>% 
        as.numeric()
      input_cut = input[c(1:lim_top),,] # crop time stamp section
      if(meta$upsidedown[sequence] == 1){
        input_cut  %>% image_read() %>% image_rotate(degrees = 180) %>% image_write(path = all_pics[pic])
      }else{
        input_cut  %>% image_read() %>% image_write(path = all_pics[pic])
      }
    }
  }
}

