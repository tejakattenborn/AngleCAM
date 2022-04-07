
# -------- Description
# This script was used to extract individual plants from the TLSLeAF output. The TLSLeAF output was created from a TLS scan and included the estimated surface angles.
# To extract individual plants from the TLSLeAF output, we manually segmented individual plants from the raw point cloud (saved as individual files).
# The point clouds of the individual plants were onwards cleaned for artefacts and filtered for noise.
# TLSLeAF output (with angle estimates) was then subsetted for each individual plant by applying a maximum distance threshold (distance to the cleaned point cloud subsets of the raw data).

library(data.table)
require(dplyr)

setwd("F:/data/data_brinno/data_2022_brinno_tls_validation/2022_01_tls/")
deci = 4 # precision to remove (keep points)
#plyr::round_any(test$Y[1:5], 0.005, f = ceiling) # could be an alternative to round

# load TLS scan with estimated angles (from TLSleAF)
input_dirty_1 = list.files(full.names = T, pattern = "BG_scan_1_angles")[1];input_dirty_1
input_dirty_2 = list.files(full.names = T, pattern = "BG_scan_2_angles")[1];input_dirty_2
# load cleaned segments
input_clean = list.files("2022_01_tls_single_plants", full.names = T, pattern = ".txt"); input_clean

dirty_1 = fread(input_dirty_1, sep = " ")
dirty_2 = fread(input_dirty_2, sep = " ")
colnames(dirty_1)[1:3] = c("X", "Y", "Z")
dirty_1$X = round(dirty_1$X, deci)
dirty_1$Y = round(dirty_1$Y, deci)
dirty_1$Z = round(dirty_1$Z, deci)
colnames(dirty_2)[1:3] = c("X", "Y", "Z")
dirty_2$X = round(dirty_2$X, deci)
dirty_2$Y = round(dirty_2$Y, deci)
dirty_2$Z = round(dirty_2$Z, deci)

# iterate over all individual plants
for(i in 1:length(input_clean)){
  
  clean = fread(input_clean[i], sep = " ")
  colnames(clean)[1:3] = c("X", "Y", "Z")
  
  # backup coordinates
  back_X = round(clean$X, 5)
  back_Y = round(clean$Y, 5)
  back_Z = round(clean$Z, 5)
  
  #round coordinates (just for matching)
  clean$X = round(clean$X ,deci)
  clean$Y = round(clean$Y, deci)
  clean$Z = round(clean$Z, deci)
  
  if(i<12){
    cleaned = left_join(clean, dirty_1, by = c("X", "Y", "Z"))
  }else{
    cleaned = left_join(clean, dirty_2, by = c("X", "Y", "Z"))
  }
  
  # restore original coordinates
  cleaned$X = back_X
  cleaned$Y = back_Y
  cleaned$Z = back_Z
  
  cleaned = cleaned[complete.cases(cleaned)]
  write.csv(cleaned, row.names = F, file = paste0("2022_01_tls_single_plants_with_angle/",
                                                  substr(basename(input_clean[i]), 1, nchar(basename(input_clean[i]))-4),
                                                  "_angle.txt"))
  print(i)
  flush.console()
}




