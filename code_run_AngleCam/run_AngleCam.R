
### ----------------------------------------------------------------------------------------------
### ---- AngleCam --------------------------------------------------------------------------------
### ----------------------------------------------------------------------------------------------

# This is the alpha version of AngleCam (2023-06-05)
# AngleCam estimates leaf angle distributions from conventional RGB photographs
# for details see: https://github.com/tejakattenborn/AngleCAM
# AngleCam can be applied on horizontally acquired imagery with a minumum image size of 600x600 px.
# Input imagery can be of different size; the pipeline will resample them on the fly to 600x600 px.
# Find below two examples for visualizing estimates as leaf angle distribution or average leaf angle

# -------TensorFlow------------------------------------------------------------------------------

# Sketch and some info on installing TensorFlow (sometimes tricky):
# devtools::install_github("rstudio/tensorflow")
# install_tensorflow(version = "2.11.0")
# > official github repository: https://github.com/rstudio/tensorflow
# > General info on installing Tensorflow: https://tensorflow.rstudio.com/installation/
# > running TensorFlow with GPU support is recommended but not mandatory (CPU also works fine, but slower)
# > Hints for TensorFlow with GPU support: https://tensorflow.rstudio.com/installation/gpu/local_gpu/
# > hello <- tf$constant("Hello"); print(hello) # check TensorFlow installation

# ------Dependencies--------------------------------------------------------------------------------

library(reticulate)
reticulate::use_condaenv(condaenv = "tfr", required = TRUE)

library(tensorflow)
library(keras)
library(dplyr)
library(tfdatasets)
require(purrr)

setwd("INSERT DIR")

### GPU settings (only needed if tensorflow is run on GPU, recommended but not mandatory; CPU also works)
gpu_no = 1 # GPU 1 or 2? (in case of multiple GPUs)
gpu_no = tf$config$list_physical_devices()[c(1, gpu_no +1)]
tf$config$set_visible_devices(gpu_no)

### compile the model and initialize the tfdatasets pipeline
source("cnn_reqs.R")
# load model weights
model <- load_model_hdf5("AngleCam_efficientnet_V2L_14-03-2023.hdf5", compile = FALSE)

# -------AngleCam prediction----------------------------------------------------------------------

### load image data and create tfdataset (the imagery will not be loaded to RAM but processed on the fly; there are, hence, no limits regarding dataset size)
imagepaths <- list.files(path = "example_dataset_timeseries_tilia_cordata", full.names = T, pattern = ".png", recursive = F); paste0(length(imagepaths), " images found.")
imgdataset <- create_dataset(tibble(img = imagepaths),  batch = 1, epochs = 1, shuffle = F, train = F, aug_geo = F, aug_rad = F)

### apply the model to the imagery and write output to file
pred_imgdataset <- model %>% predict(imgdataset)
pred_imgdataset <- pred_imgdataset/10
rownames(pred_imgdataset) <- basename(imagepaths)
write.csv(pred_imgdataset, file = "AngleCam_predictions_example_dataset_timeseries_tilia_cordata.csv", row.names = T)

#--------simple visulization--------------------------------------------------------------------

### plot leaf angle distributions over time
for(i in 1:nrow(pred_imgdataset)){
  if (i%%10 == 0) {
    #lines(seq(0,90, length.out = 43), pred_imgdataset[i,])
    plot(seq(0,90, length.out = 43), pred_imgdataset[i,], ylim=c(0,0.07), xlim=c(0,90), type="l", main = substr(rownames(pred_imgdataset)[i], 15, 33), ylab = "density", xlab = "leaf angle [deg]")
    Sys.sleep(0.6)
  }
}

### plot average leaf angles over time
# derive average leaf angle from leaf angle distributions
mean_angle = function(pred){sum(pred*seq(0,90, length.out = ncol(pred_imgdataset)))}
avg_angle <- apply(pred_imgdataset, 1, mean_angle)
# derive date from filename
dates = as.POSIXct(substr(rownames(pred_imgdataset), 15, 33), tz = "Europe/Berlin", format = "%Y-%m-%d_%H-%M-%OS")
# plot leaf angles for one day
plot(dates, avg_angle, ylab = "Average leaf angle [deg]", xlab = "time (CEST)")


