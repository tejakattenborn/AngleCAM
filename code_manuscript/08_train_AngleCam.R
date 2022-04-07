

# -------- Description
# This script trains CNN models using the pairs of input imagery and leaf angle distributions.
# The training includes a data augmentation on both the predictors (imagery) and the response (leaf angle distributions).
# The augmentation of the imagery includes color modifications, geometric operations and selecting different parts of the imagery
# The augmentation of the leaf angle distribution is based on the variants of the fitted beta distributions (one variant is randmoly selected during the training process)
# The data is loaded on the fly from harddist (tfdataset input pipeline). The input pipeline will act differently on the training and the test data (no augmentation on test data).
# different (pretrained) backbones may be used
# all model objects and are written to checkpoints and can restored from there after training (per default the best model as determined by the test dataset will be loaded).

# the first row of each angle_distribution.csv file should contain the "original" data that will be used for testing.
# additional ressources:
# Guide to transfer learning: https://keras.io/guides/transfer_learning/
# Available models: https://keras.io/api/applications/

library(reticulate)
reticulate::use_condaenv("rtf2", required = TRUE)
reticulate::conda_binary() # "/net/home/tkattenborn/.local/share/r-miniconda/bin/conda"
#install_tensorflow(version = "2.5.0")
library(tensorflow)

# check tensorflow 
hello <- tf$constant("Hello")
print(hello)

#initialize GPU
gpu <- tf$config$experimental$get_visible_devices('GPU')[[1]]
tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)
# limit GPU RAM usage
#tf$config$gpu$set_per_process_memory_growth()

library(keras)
library(dplyr)
library(tfdatasets)

# model config ----------------------------------------------------------------

workdir = "INSERT DIR"
setwd(workdir)

# model output directory
outdir = "cnn_angle_model_efficientnet_b7_custlay_v2_do02/"
# define directory for model checkpoints
checkpoint_dir = paste0(outdir, "checkpoints_cnn_angle_predict")
overwrite = FALSE # overwrite previous model output?

# create output directories
if(!dir.exists(outdir)){
  dir.create(outdir, recursive = TRUE)
}
if(overwrite == TRUE) {
  unlink(list.files(outdir, full.names = TRUE))
}
if(length(list.files(outdir)) > 0 & overwrite == FALSE) {
  stop(paste0("Can't overwrite files in ", outdir, " -> set 'overwrite = TRUE'"))
}

# image settings
xres = 512L # max 1280 with brinno
yres = 512L #  max 690 with brinno
no_bands = 3L # RGB
no_test_samples = 200L # number of samples used for model selection / testing
angle_resolution = 45L-2L # intervals of the leaf angle distributions

# hyperparameters
batch_size <- 10L # 8 for 256 * 256
num_epochs <- 200L

# Loading Data ----------------------------------------------------------------

# list data with reference data
path_ref = list.files("data_brinno_lak_training_samples_withlabels", full.names = T, pattern = "sim.csv", recursive = T)
path_img = list.files("data_brinno_lak_training_samples_withlabels", full.names = T, pattern = ".png", recursive = T)

#match images with available reference data
matches = match(sub("_sim.csv.*", "", path_ref), sub(".png.*", "", path_img))
path_img = path_img[matches]

#just for identifying corrupt files
# for(i in 1:length(path_ref)){
#   read.table(path_ref[i])
# }
# path_ref[i];i

# prepare and standardize reference data samples
ref = lapply(path_ref, read.table)
ref_min_no = min(do.call(rbind, lapply(ref, dim))[,1]); ref_min_no # minimum number of valid simulations (tf dataset requires equal-sized data)
# convert to equal-sized double
for(i in 1:length(ref)){ 
  ref[[i]] = array_reshape(as.matrix(ref[[i]][1:ref_min_no,]), dim=c(ref_min_no, angle_resolution))
}

# split test data and save to disk
set.seed(1234)
testIdx = sample(x = 1:length(path_img), size = no_test_samples, replace = F)
test_img = path_img[testIdx]
test_ref = ref[testIdx]
train_img = path_img[-testIdx]
train_ref = ref[-testIdx]

save(test_img, file = paste0(outdir, "test_img.RData"), overwrite = T)
save(test_ref, file = paste0(outdir, "test_ref.RData"), overwrite = T)
save(train_img, file = paste0(outdir, "train_img.RData"), overwrite = T)
save(train_ref, file = paste0(outdir, "train_ref.RData"), overwrite = T)

# if restore
load(file = paste0(outdir, "test_img.RData"))
load(file = paste0(outdir, "test_ref.RData"))
load(file = paste0(outdir, "train_img.RData"))
load(file = paste0(outdir, "train_ref.RData"))

train_data = tibble(img = train_img, ref = train_ref)
test_data = tibble(img = test_img, ref = test_ref)

head(train_data)
dim(train_data)
train_data$img

# tfdatasets input pipeline -----------------------------------------------

create_dataset <- function(data,
                           train, # logical. TRUE for augmentation of training data
                           batch, # numeric. multiplied by number of available gpus since batches will be split between gpus
                           epochs,
                           aug_rad,
                           aug_geo,
                           shuffle, # logical. default TRUE, set FALSE for test data
                           dataset_size){ # numeric. number of samples per epoch the model will be trained on
  # the data will be shuffled in each epoch
  if(shuffle){
    dataset = data %>%
      tensor_slices_dataset() %>%
      dataset_shuffle(buffer_size = length(data$img), reshuffle_each_iteration = TRUE)
  } else {
    dataset = data %>%
      tensor_slices_dataset() 
  }
  
  # Apply data augmentation for training data
  if(train){
    dataset = dataset %>%
      dataset_map(~.x %>% purrr::list_modify( # read files and decode png
        #img = tf$image$decode_png(tf$io$read_file(.x$img), channels = no_bands)
        img = tf$image$decode_jpeg(tf$io$read_file(.x$img), channels = no_bands, try_recover_truncated = TRUE, acceptable_fraction=0.3) %>%
          tf$image$convert_image_dtype(dtype = tf$float32), #%>%
        ref = .x$ref[sample(2:ref_min_no, 1),] # sample from beta distributions with random errors
      ), num_parallel_calls = NULL)# %>%
  }else{
    dataset = dataset %>%
      dataset_map(~.x %>% purrr::list_modify( # read files and decode png
        #img = tf$image$decode_png(tf$io$read_file(.x$img), channels = no_bands)
        img = tf$image$decode_jpeg(tf$io$read_file(.x$img), channels = no_bands, try_recover_truncated = TRUE, acceptable_fraction=0.3) %>%
          tf$image$convert_image_dtype(dtype = tf$float32), #%>%
        ref = .x$ref[1,] # sample from original beta distributions
      ), num_parallel_calls = NULL)# %>%
  }
  
  # geometric modifications
  if(aug_geo) {
    dataset = dataset %>%
      dataset_map(~.x %>% purrr::list_modify( # randomly flip left/right
        img = tf$image$random_flip_left_right(.x$img, seed = 1L) %>%
          tf$image$random_crop(size = c(552L, 1024L, 3L)) %>% # will crop within image, original height = 690 *0.8, width = 1280*0,8
          tf$keras$preprocessing$image$smart_resize(size=c(xres, yres))
      ))
  }else{
    dataset = dataset %>%
      dataset_map(~.x %>% purrr::list_modify( # randomly flip left/right
        img = tf$keras$preprocessing$image$smart_resize(.x$img, size=c(xres, yres))
      ))
  }
  # radiometric modifications
  if(aug_rad) {
    dataset = dataset %>%
      dataset_map(~.x %>% purrr::list_modify( # randomly assign brightness, contrast and saturation to images
        img = tf$image$random_brightness(.x$img, max_delta = 0.1, seed = 1L) %>% 
          tf$image$random_contrast(lower = 0.8, upper = 1.2, seed = 2L) %>%
          tf$image$random_saturation(lower = 0.8, upper = 1.2, seed = 3L) %>% # requires 3 chnl -> with useDSM chnl = 4
          tf$clip_by_value(0, 1) # clip the values into [0,1] range.
      ))
  }
  if(train) {
    dataset = dataset %>%
      dataset_repeat(count = epochs) 
  }
  dataset = dataset %>% dataset_batch(batch, drop_remainder = TRUE) %>%
    dataset_map(unname) %>%
    dataset_prefetch(buffer_size = tf$data$experimental$AUTOTUNE)
  #dataset_prefetch_to_device("/gpu:0", buffer_size = tf$data$experimental$AUTOTUNE)
}

dataset_size <- length(train_data$img)

train_dataset <- create_dataset(train_data,  batch = batch_size, epochs = num_epochs,
                                shuffle = T, train = T, aug_geo = T, aug_rad = T)
test_dataset <- create_dataset(test_data,  batch = 1, epochs = 1,
                               shuffle = F, train = F, aug_geo = F, aug_rad = F)

# test train dataset pipeline
dataset_iter = reticulate::as_iterator(train_dataset)
example = dataset_iter %>% reticulate::iter_next()
#example
par(mfrow=c(1,2))
plot(as.raster(as.array(example[[1]][1,,,1:3])))
plot(seq(0,90, length.out = angle_resolution),as.numeric(example[[2]][1,]), type="l")

# test test dataset pipeline
dataset_iter = reticulate::as_iterator(test_dataset)
example = dataset_iter %>% reticulate::iter_next()
example
par(mfrow=c(1,2))
plot(as.raster(as.array(example[[1]][1,,,1:3])))
plot(seq(0,90, length.out = angle_resolution),as.numeric(example[[2]][1,]), type="l")



# Definition of the CNN structure (some alternatives are included; commented) -----------------------------------------------

base_model <- application_efficientnet_b7(
  input_shape = c(xres, yres, no_bands),
  include_top = FALSE,
  drop_connect_rate=0.1, # 0.2 is default
  #include_preprocessing=True,
  pooling = NULL
)

# base_model <- application_resnet50_v2(weights = NULL, # = 'imagenet',
#                                       include_top = FALSE,
#                                       input_shape = c(xres, yres, no_bands),
#                                       pooling = NULL
# )

# base_model <- application_resnet152_v2(weights = NULL, # = 'imagenet',
#                                        include_top = FALSE,
#                                        input_shape = c(xres, yres, no_bands),
#                                        pooling = NULL
# )

# base_model <- application_mobilenet(
#   input_shape = c(xres, yres, no_bands),
#   alpha = 1,
#   depth_multiplier = 1L,
#   dropout = 0.001,
#   include_top = FALSE,
#   weights = "imagenet",
#   pooling = NULL
# )

# # custom layers v1
# predictions <- base_model$output %>%
#   layer_global_average_pooling_2d() %>%
#   layer_dropout(rate = 0.5) %>% 
#   layer_dense(units = 512L, activation = 'relu') %>%
#   layer_dropout(rate = 0.5) %>% 
#   layer_dense(units = 256L, activation = 'relu') %>%
#   layer_dropout(rate = 0.5) %>% 
#   # layer_dense(units = 128L, activation = 'relu') %>%
#   # layer_dropout(rate = 0.5) %>% 
#   layer_dense(units = 64L, activation = 'relu') %>%
#   layer_dense(units = angle_resolution, activation = 'linear')

# # custom layers v2
predictions <- base_model$output %>%
  layer_global_average_pooling_2d() %>%
  layer_dense(units = angle_resolution, activation = 'linear')

# # custom layers v3
# predictions <- base_model$output %>%
#   layer_global_max_pooling_2d() %>%
#   layer_dense(units = angle_resolution, activation = 'linear')

# merge base model and custom layers
model <- keras_model(inputs = base_model$input, outputs = predictions)

# compile
model %>% compile(optimizer = optimizer_adam(learning_rate = 0.0001), loss = 'mse') # mse or mae?


# Model Training -----------------------------------------------

# create / clear output directory
checkpoint_dir <- paste0(outdir, "checkpoints/")
unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir, recursive = TRUE)
filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.5f}.hdf5")

# define checkpoint saving
ckpt_callback <- callback_model_checkpoint(filepath = filepath,
                                           monitor = "val_loss",
                                           save_weights_only = FALSE,
                                           save_best_only = TRUE,
                                           verbose = 1,
                                           mode = "auto",
                                           save_freq = "epoch")
csv_callback  <- callback_csv_logger(filename = paste0(outdir, "/epoch_results.csv"))

#start training
history <- model %>% fit(x = train_dataset,
                         epochs = num_epochs,
                         steps_per_epoch = dataset_size/batch_size,
                         callbacks = list(ckpt_callback,
                                          csv_callback,
                                          callback_terminate_on_naan()),
                         validation_data = test_dataset)


# load best models from all epochs
checkpoint_dir <- paste0(outdir, "checkpoints/")
models = list.files(checkpoint_dir)
models_best = which.min(substr(models, nchar(models)-11,nchar(models)-5))
model = load_model_hdf5(paste0(checkpoint_dir, models[models_best]), compile = FALSE)


### Export predictions for sampled train and test

pred_test = model %>% predict(test_dataset)
write.table(pred_test, file = paste0(outdir, "test_pred.csv"), row.names = F, col.names = F)

# create train dataset without augmentation, shuffling, etc...
train_dataset2 <- create_dataset(train_data,  batch = 1, epochs = 1,
                                 shuffle = F, train = F, aug_geo = F, aug_rad = F)
pred_train = model %>% predict(train_dataset2)
write.table(pred_train, file = paste0(outdir, "train_pred.csv"), row.names = F, col.names = F)

### Export reference data for sampled train and test

test_ref = lapply(test_ref,'[',1,)
train_ref = lapply(train_ref,'[',1,)
write.table(test_ref, file = paste0(outdir, "test_ref.csv"), row.names = F, col.names = F)
write.table(train_ref, file = paste0(outdir, "train_ref.csv"), row.names = F, col.names = F)
