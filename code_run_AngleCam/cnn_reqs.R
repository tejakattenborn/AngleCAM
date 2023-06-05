

# -------- Description
# This script trains CNN models using the pairs of input imagery and leaf angle distributions.
# The training includes a data augmentation on both the predictors (imagery) and the response (leaf angle distributions).
# The augmentation of the imagery includes color modifications, geometric operations and selecting different parts of the imagery
# The augmentation of the leaf angle distribution is based on the variants of the fitted beta distributions (one variant is randmoly selected during the training process)
# The data is loaded on the fly from harddist (tfdataset input pipeline). The input pipeline will act differently on the training and the test data (no augmentation on test data).
# different (pretrained) backbones may be used
# all model objects and are written to checkpoints and can restored from there after training (per default the best model as determined by the test dataset will be loaded).


# image settings
xres = 600L # max 1280 with brinno
yres = 600L #  max690 with brinno
no_bands = 3L
angle_resolution = 45L-2L


# hyperparameters
batch_size <- 9L # 8 for 256 * 256
num_epochs <- 500L


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



# Definition of the CNN structure -----------------------------------------------
# 
# base_model <- tf$keras$applications$EfficientNetV2L(
#   input_shape = c(xres, yres, no_bands),
#   include_top = FALSE,
#   include_preprocessing = FALSE,
#   weights = NULL,
#   pooling = NULL
# )
# 
# # custom layers v2
# predictions <- base_model$output %>%
#   layer_global_average_pooling_2d() %>%
#   layer_dropout(rate = 0.1) %>%
#   layer_dense(units = angle_resolution, activation = 'linear')
# 
# # merge base model and custom layers
# model <- keras_model(inputs = base_model$input, outputs = predictions)
# 
# # compile
# model %>% compile(optimizer = optimizer_adam(learning_rate = 0.0001), loss = 'mse') # mse or mae?