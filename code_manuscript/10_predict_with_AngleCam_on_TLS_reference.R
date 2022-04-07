
# -------- Description
# This script applies AngleCam on the data of the botanical garden (TLS comparison)
# The script assumes that the model was compiled

checkpoint_dir <- paste0(outdir, "checkpoints/")
models = list.files(checkpoint_dir)
models_best = which.min(substr(models, nchar(models)-11,nchar(models)-5))
model = load_model_hdf5(paste0(checkpoint_dir, models[models_best]), compile = FALSE)

path_tls_pics = "INSERT DIR"
filename = "tls_predictions_2022_efficientnet_b7_custlay_v2_do01.RData"

paths = list.dirs(path_tls_pics, recursive = F)[grepl("brinno", basename(list.dirs(path_tls_pics, recursive = F)))]
paths

tls_preds = list()
for(i in 1:length(paths)){
  tls_images = list.files(paths[i], full.names = T)
  
  tls_data = tibble(img = tls_images)
  tls_dataset <- create_dataset(tls_data,  batch = 1, epochs = 1,
                                  shuffle = F, train = F, aug_geo = F, aug_rad = F)
  tls_preds[[i]] = model %>% predict(tls_dataset)
}

names(tls_preds) = basename(paths)
save(tls_preds, file = paste0(path_tls_pics, filename))

# test botgarden pics vs. botgarden labelling

botselect = grepl("raw", basename(path_img))
path_img_bot = path_img[botselect]
ref_bot = ref[botselect]

path_img_bot_data = tibble(img = path_img_bot)
path_img_bot_dataset <- create_dataset(path_img_bot_data,  batch = 1, epochs = 1,
                              shuffle = F, train = F, aug_geo = F, aug_rad = F)
path_img_bot_pred = model %>% predict(path_img_bot_dataset)

# plot predictions (just for testing)
i=1
plot(seq(0,90, length.out= ncol(path_img_bot_pred)), path_img_bot_pred[i,], ylim= c( 0,0.8))
lines(seq(0,90, length.out= ncol(path_img_bot_pred)), ref_bot[[i]][1,])
i = i+1
