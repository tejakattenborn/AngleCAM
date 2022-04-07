
# -------- Description
# This script applies AngleCam on image time series
# The script assumes that the model was compiled
# The file name included the camera-id and the timestamp. The file names were exported with the predictions, so that at a later stage the predictions were already linked with the acquisition time.

checkpoint_dir <- paste0(outdir, "checkpoints/")
models = list.files(checkpoint_dir)
models_best = which.min(substr(models, nchar(models)-11,nchar(models)-5))
model = load_model_hdf5(paste0(checkpoint_dir, models[models_best]), compile = FALSE)

# load image data
time_series_no = 3 # our dataset comprised multiple time series as segments (1 June - July, 2 July - August, 3 August - September).
t_series = list.files(path = paste0("/net/home/tkattenborn/data_brinno/data_2021_lak_brinno/data_brinno_lak",time_series_no,"_timeseries"),
                      full.names = T, pattern = ".png", recursive = F)
t_series_size = lapply(t_series, file.size)

# query individual cameras (the predictions are performed and saved for each camera individually)
t_series_trees = unique(substr(basename(t_series), 6, 13)); t_series_trees

# predict any write to file
for(i in 1:length(t_series_trees)){
  t_series_sub = grepl(t_series_trees[i], t_series)
  imgdata = tibble(img = t_series[which(t_series_sub)])
  imgdataset <- create_dataset(imgdata,  batch = 1, epochs = 1,
                               shuffle = F, train = F, aug_geo = F, aug_rad = F)
  pred_imgdataset = model %>% predict(imgdataset)
  rownames(pred_imgdataset) = basename(t_series[which(t_series_sub)])
  write.csv(pred_imgdataset, file = paste0("/net/home/tkattenborn/data_brinno/data_2021_lak_brinno/data_brinno_lak",time_series_no,"_timeseries_prediction/", t_series_trees[i], ".csv"),
            row.names = T)
}

