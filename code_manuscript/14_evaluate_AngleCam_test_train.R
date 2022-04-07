
# -------- Description
# This script estimates the accuracy (R2) of AngleCam for the training and the test data based on the average leaf angle (determined from the predicted leaf angle distribution)
# The script assumes that the model was compiled

checkpoint_dir <- paste0(outdir, "checkpoints/")
models = list.files(checkpoint_dir)
models_best = which.min(substr(models, nchar(models)-11,nchar(models)-5))
model = load_model_hdf5(paste0(checkpoint_dir, models[models_best]), compile = FALSE)

require(zoo)
library(fpp2)

setwd("G:/My Drive/paper/paper_2022_time_lapse_angles")

# load the test and train reference data and predictions corresponding to the model
model_version = "cnn_angle_model_efficientnet_b7_custlay_v2_do02/"
test_pred = read.csv(paste0(model_version,"test_pred.csv"), sep=" ", row.names = NULL, header = FALSE)
test_ref = t(read.csv(paste0(model_version,"test_ref.csv"), sep=" ", row.names = NULL, header = FALSE))
train_pred = read.csv(paste0(model_version,"train_pred.csv"), sep=" ", row.names = NULL, header = FALSE)
train_ref = t(read.csv(paste0(model_version,"train_ref.csv"), sep=" ", row.names = NULL, header = FALSE))
load(paste0(model_version, "test_img.RData"))
load(paste0(model_version, "train_img.RData"))

# calculate the average leaf angle from the leaf angle distribution
angle_res = ncol(test_pred)
mean_angle = function(pred){
    sum(pred/10*seq(0,90, length.out = angle_res))
}
test_pred_avg = apply(test_pred, 1, mean_angle)
test_ref_avg = apply(test_ref, 1, mean_angle)
train_pred_avg = apply(train_pred, 1, mean_angle)
train_ref_avg = apply(train_ref, 1, mean_angle)

#remove outliers
errr = which(train_pred_avg < 0)
train_pred_avg = train_pred_avg[-errr]
train_ref_avg = train_ref_avg[-errr]
train_img  = train_img[-errr]

test_dat = data.frame(test_pred_avg = test_pred_avg, test_ref_avg = test_ref_avg)
train_dat = data.frame(train_pred_avg = train_pred_avg,train_ref_avg = train_ref_avg)

crane0_train = which(substr(basename(train_img), 1,3) %in% c("hom", "kra", "raw"))
crane0_test = which(substr(basename(test_img), 1,3) %in% c("hom", "kra", "raw"))

### accuracy all data
round(cor.test(test_dat$test_pred_avg, test_dat$test_ref_avg)$estimate^2, 2)

### accuracy crane
round(cor.test(train_dat$train_pred_avg[-crane0_train], train_dat$train_ref_avg[-crane0_train])$estimate^2, 2)
round(cor.test(test_dat$test_pred_avg[-crane0_test], test_dat$test_ref_avg[-crane0_test])$estimate^2, 2)

### accuracy potpurri
round(cor.test(train_dat$train_pred_avg[crane0_train], train_dat$train_ref_avg[crane0_train])$estimate^2, 2)
round(cor.test(test_dat$test_pred_avg[crane0_test], test_dat$test_ref_avg[crane0_test])$estimate^2, 2)

# x and y limits
lims = c(5,70)

#scatterplot test
plot_test = ggplot(test_dat, aes(x=test_pred_avg, y=test_ref_avg)) + 
  geom_abline(slope = 1, linetype = "dashed", color="grey", size=1) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, col = "cyan3") +
  #geom_abline(slope = 1) +
  scale_x_continuous(limits = lims) +
  scale_y_continuous(limits = lims) +
  coord_fixed() +
  ylab("Avg. angle reference") +
  xlab("Avg. angle prediction") +
  annotate(geom="text", x=50, y=20, label=paste0("R² = ", round(cor.test(test_dat$test_pred_avg, test_dat$test_ref_avg)$estimate^2, 2)))+
  theme_classic()
plot_test
ggsave(filename = paste0(model_version, "compare_pred_vs_ref_test.pdf"),
       width = 3, height = 3, plot_test)

#scatterplot train
plot_train = ggplot(train_dat, aes(x=train_pred_avg, y=train_ref_avg)) + 
  geom_abline(slope = 1, linetype = "dashed", color="grey", size=1) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, col = "cyan3") +
    scale_x_continuous(limits = lims) +
  scale_y_continuous(limits = lims) +
  coord_fixed() +
  ylab("Avg. angle reference") +
  xlab("Avg. angle prediction") +
  annotate(geom="text", x=50, y=20, label=paste0("R² = ", round(cor.test(train_dat$train_pred_avg, train_dat$train_ref_avg)$estimate^2, 2)))+
  theme_classic()
plot_train
ggsave(filename = paste0(model_version, "compare_pred_vs_ref_train.pdf"),
       width = 3, height = 3, plot_train)
