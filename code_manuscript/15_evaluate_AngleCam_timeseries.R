
# -------- Description
# This script includes several analysis based on the predicted time series derived from the 19 cameras placed at the Leipzig Canopy Crane
# > plotting of time series
# > correlation of leaf angle estimates from different cameras (cameras mounted on top vs within crowns)
# > simulation of effects on canopy reflectance (simulated with PROSAIL-5B) determined by leaf angle variation through time
# > calculating random forest(rf, x = environmental variables, y = mean_average_leaf_angle and sd_average_leaf_angle); calculation of rf variable importance
# > plotting of time series of environmental variables and the rf variable importance


require(zoo)
library(fpp2)
require(xlsx)
require(readxl)
require(ggplot2)
require(data.table)
library(ggpubr)
require(hsdar)
require(randomForest)
require(permimp)
require(foreach)
require(doParallel)


setwd("INSERT DIR")

# Loading Data ----------------------------------------------------------------

cam_setup = read.xlsx("data_brinno_lak_timetable.xlsx", sheetIndex = 1)

dlak_1 = "data_brinno_lak1_timeseries_prediction/"
flak_1 = list.files(dlak_1, full.names = T, pattern = ".csv"); flak_1
lak_1 = lapply(flak_1, read.csv, row.names = 1)

dlak_2 = "data_brinno_lak2_timeseries_prediction/"
flak_2 = list.files(dlak_2, full.names = T, pattern = ".csv"); flak_2
lak_2 = lapply(flak_2, read.csv, row.names = 1)

dlak_3 = "data_brinno_lak3_timeseries_prediction/"
flak_3 = list.files(dlak_3, full.names = T, pattern = ".csv"); flak_3
lak_3 = lapply(flak_3, read.csv, row.names = 1)

angle_res = ncol(lak_1[[1]])

### insert date time to data freame
for(i in 1:length(lak_1)){
  lak_1[[i]] = data.frame(datetime = as.POSIXct(paste0("2021", substr(rownames(lak_1[[i]]), 19, 33)), tz = "Europe/Berlin", format = "%Y-%m-%d_%H-%M-%OS"), lak_1[[i]])
}
for(i in 1:length(lak_2)){
  lak_2[[i]] = data.frame(datetime = as.POSIXct(paste0("2021", substr(rownames(lak_2[[i]]), 19, 33)), tz = "Europe/Berlin", format = "%Y-%m-%d_%H-%M-%OS"), lak_2[[i]])
}
for(i in 1:length(lak_3)){
  lak_3[[i]] = data.frame(datetime = as.POSIXct(paste0("2021", substr(rownames(lak_3[[i]]), 19, 33)), tz = "Europe/Berlin", format = "%Y-%m-%d_%H-%M-%OS"), lak_3[[i]])
}
names(lak_1) = substr(basename(flak_1),0, 8)
names(lak_2) = substr(basename(flak_2),0, 8)
names(lak_3) = substr(basename(flak_3),0, 8)


### convert to avg angle
mean_angle = function(pred){
  sum(pred/10*seq(0,90, length.out = angle_res))
}
lak_1_avg = list()
for(i in 1:length(lak_1)){
  lak_1_avg[[i]] = data.frame(date = lak_1[[i]][,1], avg = apply(lak_1[[i]][,-1], 1, mean_angle))
}
lak_2_avg = list()
for(i in 1:length(lak_2)){
  lak_2_avg[[i]] = data.frame(date = lak_2[[i]][,1], avg = apply(lak_2[[i]][,-1], 1, mean_angle))
}
lak_3_avg = list()
for(i in 1:length(lak_3)){
  lak_3_avg[[i]] = data.frame(date = lak_3[[i]][,1], avg = apply(lak_3[[i]][,-1], 1, mean_angle))
}


### filter daytimes
filter_time = function(x){
  new_dat = list() 
  for(i in 1:length(x)){
    filtered = which((format(x[[i]]$date, "%H-%M")>"09-00") & (format(x[[i]]$date, "%H-%M")<"19-00"))
    new_dat[[i]] = x[[i]][filtered,]
  }
  return(new_dat)
}

lak_1_avg = filter_time(lak_1_avg)
lak_2_avg = filter_time(lak_2_avg)
lak_3_avg = filter_time(lak_3_avg)

# add names
names(lak_1_avg) = names(lak_1)
names(lak_2_avg) = names(lak_2)
names(lak_3_avg) = names(lak_3)


library(scales)
library(dplyr)

windows = 5*60 # minutes to hours


### rolling functions

applyrollmean = function(data, camera){
  dat = getElement(data, camera)
  if(is.null(dat)){
    return(NULL)
  }else{
    every.hour <- data.frame(date=seq(min(dat$date), max(dat$date), by="1 min"))
    dat_merg = full_join(x = dat, y= every.hour, by = "date")
    dat_merg_2 = dat_merg[order(dat_merg$date),]
    rmean = frollmean(dat_merg_2$avg, n = windows, fill = NA, na.rm = T, align = "center")
    rmean = data.frame(date = dat_merg_2$date, avg = rmean)
    return(rmean)
  }
}

applyrollsd = function(data, camera){
  dat = getElement(data, camera)
  if(is.null(dat)){
    return(NULL)
  }else{
    every.hour <- data.frame(date=seq(min(dat$date), max(dat$date), by="1 min"))
    dat_merg = full_join(x = dat, y= every.hour, by = "date")
    dat_merg_2 = dat_merg[order(dat_merg$date),]
    rsd = frollapply(dat_merg_2$avg, n = windows, sd, fill = NA, na.rm = T, align = "center")
    rsd = data.frame(date = dat_merg_2$date, sd = rsd)
    return(rsd)
  }
}

vec_slope = function(x, na.rm){
  if(length(which(is.na(x))) == length(x)){
    return(NaN)
  }else{
    x_vec = 1:length(x)
    return(as.numeric(lm(x ~ x_vec)$coefficients[2]))
  }
}

applyrollslope = function(data, camera){
  dat = getElement(data, camera)
  if(is.null(dat)){
    return(NULL)
  }else{
    every.hour <- data.frame(date=seq(min(dat$date), max(dat$date), by="1 min"))
    dat_merg = full_join(x = dat, y= every.hour, by = "date")
    dat_merg_2 = dat_merg[order(dat_merg$date),]
    rslope = frollapply(dat_merg_2$avg, n = windows, vec_slope, fill = NA, na.rm = T, align = "center")
    rslope = data.frame(date = dat_merg_2$date, slope = rslope)
    return(rslope)
  }
}




# plot time series ----------------------------------------------------------------

xlims <- as.POSIXct(strptime(c(min(lak_1_avg$b_346_o1$date), max(lak_3_avg$b_346_o1$date)), 
                             format = "%Y-%m-%d %H:%M"))
ylims = c(10, 61)
plot_avg = TRUE
times_col = "darkcyan" #  "cyan3" #"deeppink3"

for(i in 1:nrow(cam_setup)){
  
  # plot time series
  camera = cam_setup[i,1]
  plot = ggplot(data = getElement(lak_1_avg, camera),
                aes(x = date, y = avg)) + 
    geom_point(size = 0.5, alpha = 0.2, colour = "black", shape=16) +
    #geom_point(size = 0.5, alpha = 0.2, colour = "black", shape=16) +
    geom_point(data = getElement(lak_2_avg, camera), size = 0.5, alpha = 0.2, colour = "black", shape=16) +
    geom_point(data = getElement(lak_3_avg, camera), size = 0.5, alpha = 0.2, colour = "black", shape=16) +
    geom_vline(xintercept = min(getElement(lak_2_avg, camera)$date), linetype="dashed", color = "lightgrey", size=0.7) +
    geom_vline(xintercept = min(getElement(lak_3_avg, camera)$date), linetype="dashed", color = "lightgrey", size=0.7) +
    scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                     breaks = date_breaks("10 days"), 
                     limits = xlims, 
                     expand = c(0, 0)) +
    scale_y_continuous(limits = ylims) +
    xlab(NULL) +#xlab("Date") +
    ylab("Avg. angle [°]") +
    theme_classic()
  
  # add rolling mean
  if(plot_avg == TRUE){
    if(is.null(getElement(lak_1_avg, camera))==FALSE){
      rmean1 = applyrollmean(lak_1_avg, camera)
      plot = plot + geom_line(data = rmean1, aes(x = date, y = avg), colour = times_col)
    }
    if(is.null(getElement(lak_2_avg, camera))==FALSE){
      rmean2 = applyrollmean(lak_2_avg, camera)
      plot = plot + geom_line(data = rmean2, aes(x = date, y = avg), colour = times_col)
    }
    if(is.null(getElement(lak_3_avg, camera))==FALSE){
      rmean3 = applyrollmean(lak_3_avg, camera)
      plot = plot + geom_line(data = rmean3, aes(x = date, y = avg), colour = times_col)
    }
  }
  
  # add camera label
  if(substr(camera, 7,7) == "o"){
    crown = paste0("top crown ", substr(camera, 8,8))
  }
  if(substr(camera, 7,7) == "u"){
    crown = paste0("within crown ", substr(camera, 8,8))
  }
  if(substr(camera, 1,1) == "l"){
    crown = paste0("Tilia cordata, ", crown)
  }
  if(substr(camera, 1,1) == "b"){
    crown = paste0("Acer pseudoplantus, ", crown)
  }
  
  annotations <- data.frame(
    xpos = c(xlims[1] + 1*24*60*60),
    ypos =  c(Inf),
    annotateText = crown,
    hjustvar = c(0) ,
    vjustvar = c(1))
  plot= plot + geom_text(data=annotations,
                         aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText), size = 5)
  ggsave(filename = paste0("plots_timeseries_prediction/", camera, ".png"),
         width = 12, height = 2.1, plot)
}




# calculate correlation of leaf angle time series (average leaf angle) among different cameras ----------------------------------------------------------------

# function to calculate the correlation of two cameras
calc_cor = function(camera_1, camera_2){
  
  c1 = data.frame(avg = c(getElement(lak_1_avg, camera_1)$avg, getElement(lak_2_avg, camera_1)$avg, getElement(lak_3_avg, camera_1)$avg),
                  date = c(getElement(lak_1_avg, camera_1)$date, getElement(lak_2_avg, camera_1)$date, getElement(lak_3_avg, camera_1)$date))
  c2 = data.frame(avg = c(getElement(lak_1_avg, camera_2)$avg, getElement(lak_2_avg, camera_2)$avg, getElement(lak_3_avg, camera_2)$avg),
                  date = c(getElement(lak_1_avg, camera_2)$date, getElement(lak_2_avg, camera_2)$date, getElement(lak_3_avg, camera_2)$date))
  d   <- function(x,y){
    distance = abs(x-y)
    if(distance[which.min(distance)]< 180){
      return(which.min(distance))
    }
  }
  idx <- sapply( c1$date, function(x) d(x,c2$date)) # find matches (closest time stamp)
  return(cor.test(c1$avg, c2$avg[idx])$estimate)
}

# calculate correlation between cameras of same level (top / within)
cor_within = c(calc_cor("b_346_u1", "b_346_u2"), calc_cor("b_513_u1", "b_513_u2"), calc_cor("l_343_u1", "l_343_u2"),
               calc_cor("l_439_u1", "l_439_u2"))
cor_top = c(calc_cor("b_346_o1", "b_346_o2"), calc_cor("b_513_o1", "b_513_o2"), calc_cor("l_343_o1", "l_343_o2"),calc_cor("l_439_o1", "l_439_o2"),
            calc_cor("b_346_o1", "b_346_o3"), calc_cor("l_343_o1", "l_343_o3"),calc_cor("l_439_o1", "l_439_o3"),
            calc_cor("b_346_o2", "b_346_o3"), calc_cor("l_343_o2", "l_343_o3"),calc_cor("l_439_o2", "l_439_o3"))

mean(cor_within)^2
mean(cor_top)^2




# simulate leaf angle effect on canopy reflectance ----------------------------------------------------------------

#camera = "l_439_o2"
camera = "b_513_o2"

dat = rbind(getElement(lak_1_avg, camera), getElement(lak_2_avg, camera), getElement(lak_3_avg, camera))
parameter <- data.frame(lidfa = dat$avg)
spectra <- PROSAIL(parameterList = parameter, TypeLidf = 0, LAI = 3, Cab = 30, Car = 8, lidfb = 0)

mean_ref = apply(spectra(spectra), 2, mean)
sd_ref = apply(spectra(spectra), 2, sd)
q05_ref = apply(spectra(spectra), 2, quantile, probs = 0.01)
q95_ref = apply(spectra(spectra), 2, quantile, probs = 0.91)
min_ref = apply(spectra(spectra), 2, min)
max_ref = apply(spectra(spectra), 2, max)

spec_sim = data.frame(min = spectra(PROSAIL(lidfa = quantile(dat$avg, 0.01), TypeLidf = 0, LAI = 3, Cab = 30, Car = 8, lidfb = 0))[1,],
                      max = spectra(PROSAIL(lidfa = quantile(dat$avg, 0.99), TypeLidf = 0, LAI = 3, Cab = 30, Car = 8, lidfb = 0))[1,],
                      mean = spectra(PROSAIL(lidfa = mean(dat$avg), TypeLidf = 0, LAI = 3, Cab = 30, Car = 8, lidfb = 0))[1,],
                      wavelength = 400:2500)

plot= ggplot(data=spec_sim, aes(x=wavelength, y=max)) +
  geom_line(colour = "cyan3", alpha=0.5) +
  geom_line(aes(y=mean), colour = "black") +
  geom_line(aes(y=min), colour = "cyan3", alpha=0.5) +
  #geom_ribbon(data=spec_sim(x, 2 <= x & x <= 3), 
  geom_ribbon(aes(ymin=min,ymax=max), fill="cyan3", alpha=0.5) +
  geom_line(aes(y=mean), colour = "black") +
  xlab("wavelength [nm]") +
  ylab("reflectance [0-1]") +
  theme_classic()
plot
ggsave(filename = paste0("plots_timeseries_prediction/simulated_spectral_variation_",camera, ".pdf"),
       width = 4, height = 3, plot)

# mean change in reflectance
mean((spec_sim$min - spec_sim$max) / spec_sim$mean)

#effect on NDVI
red = c(630:690)
nir = c(760:900)
mean_red = rowMeans(spectra(spectra)[,red-400])
mean_nir = rowMeans(spectra(spectra)[,nir-400])
ndvi = (mean_nir-mean_red)/(mean_nir+mean_red)
plot(ndvi)




# random forest (leaf angles vs. environment) ----------------------------------------------------------------

### load environmental data (weather + soil)
envpath = "F:/data/data_brinno/data_2021_lak_environment/"
# wheather; temp / humidity top of canopy
weather = read_excel(paste0(envpath,"180927-upls-31_KranWetter_1642102879_TD.xlsx"))
weather$date = as.POSIXct(paste(weather$`YYYY-MM-DD`, weather$Time), tz = "Europe/Berlin", format = "%Y-%m-%d %H:%M:%OS")
soil = read_excel(paste0(envpath,"soil_moisture_2020_2021.xlsx"))
soil$date = as.POSIXct(soil$Label, tz = "Europe/Berlin", format = "%d. %m. %Y %H:%M:%OS")
weather = left_join(weather, soil[, c("date", "BF2")], "date")

set.seed(1234)
ntree = 1000
mtry = 3
n_cores = 6
train_bins = c(5,10) # first value no of training bins, second value number of total bins

weather2 = weather # simply a backup since the data is being modified in the following
weather2$date = format(weather$date, "%Y-%m-%d %H:%M")

# select predictors
variable_set_avg = c("avg", "SPN1_Total",  "Regen" , "RegenDau" ,"Ltemp" , "Ldruck" , "relFeuchte" , "WiGe", "BF2")
variable_set_sd = c("sd", "SPN1_Total", "Regen" , "RegenDau" ,"Ltemp" , "Ldruck" , "relFeuchte" , "WiGe", "BF2")
# initialize output files
rf_results = data.frame(camera = NA, rf_avg_r2 = NA, rf_sd_r2 = NA)
rf_results_varimp_avg = data.frame(matrix(NA, ncol=length(variable_set_avg[-1])))
names(rf_results_varimp_avg) = variable_set_avg[-1]
rf_results_varimp_sd = data.frame(matrix(NA, ncol=length(variable_set_sd[-1])))
names(rf_results_varimp_sd) = variable_set_sd[-1]

#for(i in 1:6){

for(i in 1:length(cam_setup[,1])){
  camera = cam_setup[i,1]
  rf_results[i,1] = camera
  
  # with rolling mean of average leaf angle
  dat1_avg = rbind(applyrollmean(lak_1_avg, camera), applyrollmean(lak_2_avg, camera), applyrollmean(lak_3_avg, camera))
  dat2_avg = rbind(applyrollmean(lak_1_avg, camera), applyrollmean(lak_2_avg, camera), applyrollmean(lak_3_avg, camera))
  dat3_avg = rbind(applyrollmean(lak_1_avg, camera), applyrollmean(lak_2_avg, camera), applyrollmean(lak_3_avg, camera))
  dat1_avg$date = format(dat1_avg$date, "%Y-%m-%d %H:%M")
  dat2_avg$date = format(dat2_avg$date, "%Y-%m-%d %H:%M")
  dat3_avg$date = format(dat3_avg$date, "%Y-%m-%d %H:%M")
  
  # with rolling sd of average leaf angle
  dat1_sd = rbind(applyrollsd(lak_1_avg, camera), applyrollsd(lak_2_avg, camera), applyrollsd(lak_3_avg, camera))
  dat2_sd = rbind(applyrollsd(lak_1_avg, camera), applyrollsd(lak_2_avg, camera), applyrollsd(lak_3_avg, camera))
  dat3_sd = rbind(applyrollsd(lak_1_avg, camera), applyrollsd(lak_2_avg, camera), applyrollsd(lak_3_avg, camera))
  dat1_sd$date = format(dat1_sd$date, "%Y-%m-%d %H:%M")
  dat2_sd$date = format(dat2_sd$date, "%Y-%m-%d %H:%M")
  dat3_sd$date = format(dat3_sd$date, "%Y-%m-%d %H:%M")
  
  datall1_avg = inner_join(dat1_avg, weather2, by = "date")
  datall2_avg = inner_join(dat2_avg, weather2, by = "date")
  datall3_avg = inner_join(dat3_avg, weather2, by = "date")
  dat_avg = rbind(datall1_avg, datall2_avg, datall3_avg)
  
  datall1_sd = inner_join(dat1_sd, weather2, by = "date")
  datall2_sd = inner_join(dat2_sd, weather2, by = "date")
  datall3_sd = inner_join(dat3_sd, weather2, by = "date")
  dat_sd = rbind(datall1_sd, datall2_sd, datall3_sd)
  
  dat_avg$date = as.POSIXct(dat_avg$date, tz = "Europe/Berlin", format = "%Y-%m-%d %H:%M")
  dat_avg$date = format(dat_avg$date, "%H:%M")
  dat_avg = dat_avg[complete.cases(dat_avg),]
  
  dat_sd$date = as.POSIXct(dat_sd$date, tz = "Europe/Berlin", format = "%Y-%m-%d %H:%M")
  dat_sd$date = format(dat_sd$date, "%H:%M")
  dat_sd = dat_sd[complete.cases(dat_sd),]
  #dat$date = as.numeric(paste0(as.numeric(substr(dat$date, 0,2)),".", 60/as.numeric(substr(dat$date, 4,5))*10))
  
  dat_avg = as.matrix(dat_avg[variable_set_avg])
  dat_sd = as.matrix(dat_sd[variable_set_sd])
  
  # binned sampling (to avoid temporal autocorrelation)
  cutmarks_avg = seq(1, nrow(dat_avg), length.out = train_bins[2]+1)
  groups_avg = .bincode(1:nrow(dat_avg), cutmarks_avg, right = TRUE, include.lowest = TRUE)
  #samp_id_avg = sample(unique(groups_avg), train_bins[1]) # define training samples
  samp_id_avg = which(unique(groups_avg) %% 2 == 1)
  cutmarks_sd = seq(1, nrow(dat_sd), length.out = train_bins[2]+1)
  groups_sd = .bincode(1:nrow(dat_sd), cutmarks_sd, right = TRUE, include.lowest = TRUE)
  #samp_id_sd = sample(unique(groups_sd), train_bins[1]) # define training samples
  samp_id_sd = which(unique(groups_sd) %% 2 == 1)
  
  dat_avg_train = data.frame(dat_avg[which(groups_avg %in% samp_id_avg),])
  dat_sd_train = data.frame(dat_sd[which(groups_sd %in% samp_id_sd),])
  dat_avg_test = data.frame(dat_avg[-which(groups_avg %in% samp_id_avg),])
  dat_sd_test = data.frame(dat_sd[-which(groups_sd %in% samp_id_sd),])
  

  # train raindom forest
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  rf_avg <- foreach(ntree=rep(ntree/n_cores, n_cores), .combine=randomForest::combine,
                    .multicombine=TRUE, .packages='randomForest') %dopar% {
                      randomForest(avg ~ ., data = dat_avg_train, ntree = ntree, localImp = TRUE, type = "regression", mtry = mtry, conditionalTree = F,
                                   keep.forest = TRUE, keep.inbag = TRUE)
                    }
  rf_sd <- foreach(ntree=rep(ntree/n_cores, n_cores), .combine=randomForest::combine,
                   .multicombine=TRUE, .packages='randomForest') %dopar% {
                     randomForest(sd ~ ., data = dat_sd_train, ntree = ntree, importance = TRUE, type = "regression", mtry = mtry, conditionalTree = F,
                                  keep.forest = TRUE, keep.inbag = TRUE)
                   }
  stopCluster(cl)
  
  # calculate importance (non conditional)
  # https://www.r-bloggers.com/2018/06/be-aware-of-bias-in-rf-variable-importance-metrics/
  # rf_results_varimp_avg[i,] =   rf_avg$importance[,1]
  # rf_results_varimp_sd[i,] =   rf_sd$importance[,1]
  
  # predict on test data
  predicted_avg = predict(rf_avg, dat_avg_test)
  predicted_sd = predict(rf_sd, dat_sd_test)
  
  # calulcate R2
  rf_results$rf_avg_r2[i] = cor.test(dat_avg_test[,1], predicted_avg)$estimate^2
  rf_results$rf_sd_r2[i] = cor.test(dat_sd_test[,1], predicted_sd)$estimate^2
  
  # https://cran.r-project.org/web/packages/permimp/vignettes/permimp-package.html
  rf_results_varimp_avg[i,] = permimp(rf_avg, conditional = T, scaled = F, do_check = F)$values
  rf_results_varimp_sd[i,] = permimp(rf_sd, conditional = T, scaled = F, do_check = F)$values
  
  rf_eval_avg = data.frame(observed = dat_avg_test[,1], predicted = predicted_avg)
  rf_eval_sd = data.frame(observed = dat_sd_test[,1], predicted = predicted_sd)
  
  write.csv(rf_results, file = "rf_model_r2.csv")
  write.csv(rf_results_varimp_avg, file = "rf_model_varimp_avg_permip.csv")
  write.csv(rf_results_varimp_sd, file = "rf_model_varimp_sd_permip.csv")
  
  if(substr(camera, 7, 7) == "u"){
    haribo = "darkorchid4"
  }else{
    haribo = "cyan4"
  }
  
  # plots for moving mean (average leaf angle)
  plot = ggplot(rf_eval_avg, aes(x=observed, y=predicted)) +
    geom_abline(slope = 1, linetype = "dashed", color="grey", size=1) +
    geom_point(alpha = 0.1, colour = haribo, shape=16, size = 2) +
    scale_x_continuous(limits = c(min(rf_eval_avg),quantile(unlist(rf_eval_avg), 0.99))) +
    scale_y_continuous(limits = c(min(rf_eval_avg),quantile(unlist(rf_eval_avg), 0.99))) +
    coord_fixed() +
    annotate(geom="text", x=quantile(unlist(rf_eval_avg), 0.05), y=quantile(unlist(rf_eval_avg), 0.98), label = paste0("R² = ", round(as.numeric(cor.test(rf_eval_avg$observed, rf_eval_avg$predicted)$estimate^2), 3))) +
    xlab("observed avg [°]") +
    ylab("predicted avg [°]") +
    theme_classic()
  ggsave(filename = paste0("plots_timeseries_prediction/rf_model_avg_",camera, ".png"),
         width = 3, height = 3.1, plot)
  
  # plots for moving sd (average leaf angle)
  plot = ggplot(rf_eval_sd, aes(x=observed, y=predicted)) +
    geom_abline(slope = 1, linetype = "dashed", color="grey", size=1) +
    geom_point(alpha = 0.1, colour = haribo, shape=16, size = 2) +
    scale_x_continuous(limits = c(min(rf_eval_sd),quantile(unlist(rf_eval_sd), 0.99))) +
    scale_y_continuous(limits = c(min(rf_eval_sd),quantile(unlist(rf_eval_sd), 0.99))) +
    coord_fixed() +
    annotate(geom="text", x=quantile(unlist(rf_eval_sd), 0.05), y=quantile(unlist(rf_eval_sd), 0.98), label = paste0("R² = ", round(as.numeric(cor.test(rf_eval_sd$observed, rf_eval_sd$predicted)$estimate^2), 3))) +
    xlab("observed sd [°]") +
    ylab("predicted sd [°]") +
    theme_classic()
  ggsave(filename = paste0("plots_timeseries_prediction/rf_model_sd_",camera, ".png"),
         width = 3, height = 3.1, plot)
}


rf_results_varimp_avg = abs(rf_results_varimp_avg)
rf_results_varimp_sd = abs(rf_results_varimp_sd)

colMeans(rf_results_varimp_avg)/sum(colMeans(rf_results_varimp_avg)) * 100
colMeans(rf_results_varimp_sd)/sum(colMeans(rf_results_varimp_sd)) * 100

rf_results_varimp_avg
rf_results_varimp_sd

## summary rf avg
# for Tilia
round(mean(rf_results$rf_avg_r2[which(substr(rf_results$camera, 1,1) == "l")]),2)
# for Acer
round(mean(rf_results$rf_avg_r2[which(substr(rf_results$camera, 1,1) == "b")]),2)
# top
round(mean(rf_results$rf_avg_r2[which(substr(rf_results$camera, 7,7) == "o")]),2)
# within
round(mean(rf_results$rf_avg_r2[which(substr(rf_results$camera, 7,7) == "u")]),2)

## summary rf sd
# for Tilia
round(mean(rf_results$rf_sd_r2[which(substr(rf_results$camera, 1,1) == "l")]),2)
# for Acer
round(mean(rf_results$rf_sd_r2[which(substr(rf_results$camera, 1,1) == "b")]),2)
# top
round(mean(rf_results$rf_sd_r2[which(substr(rf_results$camera, 7,7) == "o")]),2)
# within
round(mean(rf_results$rf_sd_r2[which(substr(rf_results$camera, 7,7) == "u")]),2)

## varimp avg
round(colMeans(rf_results_varimp_avg),2)

## varimp sd
round(colMeans(rf_results_varimp_sd),2)





# plot environmental variables and rf variable importance ----------------------------------------------------------------

order(weather$date[-nrow(weather)])
weather$Ltemp = as.numeric(weather$Ltemp)
weather$Ldruck = as.numeric(weather$Ldruck)
weather$SPN1_Diff = as.numeric(weather$SPN1_Diff)
weather$SPN1_Total = as.numeric(weather$SPN1_Total)
weather$Regen = as.numeric(weather$Regen)
weather$RegenDau = as.numeric(weather$RegenDau)
weather$relFeuchte = as.numeric(weather$relFeuchte)
weather$WiGe = as.numeric(weather$WiGe)
weather$wg_peak = as.numeric(weather$wg_peak)
weather$BF2 = as.numeric(weather$BF2)

anno_bg = "white"
alpha_bg = 0.7
vj = 0
hj = 1
anno_date = min(weather$date, na.rm=T)+55*24*60*60

rf_varimp_avg = read.csv(file = "rf_model_varimp_avg_permip.csv", row.names = 1) # format to keep zeros for label consitency
rf_varimp_sd = read.csv(file = "rf_model_varimp_sd_permip.csv", row.names = 1)
rf_varimp_avg = format(round(colMeans(rf_varimp_avg)/sum(colMeans(rf_varimp_avg)) * 100, 2), nsmall = 1)
rf_varimp_sd = format(round(colMeans(rf_varimp_sd)/sum(colMeans(rf_varimp_sd)) * 100, 2), nsmall = 1)
rf_varimp_avg
rf_varimp_sd

# Temp
ylims = c(10, 31)
plot_temp = ggplot(data=weather, aes(x=date, y=Ltemp)) +
  geom_line(colour = "red3") +
  ylab("temp. [°C]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  scale_y_continuous(limits = ylims) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[4], "        Var imp sd: ", rf_varimp_sd[4])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  theme_classic()
plot_temp

geom_label(aes(x = anno_date, y = ylims[2], label = "Here is a line"), fill = "green")

# Airpressure
ylims = c(988, 1010)
plot_pressure = ggplot(data=weather, aes(x=date, y=Ldruck)) +
  geom_line(colour = "cyan3") +
  ylab("air press. [hPa]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  scale_y_continuous(limits = ylims) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[5], "        Var imp sd: ", rf_varimp_sd[5])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  theme_classic()
plot_pressure 

# Regen
ylims = c(0.01, 4)
plot_rain = ggplot(data=weather, aes(x=date, y=Regen)) +
  #geom_segment(colour = "cyan3") +
  geom_line(colour = "blue2") +
  #geom_bar(stat="identity") +
  ylab("rain [mm]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  scale_y_continuous(limits = ylims) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[2], "        Var imp sd: ", rf_varimp_sd[2])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  theme_classic()
plot_rain

# solar irradiance
ylims = c(min(weather$SPN1_Total, na.rm=T), max(weather$SPN1_Total, na.rm=T))
plot_solar = ggplot(data=weather, aes(x=date, y=SPN1_Total)) +
  geom_line(colour = "orange2") +
  ylab("rad. [W/m²]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[1], "        Var imp sd: ", rf_varimp_sd[1])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  #  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_solar

# humidity
ylims = c(min(weather$relFeuchte, na.rm=T), max(weather$relFeuchte, na.rm=T))
plot_hum = ggplot(data=weather, aes(x=date, y=relFeuchte)) +
  geom_line(colour = "green4") +
  ylab("rel. hum. %]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[6], "        Var imp sd: ", rf_varimp_sd[6])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  #  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_hum

ylims = c(0, 6)
plot_wind = ggplot(data=weather, aes(x=date, y=WiGe)) +
  geom_line(colour = "deeppink3") +
  ylab("wind [m/s]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[7], "        Var imp sd: ", rf_varimp_sd[7])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_wind

ylims = c(0, 600)
plot_raind = ggplot(data=weather, aes(x=date, y=RegenDau)) +
  geom_line(colour = "purple") +
  ylab("rain dur. [s]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[3], "        Var imp sd: ", rf_varimp_sd[3])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_raind


ylims = c(0, 30)
plot_windpeak = ggplot(data=weather, aes(x=date, y=wg_peak)) +
  geom_line(colour = "deeppink3") +
  ylab("wind [m/s]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[7], "        Var imp sd: ", rf_varimp_sd[7])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_windpeak

ylims = c(435, 540)
plot_soil = ggplot(data=weather, aes(x=date, y=BF2)) +
  geom_line(colour = "deeppink3") +
  ylab("soil [m³/mm]") +
  xlab(NULL) +
  scale_x_datetime(labels = date_format("%d-%m-%y", tz = "Europe/Berlin"), 
                   breaks = date_breaks("10 days"), 
                   limits = xlims, 
                   expand = c(0, 0)) +
  geom_label(data = data.frame(x = anno_date, y = ylims[1],
                               label = paste0("Var imp mean: ", rf_varimp_avg[8], "        Var imp sd: ", rf_varimp_sd[8])), 
             aes(x = x, y = y, label = label), fill=anno_bg, alpha = alpha_bg, label.size = 0.0, vjust = vj, just = hj) +
  scale_y_continuous(limits = ylims) +
  theme_classic()
plot_soil


figure <- ggarrange(plot_solar + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                    plot_rain + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.55), "cm")),
                    plot_raind + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.18), "cm")),
                    plot_temp + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.38), "cm")),
                    plot_pressure + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0), "cm")),
                    plot_hum + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.38), "cm")),
                    plot_wind + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.55), "cm")),
                    plot_soil + rremove("xlab") + theme(plot.margin = unit(c(0,0,0,0.18), "cm")),
                    #labels = c("A", "B", "C"),
                    ncol = 1, nrow = 8, align = "h")
figure = figure + theme(plot.margin = ggplot2::margin(0.1,0.3,0.1,0.1, "cm"))
figure
ggsave(filename = paste0("plots_timeseries_prediction/environmental_variables.pdf"),
       width = 12, height = 7, figure)










