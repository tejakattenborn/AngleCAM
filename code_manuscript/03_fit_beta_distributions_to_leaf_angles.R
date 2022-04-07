
# -------- Description
# This script fits beta distributions leaf angle samples. The script will iteratate over individual leaf samples sets (where one sample set corresponds to one image).
# the mle method results in error estimates for both beta coefficients (alpha, beta). These error estimates are then used to augment the labels (50 variants per default)
# The augmented labels are used in the CNN training (model regularization), where in each epoch a random variant is used as reference.
# The script exports the fitted beta distribution and the 50 variants as .csv file that will be loaded during CNN training.

require(fitdistrplus)

# set working directory
setwd("INSERT DIR")

#fraction of the standard deviation (0-1)
sd_frac = 0.2 # 0.50
sim_per_pic = 50
angle_resolution = 45 # angle resolution
scaling_factor = 10 #used to rescale the values; to low numbers will slow down NN leanring, to high values will result in too fast updates
ncp = 0 # ncp corrects for bias, just for testing

# read all files
samples_files = list.files(pattern=".csv", recursive = TRUE); samples_files
samples_files = samples_files[-grep("sim", samples_files)]; samples_files

#samples_files = samples_files[-grep("results", samples_files)]; samples_files
samples = lapply(samples_files, read.csv, sep=",", header = T)

# #just for repairing / error checking
# for(i in 1:length(samples_files)){
#   read.csv(samples_files[i])
# }
# samples_files[i];i


# fit distribution to all data
for(i in 1:length(samples)){
  
  # produce multiple simulations per pic (n = sim_per_pic)
  # one file for each pic with original distribution (n = 1) in the first row and simulated distributions with errors (n = sim_per_pic) in subsequent rows
  # in read from data read text file for each pic containing the simulations
  # use the simulations for training and the original rows for validation
  
  # load file and fit beta dist
  obs_all = samples[[i]]
  obs_angle_scaled = obs_all$angle/90 # from degree (0-90) to 0-1
  obs_angle_scaled[obs_angle_scaled == 0] = 0.00001 # dbeta cannot be fitted to zeros. Replace zeros with small value.
  obs_angle_scaled = obs_angle_scaled - 0.000001 # ...also cannot fit 1, so we have to subtract  marginal value
  dist = fitdist(obs_angle_scaled, "beta", method = "mle")
  
  # predict distribution without fitted coefficients (onwards reference)
  beta = dbeta(seq(0, 1, length.out = angle_resolution), dist$estimate[1], dist$estimate[2], ncp = ncp, log = FALSE)
  beta[which(beta==Inf)] = 0
  beta = beta/sum(beta)*scaling_factor
  
  # predict multiple distributions incorporating the estimated errors (sd); onwards used for response based data augmentation
  sims = matrix(NA, nrow = sim_per_pic, ncol = angle_resolution)
  for(ii in 1:sim_per_pic){
    beta1 = dbeta(seq(0, 1, length.out = angle_resolution), dist$estimate[1] + rnorm(1, 0, dist$sd[1])*sd_frac, dist$estimate[2] + rnorm(1, 0, dist$sd[1])*sd_frac, ncp = 0, log = FALSE)
    beta1[which(beta1==Inf)] = 0
    #beta1 = beta1*is.finite(beta1) # remove inf (sometimes happens...)
    sims[ii,] = beta1/sum(beta1)*scaling_factor
  }
  sims = sims[complete.cases(sims),-c(1,angle_resolution)] # remove NA, i.e. cases where no beta could be simulated (sampled values out of range), and remove first and last column since they always will be 0
  all = rbind(beta[-c(1,angle_resolution)], sims)
  # scale from 0-1 to 0-0.9 (degree * 0.01; 90 degree --> 0.9)
  all = all*90*0.01
  # write beta distribution variants to file
  write.table(all, paste0(sub(pattern = "(.*)\\..*$", replacement = "\\1", samples_files[i]), "_sim.csv"), row.names = F, col.names = F)
}


