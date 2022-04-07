

# -------- Description
# This script compares TLSLeAF outputs (leaf surface angles) for individual plants with leaf angle estimatates from the AngleCam method
# Leaf angles are compared on the basis of leaf angle distributions and average leaf angles
# The leaf angle distributions for TLSLeAF were derived by aggregating a distribution over all points.


require(data.table)
#devtools::install_bitbucket("derpylz/babyplots.git")
require(babyplots)
library(viridis) # https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
require(ggplot2)

setwd("F:/data/data_brinno/")

angle_resolution = 44
scaling_factor = 10 #used to rescale the values to the range predicted by AngleCam; values were rescaled, since low values will slow the convergence of the CNN models for AngleCam (see respective script)

load("INSERT DIR")
brinno_freq = tls_preds # the AngleCam predictions were named (tls_preds; i.e. predictions on the TLS dataset)

### clean predictions (remove outliers)
cleaner = function(x){
  errors = which(x < 0 | x > 0.75)
  x[errors] = 0
  return(x)
}
brinno_freq = lapply(brinno_freq, cleaner)

### load and preprocess TLS
dir_tls = "data_2022_brinno_tls_validation/2022_01_tls/2022_01_tls_single_plants_with_angle/"

tls_files = list.files(dir_tls, full.names = T, pattern = "_angle.txt")
tls_files


### calculate / compare leaf angle distributions  -----------------------------

# conversion to leaf angle distributions
breaks = seq(0, 90, length.out = angle_resolution)
to_interval = function(x){
  to_breaks = cut(tls$dip, breaks, right=FALSE)
  to_freq = as.numeric(table(to_breaks))
  to_freq = to_freq/sum(to_freq)*scaling_factor
}

tls_freq = matrix(NA, nrow = length(tls_files), ncol = angle_resolution-1)
rownames(tls_freq) = basename(tls_files)
for(i in 1:length(tls_files)){
  tls = fread(tls_files[i])
  tls_freq[i,] = to_interval(tls$dip)
}

res_p = c()
res_w = c()
nmae_range = c()
nmae_sd = c()
range_1 = c()
range_2 = c()

for(i in 1:nrow(tls_freq)){
  tls = tls_freq[i,]
  brinno = colMeans(brinno_freq[[i]])
  res_p[i] <- as.numeric(stats::wilcox.test(x = tls, y= brinno, data = comp, paired = T)$p.value)
  res_w[i] <- as.numeric(stats::wilcox.test(x = tls, y= brinno, data = comp, paired = T)$statistic)
  nmae_range[i] = mean((tls - brinno) / diff(range(tls_freq))) * 100
  nmae_sd[i] = mean((tls - brinno) / sd(tls)) * 100
  range_1[i] =  diff(range(tls))
  range_2[i] =  diff(range(tls_freq))
}

# check output
nmae_range
nmae_sd
mean(nmae_range)
sd(nmae_sd)
range_1
range_2

# calc nrmse
sqrt(mean((tls - brinno)^2)) / diff(range(tls))

round(unlist(res_p), 4)
nrow(tls_freq)
length(which(round(unlist(res_p), 4)>0.05))
res_w
plot(tls)
lines(brinno)

# export plots
plot(1, ylim = c(0,0.8), xlim = c(0,90))
for(ii in 1:nrow(brinno_freq[[i]])){
  lines(breaks[-1], brinno_freq[[i]][ii,], col = rgb(0.5, 0.5, 0.5, 0.3), lwd= 3)
}
lines(breaks[-1], tls_freq[i,], col = rgb(0.5, 0.5, 0.7, 1), lwd = 7)
tls_test = fread(tls_files[i])
t = pointCloud(cbind(tls_test$X, tls_test$Y, tls_test$Z), colorBy = 'values', colorVar = tls_test$dip, upAxis = "+z",
           name = "test",
           size = 2,
           colorScale = "custom", #viridis(90, direction = -1),# "viridis", #"custom", # "viridis",
           customColorScale = viridis(90, direction = -1),
           #customColorScale = c("lightblue", "cyan", "yellow"),
           backgroundColor = "#ffffffff",
           showAxes = T,
           turntable = F)
t
i; names(brinno_freq)[i]; basename(tls_files[i])
i = i+1

plot(1, ylim = c(0,0.8), xlim = c(0,90))
for(ii in 1:nrow(brinno_freq[[i]])){
  lines(breaks[-1], brinno_freq[[i]][ii,], col = rgb(0.5, 0.5, 0.5, 0.3), lwd= 3)
}
lines(breaks[-1], tls_freq[i,], col = rgb(0.5, 0.5, 0.7, 1), lwd = 7)

line_color = "black"
line_size = 1
col_brinno = "cyan3" # "deeppink3"  
col_tls = "yellow2"#"darkolivegreen" # "lightseagreen"

for(i in 1:length(tls_files)){

  df = data.frame(breaker = c(0,breaks[-1],90),
                  tls = c(0, tls_freq[i,],0),
                  brinno = c(0, colMeans(brinno_freq[[i]]),0))
  mean_tls = sum(breaks[-1] * tls_freq[i,])/10
  mean_brinno = sum(breaks[-1] * colMeans(brinno_freq[[i]]))/10
  
  p <- ggplot(data=df, aes(x=breaker)) + 
    geom_polygon(aes(y=tls), fill=col_tls, colour = line_color, size = line_size, alpha=0.3) +
    geom_polygon(aes(y=brinno), fill=col_brinno, colour = line_color, size = line_size, alpha=0.3) +
    xlab("leaf angle [°]") + ylab("density") +
    theme_minimal()
  ggsave(filename = paste0("data_2022_brinno_tls_validation/2022_01_results/compare_density_tls_plant_", sprintf("%02d", i), ".pdf"),
         width = 3, height = 3, p)
}


### calculate / compare average leaf angles  -----------------------------

# derive average leaf angle from leaf angle distributions
avg_angle = function(x){
  sum(breaks[-1] * x/10)
}
avg_tls = apply(tls_freq, 1, avg_angle)
avg_brinno = c()
for(i in 1:length(brinno_freq)){
  avg_brinno[i] = mean(apply(brinno_freq[[i]], 1, avg_angle))
}

# correlation (pearson)
cor.test(avg_brinno, avg_tls)$estimate^2
# linear model comparison
lm(avg_tls ~ avg_brinno)

avg = data.frame(avg_tls = avg_tls, avg_brinno = avg_brinno)

# plot fit
lims = c(25, 75)
plot = ggplot(avg, aes(x=avg_brinno, y=avg_tls)) +
  geom_abline(slope = 1, linetype = "dashed", color="grey", size=1) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, col = "cyan3") +
  scale_x_continuous(limits = lims) +
  scale_y_continuous(limits = lims) +
  coord_fixed() +
  annotate(geom="text", x=50, y=30, label = paste0("R² = ", round(as.numeric(cor.test(avg$avg_tls, avg$avg_brinno)$estimate^2), 2))) +
  xlab("Avg. angle Brinno") +
  ylab("Avg. angle TLS") +
  theme_classic()
plot
ggsave(filename = "data_2022_brinno_tls_validation/2022_01_results/compare_avg_brinni_tls.pdf",
       width = 3, height = 3, plot)

