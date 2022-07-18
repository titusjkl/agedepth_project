rm(list = ls(envir = globalenv()), envir = globalenv())
library(Hmisc)
library(matrixStats)
setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
source("functions.r")

### Setting Up Variables

input_data_proxy <- data.matrix(read.csv("proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("ages_1.txt", header = TRUE, sep = ""))

dft_10k <- input_data_proxy[,1]

dft <- input_data_age[,1]
age <- input_data_age[,2]

tries <- 10^4
age_mc <- mc_sim_h(input_data_age, tries)

### Fitting Age Depth Model

all_fitted <- matrix(0,
  nrow = (nrow(input_data_proxy)), ncol = tries)

for (col in 1:tries){
  all_fitted[,col] <- to_depth_age(input_data_proxy, dft, age_mc[,col], "spline")
}

### Calculating Age Averages, Age Errors

all_stats <- get_stats(all_fitted, input_data_proxy, dft_10k, avg = "median")
stats_extract <- all_stats[c(rownames(age_mc)),]

### Plotting Models

# plot(dft, age, "p", pch = 16, col=rgb(0, 0, 0, 0))
# for (col in 1:tries){
#   lines(dft_10k, all_fitted[,col], "l", col=rgb(1, 0, 0, 0.1))
# }
# points(dft, age, pch = 16, col=rgb(0, 0, 0, 1))
#
# plot(dft_10k, all_stats[,1], "l", pch = 16, col = "black", 
#      xlim = c(0,6000), ylim = c(0,10000))
# lines(dft_10k, all_stats[,1] + all_stats[,2], "l", col = "red")
# lines(dft_10k, all_stats[,1] - all_stats[,2], "l", col = "red")
# points(dft, stats_extract[,1], pch = 16, col = "black")
# grid()

errbar(dft, stats_extract[,1], stats_extract[,1]+stats_extract[,2], stats_extract[,1]-stats_extract[,2], 
       xlim = c(0,6000), ylim = c(0,10000))
lines(dft_10k, all_stats[,1] + all_stats[,2], "l", col = "red")
lines(dft_10k, all_stats[,1] - all_stats[,2], "l", col = "red")
lines(dft_10k, all_stats[,1], col = "black")
grid()

### Finished

print("Finished.")
