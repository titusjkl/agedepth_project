rm(list = ls(envir = globalenv()), envir = globalenv())

library(Hmisc)
library(e1071)
setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
source("functions.r")
set.seed(5)


input_data_proxy <- read.csv("proxy_data_1.txt", header = TRUE, sep = "")
input_data_age <- read.csv("ages_1.txt", header = TRUE, sep = "")
dft_10k <- input_data_proxy$depth


all_fitted <- read.csv("all_fitted.txt", header = TRUE, sep = "")


all_stats <- get_stats(all_fitted, input_data_proxy, dft_10k)

plot(input_data_proxy$depth, all_stats[,1], "l")
lines(input_data_proxy$depth, all_stats[,1] + all_stats[,3], "l", col = "red")
lines(input_data_proxy$depth, all_stats[,1] - all_stats[,3], "l", col = "red")