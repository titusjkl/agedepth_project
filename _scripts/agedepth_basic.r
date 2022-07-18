### Initial Setup
setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
rm(list = ls(envir = globalenv()), envir = globalenv())
source("agedepth_functions.r")
set.seed(5)

### User Inputs
fitting_method <- "lin"   # lin, lin ex, spline

### Loading Inputs
input_data_proxy <- data.matrix(read.csv("Input Data 1/proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("Input Data 1/ages_1.txt", header = TRUE, sep = ""))

dft_10k <- input_data_proxy[,1]
proxy <- input_data_proxy[,2]
dft <- input_data_age[,1]
age <- input_data_age[,2]

### Fitting Age Depth Model
all_fitted <- to_depth_age(input_data_proxy, dft, age, fitting_method)

### Plotting
plot(dft_10k, all_fitted, "l")
points(dft, age)

plot(all_fitted, proxy, "l")

### Finished