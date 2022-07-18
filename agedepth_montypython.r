### Initial Setup
setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
rm(list = ls(envir = globalenv()), envir = globalenv())
source("agedepth_functions.r")
set.seed(5)

### User Inputs
tries <- 10^3
fitting_method <- "lin"   # lin, lin ex, spline
avg_method <- "mean"    # mean, median
error_method <- "quants"  # sd, quants

### Loading Inputs
input_data_proxy <- data.matrix(read.csv("Input Data 1/proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("Input Data 1/ages_1.txt", header = TRUE, sep = ""))
age_mc <- mc_sim_h(input_data_age, tries)

dft_10k <- input_data_proxy[,1]
proxy <- input_data_proxy[,2]
dft <- input_data_age[,1]
age <- input_data_age[,2]

### Fitting Age Depth Model
all_fitted <- get_fits(input_data_proxy, dft, age_mc, tries, fitting_method, strat_check = TRUE)

### Calculating Averages & Errors
all_stats <- get_stats(all_fitted, input_data_proxy, dft_10k, avg = avg_method, error = error_method)
stats_extract <- all_stats[c(rownames(age_mc)),]

### Plotting
plot_errbars(error_method, dft, stats_extract, dft_10k, all_stats)
title(main = paste("Depth From Top Vs. Age\n", firstup(fitting_method)), 
      sub = paste("Monte Carlo Simulation, n = ", tries),
      xlab = "dft [mm]", ylab = "age [ka]")

# plot(all_stats[,1], proxy, "l")
# grid()

### Finished

