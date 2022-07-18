### Initial Setup
setwd("D:/Dokumente/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
rm(list = ls(envir = globalenv()), envir = globalenv())
source("agedepth_functions.r")
set.seed(5)

### User Inputs
tries <- 10^2
fitting_method <- "lin"   # lin, lin ex, spline
avg_method <- "median"    # mean, median
error_method <- "quants"  # sd, quants

### Loading Inputs
input_data_proxy <- data.matrix(read.csv("Input Data 2/proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("Input Data 2/ages_1.txt", header = TRUE, sep = ""))
age_mc <- mc_sim_h(input_data_age, tries)

dft_10k <- input_data_proxy[,1]
proxy <- input_data_proxy[,2]
dft <- input_data_age[,1]
age <- input_data_age[,2]

### Fitting Age Depth Model Before And After Strat Check
all_fitted_unchecked <- get_fits(input_data_proxy, dft, age_mc, tries, fitting_method, strat_check = FALSE)
all_fitted <- get_fits(input_data_proxy, dft, age_mc, tries, fitting_method, strat_check = TRUE)

### Calculating Averages & Errors
all_stats_unchecked <- get_stats(all_fitted_unchecked, input_data_proxy, dft_10k, avg = avg_method, error = error_method)
stats_extract_unchecked <- all_stats_unchecked[c(rownames(age_mc)),]

all_stats <- get_stats(all_fitted, input_data_proxy, dft_10k, avg = avg_method, error = error_method)
stats_extract <- all_stats[c(rownames(age_mc)),]

### Plotting
plot_errbars(error_method, dft, stats_extract_unchecked, dft_10k, all_stats_unchecked)
title(main = "Depth From Top Vs. Age", 
      sub = paste("Before Checking Stratigraphy, n = ", tries),
      xlab = "dft [mm]", ylab = "age [ka]")

plot(all_stats_unchecked[,1], proxy, "l", main = "", sub = "", xlab = "", ylab = "")
grid()
title(main = expression(paste("Age Vs. ", delta^18, "O Proxy")), 
      sub = paste("Before Checking Stratigraphy, n = ", tries),
      xlab = "age [ka]", expression(paste(delta^18, "O Proxy [\211]"),))

plot_errbars(error_method, dft, stats_extract, dft_10k, all_stats)
title(main = "Depth From Top Vs. Age", 
      sub = paste("After Checking Stratigraphy, n = ", tries),
      xlab = "dft [mm]", ylab = "age [ka]")

plot(all_stats[,1], proxy, "l", main = "", sub = "", xlab = "", ylab = "")
grid()
title(main = expression(paste("Age Vs. ", delta^18, "O Proxy")), 
      sub = paste("After Checking Stratigraphy, n = ", tries),
      xlab = "age [ka]", expression(paste(delta^18, "O Proxy [\211]"),))

# errbar(dft, stats_extract[,1], stats_extract[,3], stats_extract[,2])
# title("Simulation, Age")
# 
# input_data_result <- data.matrix(read.csv("Kontroll Daten/results.txt", header = TRUE, sep = ";"))
# errbar(input_data_result[,1], input_data_result[,2], input_data_result[,3],input_data_result[,4])
# title("Results, Age")
# 
# input_data_check <- data.matrix(read.csv("Kontroll Daten/proxy_data_t_1.txt", header = TRUE, sep = ""))
# plot(input_data_proxy[,1], input_data_proxy[,2], "l")
# title("Control, Proxy")

### Finished