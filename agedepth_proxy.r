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

age_range <- 10
age_min <- 0
age_max <- 12000
age_steps <- 150

### Loading Inputs
input_data_proxy <- data.matrix(read.csv("Input Data 1/proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("Input Data 1/ages_1.txt", header = TRUE, sep = ""))

dft_10k <- input_data_proxy[,1]
proxy <- input_data_proxy[,2]
dft <- input_data_age[,1]
age <- input_data_age[,2]

### Fitting Age Depth Model
age_mc <- mc_sim_h(input_data_age, tries)
fit_proxy <- get_fits(input_data_proxy, dft, age_mc, tries, fitting_method, strat_check = TRUE)

age_brackets <- c(seq(age_min-age_range, age_max-age_range, age_steps), seq(age_min+age_range, age_max+age_range, age_steps))
age_brackets <- sort(age_brackets)

cols <- 2500
rows <- length(age_brackets)/2
ages <- matrix(0, nrow = length(age_brackets)/2, ncol = cols)

proxy_extract <- matrix(NA, nrow = length(age_brackets)/2, cols)
proxy_mean <- matrix(NA, nrow = length(age_brackets)/2, tries)
proxy_sds <- matrix(NA, nrow = length(age_brackets)/2, tries)
proxy_quants_1 <- matrix(NA, nrow = length(age_brackets)/2, tries)
proxy_quants_2 <- matrix(NA, nrow = length(age_brackets)/2, tries)

for(try in 1:tries){
  row_no <- 1
  
  for (age in seq(1, length(age_brackets), 2)){
    ages_to_insert <- which(fit_proxy[,try] >= age_brackets[age] & fit_proxy[,try] <= age_brackets[age+1])
    
    if (length(ages_to_insert) < cols){
      ages_to_insert <- c(ages_to_insert, rep(NA, cols - length(ages_to_insert)))}

    ages[row_no,] <- ages_to_insert
    row_no <- row_no + 1}
  
  for (i in 1:cols){
    for (j in 1:length(age_brackets)/2){
      proxy_extract[j,i] <- proxy[ages[j,i]]}}
  
  proxy_mean[,try] <- rowMeans2(proxy_extract, na.rm = T)
  proxy_quants_1[,try] <- rowQuantiles(proxy_extract, na.rm = T, probs = c(0.025))
  proxy_quants_2[,try] <- rowQuantiles(proxy_extract, na.rm = T, probs = c(0.975))
}

proxy_stats <- matrix(NA, nrow = length(age_brackets)/2, 4)
proxy_stats[,1] <- seq(age_min, age_max, age_steps)
proxy_stats[,2] <- rowMeans2(proxy_mean, na.rm = T) # Mean
proxy_stats[,3] <- rowMeans2(proxy_quants_1, na.rm = T) # Quant 02.5%
proxy_stats[,4] <- rowMeans2(proxy_quants_2, na.rm = T) # Quant 97.5%

all_stats <- get_stats(fit_proxy, input_data_proxy, dft_10k, avg = avg_method, error = error_method)
stats_extract <- all_stats[c(rownames(age_mc)),]

plot_errbars(error_method, dft, stats_extract, dft_10k, all_stats)
title(main = paste("Depth From Top Vs. Age\n", firstup(fitting_method)), 
      sub = paste("Monte Carlo Simulation, n = ", tries),
      xlab = "dft [mm]", ylab = "age [ka]")


errbar(proxy_stats[,1], proxy_stats[,2], proxy_stats[,4], proxy_stats[,3], main = "", sub = "", xlab = "", ylab = "")
lines(proxy_stats[,1], proxy_stats[,3], "l", col = "red")
lines(proxy_stats[,1], proxy_stats[,4], "l", col = "red")
lines(proxy_stats[,1], proxy_stats[,2], col = "black", lwd = 3)

title(main = expression(paste("Age Vs. ", delta^18, "O Proxy")), 
      xlab = "age [ka]", ylab = expression(paste(delta^18, "O Proxy [\211]"),))

grid()

### Finished