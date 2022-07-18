### Initial Setup
setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")
rm(list = ls(envir = globalenv()), envir = globalenv())
source("agedepth_functions.r")
set.seed(5)

### User Inputs
tries <- 10^4
fitting_method <- "lin" # lin, lin ex, spline
avg_method <- "mean"    # mean, median
error_method <- "quants"    # sd, quants

### Loading Inputs
input_data_proxy <- data.matrix(read.csv("Input Data 1/proxy_data_1.txt", header = TRUE, sep = ""))
input_data_age <- data.matrix(read.csv("Input Data 1/ages_1.txt", header = TRUE, sep = ""))
age_mc <- mc_sim_h(input_data_age, tries)

dft_10k <- input_data_proxy[,1]
proxy <- input_data_proxy[,2]

dft <- input_data_age[,1]
age <- input_data_age[,2]

### Simulations
fitted_age <- matrix(0, nrow = (length(dft_10k)), ncol = 2)
fitted_age[,1] <- to_depth_age(input_data_proxy, dft, age, method = "lin")
fitted_age[,2] <- to_depth_age(input_data_proxy, dft, age, method = "spline")

all_fitted_lin <- matrix(0, nrow = (nrow(input_data_proxy)), ncol = tries)
all_fitted_spline <- matrix(0, nrow = (nrow(input_data_proxy)), ncol = tries)

for (col in 1:tries){
        all_fitted_lin[,col] <- to_depth_age(input_data_proxy, dft, age_mc[,col], "lin")}
for (col in 1:tries){
        all_fitted_spline[,col] <- to_depth_age(input_data_proxy, dft, age_mc[,col], "spline")}

all_stats_lin_sd <- get_stats(all_fitted_lin, input_data_proxy, dft_10k, "mean", "sd")
stats_extract_lin_sd <- all_stats_lin_sd[c(rownames(age_mc)),]

all_stats_lin_quants <- get_stats(all_fitted_lin, input_data_proxy, dft_10k, "mean", "quants")
stats_extract_lin_quants <- all_stats_lin_quants[c(rownames(age_mc)),]

all_stats_spline_sd <- get_stats(all_fitted_spline, input_data_proxy, dft_10k, "mean", "sd")
stats_extract_lin_sd <- all_stats_spline_sd[c(rownames(age_mc)),]

all_stats_spline_quants <- get_stats(all_fitted_spline, input_data_proxy, dft_10k, "mean", "quants")
stats_extract_lin_quants <- all_stats_spline_quants[c(rownames(age_mc)),]


# ### Plot von Tiefe gegen d18O Proxy
# plot(dft_10k, proxy, "l", col = "aquamarine4",
#      main = expression(paste("Depth from Top Vs. ", delta^18, "O Proxy")),
#      xlab = "dft [mm]",
#      ylab = expression(paste(delta^18, "O Proxy [\211]"),
#      xaxs = "i", yaxs = "i"))
# grid()
# 
# ### Plot von Tiefe gegen Alter
# plot(dft, age, "p" , col = "brown4",
#      main = "Depth From Top Vs. Age",
#      xlab = "dft [mm]",
#      ylab = "age [ka]",
#      xaxs = "i", yaxs = "i",
#      xlim = c(0, max(dft)+500),
#      ylim = c(0, max(age)+500),
#      pch = 16)
# grid()
# 
# ### Plot von Tiefe gegen Alter, Interpoliert
# plot(dft, age, "p" , col = "brown4",
#      main = "Depth From Top Vs. Age\nInterpolation Methods",
#      xlab = "dft [mm]",
#      ylab = "age [ka]",
#      xaxs = "i", yaxs = "i",
#      xlim = c(0, max(dft)+500),
#      ylim = c(0, max(age)+500),
#      pch = 16)
# 
# lines(dft_10k, fitted_age[,1], col = "red", lwd = 3)
# lines(dft_10k, fitted_age[,2], col = "blue", lty = "dashed", lwd = 3)
# grid()
# legend("topleft", inset = 0.05, legend=c("Linear Interpolation", "Spline"),
#        col=c("red", "blue"), lty=1:2, cex=0.8, box.lty = 0)
# # 
# ### Plot von Alter gegen d18O Proxy
# plot(fitted_age[,1], proxy, "l",
#      col = "brown4",
#      main = expression(paste("Age Vs. ", delta^18, "O Proxy")),
#      sub = "Linear Interpolation",
#      xlab = "age [ka]",
#      ylab = expression(paste(delta^18, "O Proxy [\211]"),
#      xaxs = "i", yaxs = "i",))
# grid()
# #
### MC - Tiefe gegen Alter
plot(dft,age_mc[,1], "p",
        col = "brown4",
        main = "Depth From Top Vs. Age",
        sub = paste("Monte Carlo Simulation, n = ", tries),
        xlab = "dft [mm]",
        ylab = "age [ka]",
        xaxs = "i", yaxs = "i",
        xlim = c(0, max(dft)+500),
        ylim = c(0, max(age)+1000),)

for(try in 2:tries){
        if (try %% 100 == 0){
        points(dft,age_mc[,try])}}
grid()

### MC - Tiefe gegen Alter, Interpoliert - Linear
plot(dft, age_mc[,1], "p",
     main = "Depth From Top Vs. Age \nLinear Interpolation",
     sub = paste("Monte Carlo Simulation, n = ", tries),
     xlab = "dft [mm]",
     ylab = "age [ka]",
     xaxs = "i", yaxs = "i",
     xlim = c(0, max(dft)+500),
     ylim = c(0, max(age)+1000),
     pch = 16,)

lines(dft_10k, all_fitted_lin[,1], col = rgb(0,0,1,0.1))

for(try in 2:tries){
        #points(dft,age_mc[,try])
        if (try %% 100 == 0){
        points(dft,age_mc[,try])
        lines(dft_10k, all_fitted_lin[,try], col = rgb(1,0,0,0.1))}}
grid()
#
# ### MC - Tiefe gegen Alter, Interpoliert - Spline
# plot(dft, age_mc[,1], "p",
#      main = "Depth From Top Vs. Age \nSpline",
#      sub = paste("Monte Carlo Simulation, n = ", tries),
#      xlab = "dft [mm]",
#      ylab = "age [ka]",
#      xaxs = "i", yaxs = "i",
#      xlim = c(0, max(dft)+500),
#      ylim = c(0, max(age)+1000),
#      pch = 16,)
# 
# lines(dft_10k, all_fitted_spline[,1], col = rgb(0,0,1,0.1))
# 
# for(try in 2:tries){
#         if (try %% 100 == 0){
#         points(dft,age_mc[,try])
#         lines(dft_10k, all_fitted_spline[,try], col = rgb(0,0,1,0.1))}}
# grid()


