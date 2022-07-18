library(Hmisc)
library(matrixStats)
library(stringr)
library(schoolmath)


mc_sim_h <- function(input_age, tries=10^3) {
  normdist <- apply(input_age, 1, function(x) rnorm(n = tries, mean = x[2], sd = x[3]))
  output_data <- normdist
  
  if (tries != 1){
    output_data <- t(normdist)
    rownames(output_data) <- input_age[,1]
  }
  
  return(output_data)
}

to_depth_age <- function(input_proxy, input_dft, input_age, method = "lin", output = "vector"){
  if (method == "lin"){
    fitted <- approx(input_dft, input_age, xout = input_proxy[,1])
  } 
  else if (method == "lin ex"){
    fitted <- approxExtrap(input_dft, input_age, xout = input_proxy[,1])
  } 
  else if (method == "spline"){
    fitted <- spline(input_dft, input_age, xout = input_proxy[,1])
  }
  
  if (output == "vector"){
    return(fitted$y)
  }
  else{
    return(output_data <- cbind(input_proxy, age))
  }
}

get_fits <- function(input_data_proxy, dft, age_mc, tries, fitting_method = "lin", strat_check = TRUE){
  all_fitted <- matrix(0, nrow = (nrow(input_data_proxy)), ncol = tries)
  
  if (all(is.positive(diff(age_mc))) == FALSE & strat_check == TRUE){
    for (try in 1:tries){
      while (all(is.positive(diff(age_mc[,try]))) == FALSE){
        age_mc[,try] <- mc_sim_h(input_data_age, 1)}}
    
    for (col in 1:tries){
      all_fitted[,col] <- to_depth_age(input_data_proxy, dft, age_mc[,col], fitting_method)}}
    
  else {
    for (col in 1:tries){
      all_fitted[,col] <- to_depth_age(input_data_proxy, dft, age_mc[,col], fitting_method)}}

  return(all_fitted)
}

get_stats <- function(input_matrix, input_proxy, row_names, 
                      quants = c(0.025, 0.975), avg = "mean", error = "sd"){
  if (error ==  "quants"){
    col_no <- 3
  }
  else{
    col_no <- 2
  }
  
  all_avgs <- matrix(0, nrow = nrow(input_proxy), ncol = col_no)
  rownames(all_avgs) <- row_names
  
  if (avg == "mean") {
    all_avgs[,1] <- rowMeans2(input_matrix)
  } 
  else if (avg == "median"){
    all_avgs[,1] <- rowMedians(input_matrix)
  }
  
  if (error == "sd"){
    all_avgs[,2] <- rowSds(input_matrix)
  } 
  else if (error == "quants"){
    all_avgs[,2] <- rowQuantiles(input_matrix, probs = quants)[,1]
    all_avgs[,3] <- rowQuantiles(input_matrix, probs = quants)[,2]
  }
  
  return(all_avgs)
}

plot_errbars <- function(error_type, dft, stats_extract, dft_10k, all_stats){
  if(error_type == "sd"){
    errbar(dft, stats_extract[,1], stats_extract[,1]+stats_extract[,2], stats_extract[,1]-stats_extract[,2], 
           xlim = c(0,max(dft_10k, na.rm = TRUE)+500), ylim = c(0,max(all_stats[,1], na.rm = TRUE)+1000), 
           xlab = NULL, ylab = NULL,
           xaxs = "i", yaxs = "i",)
    
    lines(dft_10k, all_stats[,1] + all_stats[,2], "l", col = "red")
    lines(dft_10k, all_stats[,1] - all_stats[,2], "l", col = "red")
  } 
  else if (error_type == "quants"){
    errbar(dft, stats_extract[,1], stats_extract[,3], stats_extract[,2], 
           xlim = c(0,max(dft_10k, na.rm = TRUE)+500), ylim = c(0,max(all_stats[,1], na.rm = TRUE)+1000), 
           xlab = "", ylab = "",
           xaxs = "i", yaxs = "i",)
    
    lines(dft_10k, all_stats[,3], "l", col = "red")
    lines(dft_10k, all_stats[,2], "l", col = "red")}
  
  lines(dft_10k, all_stats[,1], col = "black")
  axis(1, seq(0, max(dft_10k, na.rm = TRUE)+100, 1000))
  axis(2, seq(0, max(all_stats[,1], na.rm = TRUE)+1000, 2000))

  grid()
}


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
