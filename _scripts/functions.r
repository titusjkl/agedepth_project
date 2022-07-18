mc_sim_h <- function(input_age, tries=10^3) {
  normdist <- apply(input_age, 1, function(x) rnorm(n = tries, mean = x[2], sd = x[3]))
  output_data <- t(normdist)
  
  rownames(output_data) <- input_age[,1]
  
  return(output_data)
}


to_depth_age <- function(input_proxy, input_dft, input_age, method = "linear", output = "vector", plot = FALSE){
  if (method == "linear"){
    fitted <- approx(input_dft, input_age, xout = input_proxy[,1])
  } 
  else if (method == "linear extrap"){
    fitted <- approxExtrap(input_dft, input_age, xout = input_proxy[,1])
  }
  else if (method == "spline"){
    fitted <- spline(input_dft, input_age, xout = input_proxy[,1])
  }
  
  if (plot == TRUE){
    plot(fitted$y, input_proxy[,2], 
         type="l", 
         main = toupper(method))
  }
  
  if (output == "vector"){
    return(fitted$y)
  }
  else{
    return(output_data <- cbind(input_proxy, age))
  }
}


get_stats <- function(input_matrix, input_proxy, row_names, quants = c(0.25, 0.75), avg = "mean", error = "sd"){
  all_avgs <- matrix(0, nrow = nrow(input_proxy), ncol = 3)
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
  else if (error == "quant"){
    all_avgs[,2] <- rowQuantiles(input_matrix, probs = quants)[,1]
    all_avgs[,3] <- rowQuantiles(input_matrix, probs = quants)[,2]
  }
  
  return(all_avgs)
}
