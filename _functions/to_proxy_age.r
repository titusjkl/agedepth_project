# rm(list = ls(envir = globalenv()), envir = globalenv())
# if(!is.null(dev.list())) dev.off()

# setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")

# input_proxy <- read.csv("proxy_data_1.txt", header = TRUE, sep = "")
# input_age <- read.csv("ages_1.txt", header = TRUE, sep = "")


to_age_proxy <- function(input_proxy, input_age, method = "linear", plot = TRUE, to_txt = FALSE){

  if (method == "linear"){
    fitted <- approx(input_age$depth, input_age$age, xout = input_proxy$depth)
  } 
  else if (method == "spline"){
    fitted <- spline(input_age$depth, input_age$age, xout = input_proxy$depth)
  }

  age <- fitted$y
  proxy_signal <- input_proxy$proxy_signal
  
  if (plot == TRUE){
    plot(age, proxy_signal, type="l")
  }  
  
  output_data <- cbind(input_proxy, age)
  
  if (to_csv == TRUE){
    write.table(output_data,file="agedepth_model.txt")
  }
  
  return(output_data)
}

# age_proxy <- to_age_proxy(input_proxy, input_age, method = "linear")