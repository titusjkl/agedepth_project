# rm(list = ls(envir = globalenv()), envir = globalenv())
# if(!is.null(dev.list())) dev.off()

# setwd("~/Seafile/Seafile/Geowissenschaften/B. Sc/6. Semester/Geostatistik 2/agedepth")


# input_data <- read.csv("ages_1.txt", header = TRUE, sep = "")


mc_sim <- function(input_data, tries=10^3) {
  
  normalized <- apply(input_data, 1, function(x) rnorm(n = tries, mean = x[2], sd = x[3]))
  output_data <- rbind(t(input_data[,1]), normalized)
  
  return(output_data)
}


# mc_sim_vector_vertical <- function(input_data, tries=10^3) {
  
  # normalized <- apply(input_data, 1, function(x) rnorm(n = tries, mean = x[2], sd = x[3]))
  # output_data <- cbind(input_data[,1], t(normalized))
  
  # return(output_data)
# }


# tries <- 10^4

# system.time(mat_horizontal <- mc_sim(input_data, tries))
# system.time(mat_vertical <- mc_sim_vector_vertical(input_data, tries))
