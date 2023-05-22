cloud_variogram <- function (my_geodata) {
  n_obs <- length(my_geodata$data)
  n_comps <- n_obs * (n_obs - 1) / 2
  vario <- data.frame(matrix(ncol = 2, nrow = 0))  # Create an empty dataframe
  
  for (i in 1:(n_obs-1)){
    for (j in (i+1):n_obs){
      distance <- (sqrt((my_geodata$coords[i, 1] - my_geodata$coords[j, 1])^2 + (my_geodata$coords[i, 2] - my_geodata$coords[j, 2])^2))
      weight <- 1/2 * (my_geodata$data[i] - my_geodata$data[j])^2
      vario <- rbind(vario, c(distance, weight))
    }
  }
  
  colnames(vario) <- c("distance", "semivariance")  # Set column names
  
  plot(vario$distance, vario$semivariance,
       xlab = "distance", ylab = "semivariance", main = "Cloud Variogram")
}

# Plot variogram
cloud_variogram(parana)

# Note the same could be achieved via
plot(variog(parana, option = "cloud"))

# Adjust for second order trends
cloud_variogram(parana_res_quad)

# Note this can be achieved directly via
plot(variog(parana, trend = "2nd", option = "bin"))

help(variog)
plot(variog4(parana, trend = "2nd", max.dist = 309.75), omnidirectional = T)
plot(variog4(parana, max.dist = 309.75), omnidirectional = T)