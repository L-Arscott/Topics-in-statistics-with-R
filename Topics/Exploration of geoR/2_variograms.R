### Variograms (parana dataset with geoR)
library(geoR)

# Euclidean distance between data entries i and j
euclid_distance <- function(my_geodata, i, j) {
  x_dif <- my_geodata$coords[i, 1] - my_geodata$coords[j, 1]
  y_dif <- my_geodata$coords[i, 2] - my_geodata$coords[j, 2]
  return(sqrt(x_dif^2 + y_dif^2))
}

# Variogram function
# For each pair of points, plots their semivariance against the distance
# separating them.
cloud_variogram <- function (my_geodata) {
  n_obs <- length(my_geodata$data)
  n_comps <- n_obs * (n_obs - 1) / 2
  vario <- data.frame(matrix(ncol = 2, nrow = 0))  # Create an empty dataframe
  
  for (i in 1:(n_obs-1)){
    for (j in (i+1):n_obs){
      distance <- euclid_distance(my_geodata, i, j)
      wt <- 1/2 * (my_geodata$data[i] - my_geodata$data[j])^2  # semivariance
      vario <- rbind(vario, c(distance, wt))
    }
  }
  
  colnames(vario) <- c("distance", "semivariance")  # Set column names
  
  plot(vario$distance, vario$semivariance,
       xlab = "Distance", ylab = "Semivariance", main = "Cloud Variogram")
}

# Plot variogram
cloud_variogram(parana)

# Note the same could be achieved via
plot(variog(parana, option = "cloud"))

# We can also divide data points into bins and compute the mean for each bin:
plot(variog(parana, option = "bin"))

# We may adjust for second order trends:
plot(variog(parana, trend = "2nd", option = "bin"))

# We may also specify a maximum distance, chosen here to be half the maximum
# distance between points
our_variogram = variog(parana, trend = "2nd", option = "bin", max.dist = 309.75)
plot(our_variogram)

# Fit covariance model to variogram.
# Available functions include "matern", "exponential", "gaussian", "spherical"
exp_fit_fix <- variofit(our_variogram, cov.model = "exp", fix.nugget = F)

# We may choose best fit by considering sum of squares
print(summary(exp_fit_fix)$sum.of.squares)

# Plot our fit
plot(our_variogram, pch = 16)
lines(exp_fit_fix, col = "green", lwd = 4, lty = "solid")
