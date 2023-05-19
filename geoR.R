# Parana data set: average rainfall over different years for the period May-June
# collected at 143 recording stations throughout Parana State, Brasil.

library(geoR)

summary(parana)
plot(parana)

### Trends
## Adjusting for first order trends
lin_model <- lm(parana$data ~ parana$coords)
summary(lin_model)
residuals <- parana$data - predict(lin_model, data.frame(parana$coords))

parana_res_lin <- parana
parana_res_lin$data <- residuals

plot(parana_res_lin)
# Note the same can be achieved through the use of plot(parana, trend="1st")

## Second order
poly_features <- poly(cbind(parana$coords), degree = 2, raw=TRUE)
quad_model <- lm(parana$data ~ poly_features)

quad_res <- parana$data - predict(quad_model, poly_features)
parana_res_quad <- parana
parana_res_quad$data <- quad_res
plot(parana_res_quad)
# Note the same can be achieved through the use of plot(parana, trend="2nd")


### Variograms
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
plot(variog(parana, trend = "2nd", option = "cloud"))
