### Exploration of geoR package
# Parana data set: average rainfall over different years for the period May-June
# collected at 143 recording stations throughout Parana State, Brasil.

library(geoR)

# Summarise and plot our data:
summary(parana)
plot(parana)
# The top-left plot shows data points colour-coded according to quartile
# 0% blue 25% green 50% yellow 75% red 100%

# Stationarity is a common assumption in geospatial analysis techniques
# (ex: kriging). Our data shows clear spatial trend. To allow for kriging,
# we adjust for trends


## Adjusting for first order trends
# Fit linear model using x, y coordinates
lin_model <- lm(parana$data ~ parana$coords)
summary(lin_model)
residuals <- parana$data - predict(lin_model, data.frame(parana$coords))

# Create new geodata object using residuals as data
parana_res_lin <- parana
parana_res_lin$data <- residuals

# Plot
plot(parana_res_lin)
# We adjusted for trends manually to confirm our knowledge, however
# note the same can be achieved through the use of plot(parana, trend="1st")

# There appears to be a high mean hotspot, with mean decreasing as we get
# further away. We thus adjust for second order.


## Second order
poly_features <- poly(cbind(parana$coords), degree = 2, raw=TRUE)
quad_model <- lm(parana$data ~ poly_features)

quad_res <- parana$data - predict(quad_model, poly_features)
parana_res_quad <- parana
parana_res_quad$data <- quad_res
plot(parana_res_quad)
# Note the same can be achieved through the use of plot(parana, trend="2nd")
