# We compute and fit the variogram to a manually constructed example
library(geoR)

lin_dep <- function (x_dep, y_dep, n){
  x_coords <- matrix(rep(1:n, each = n), nrow = n, ncol = n)
  y_coords <- matrix(rep(1:n, times = n), nrow = n, ncol = n)
  
  return(x_dep * x_coords + y_dep * y_coords)
}

exp_peak <- function (peak_x, peak_y, sz, n){
  x_coords <- matrix(rep(1:n, each = n), nrow = n, ncol = n)
  y_coords <- matrix(rep(1:n, times = n), nrow = n, ncol = n)
  dist_mat <- sqrt((x_coords - matrix(peak_x, n, n))^2 
                   + (y_coords - matrix(peak_y, n, n))^2)
  peak_mat <- sz * exp(-dist_mat)
  print(peak_mat)
  return(peak_mat)
}

get_val_mat <- function (x, y, n = 10) {
  val_mat <- lin_dep(5, 0, n)
  val_mat <- val_mat + exp_peak(3, 2, 50, n) + exp_peak(7, 7, -25, n) + 
    exp_peak(7, 2, 50, n) + exp_peak(5, 5, -25, n)
  return(val_mat)
}

# Create geodata
combinations <- data.frame(expand.grid(1:10, 1:10))  # 10 by 10 grid  
val_mat <- get_val_mat()  # associated values
combinations$vals <- c(t(val_mat))
geor_data <- as.geodata(combinations, 
                        coords.col = c("Var1", "Var2"), data.col = "vals")

# Plots
plot(geor_data)  # as engineered, observe linear trend
plot(geor_data, trend = "1st")  # peaks we placed appear clearly

# Variograms
# Note unexpected behaviour above max.dist/2  ~= 6
plot(variog(geor_data, trend = "1st"))

our_variogram <- variog(geor_data, trend = "1st", max.dist = 6)  # Employ max
plot(our_variogram)  # Notice ceiling affect from around 3.5

# We may plot variograms in different directions to test for anisotropy
# Test a specific direction with chosen tolerance
# i.e. pairs of points are included only if their separation vector has
# appropriate direction, within tolerance range.
# Note R takes the angle from the vertical, clockwise
plot(variog(geor_data, trend = "1st", direction = 2*pi/4, tolerance = pi/8),
     max.dist = 6)

# Or use the following for 4 directional overview
plot(variog4(geor_data, trend = "1st", max.dist = 6))

# Note we seem to have lower variance at 90 degrees.
# This likely due to the fact we have positive peaks at equal y value,
# smoothing out horizontal spread. This kind of random effect should
# smooth out with larger data sets.

# Fit variogram (we already know "exponential" should provide best fit)
exp_fit_fix <- variofit(our_variogram, cov.model = "exp", fix.nugget = F)
print(summary(exp_fit_fix)$sum.of.squares)  # sum of squares

# Plot our fit
plot(our_variogram, pch = 16)
lines(exp_fit_fix, col = "green", lwd = 4, lty = "solid")

# (Nice fit due to engineered data)
