using Plots
function cfplot(m)
  # Define your starting and ending points, and step size for both dimensions
  x_start, x_end, x_step = -2.5, 0.5, 0.01     # Forcing  -2.5, 0.6, 0.01
  #y_start, y_end, y_step = -0.5, 1.5, 0.01     # Time -0.3, 1.2, 0.01
  y_start, y_end, y_step = 1870.0, 2020.0, 1.0     # Time -0.3, 1.2, 0.01

  # Create the arrays of equally spaced data points for both dimensions
  x_points = collect(range(x_start, x_end, step=x_step))
  y_points = collect(range(y_start, y_end, step=y_step))

  # Define a 2D function
  f(x, y) = m([x,(y-1900.0)/1000.0])[1]

  # Create a matrix of values using the 2D function
  z_values = [f(x, y) for y in y_points, x in x_points]

  # Generate a contour plot
#  contour(y_points, x_points, transpose(z_values), fill=true, linewidth=0.1, color=:turbo, xlabel="Year", ylabel="Forcing Level")
  contour(y_points, x_points, transpose(z_values), fill=true, linewidth=0.01, xlabel="Year", ylabel="Forcing Level")
end