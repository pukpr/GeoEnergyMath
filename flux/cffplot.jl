using Plots
function cffplot(m,file_n::String, Show_Forcing::Bool=false, ts::Integer=0)
  df=CSV.read(file_n, DataFrame; delim='\t')
  N0 = nrow(df);
  t=Vector{Float32}(df[1:N0,1]) # 
  f2=Vector{Float32}(df[1:N0,2]) # 
  f1=Vector{Float32}(df[1:N0,4]) # 
  Year=t[ts];
  g = f1 .+ f2

  x_start, x_end, x_step = minimum(f1), maximum(f1), (maximum(f1) - minimum(f1))/100.0 
  y_start, y_end, y_step = minimum(f2), maximum(f2), (maximum(f2) - minimum(f2))/100.0 

  # Create the arrays of equally spaced data points for both dimensions
  x_points = collect(range(x_start, x_end, step=x_step))
  y_points = collect(range(y_start, y_end, step=y_step))

  # Define a 2D function
   f(x, y) = m([x,y,(Year-1900.0)/1000.0])[1]

  if Show_Forcing
    plot(t, f1, linewidth=0.5, title=file_n, xlabel="Year", ylabel="Forcing", label="F -, shift 6 mo")
    plot!(t, f2, linewidth=0.5, label="Forcing +")
    plot!(t, g, linewidth=0.5, linestyle=:dot, label="sum")
  else
    # Create a matrix of values using the 2D function
    z_values = [f(x, y) for y in y_points, x in x_points]

    # Generate a contour plot
    contour(x_points, y_points, z_values, aspect_ratio=1.0, fill=true, linewidth=0.01, title=file_n, xlabel="Forcing +", ylabel="Forcing -")

    # Add a line plot along x = -y
    #x_line = y_range
    #y_line = -y_range
    plot!(x_points, -x_points, linewidth=0.5, linestyle=:dash, color=:white, label="x = -y")
    plot!(x_points, -map(x -> x + 2.5, x_points), linestyle=:dot, linewidth=0.5, color=:white, label="+")
    plot!(x_points, -map(x -> x - 2.5, x_points), linestyle=:dot, linewidth=0.5, color=:white, label="-")
    scatter!([f1[ts]], [f2[ts]], label="year")
  end
  display(plot!())
end
