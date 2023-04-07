using Flux, Plots, CSV, DataFrames, Distributions;

function forcefit(Iterations::Integer)

  function create_repeated_dense_layers(input_size, hidden_size, output_size, num_layers, activation)
     layers = []
     push!(layers, Dense(input_size, hidden_size, activation))
     for _ in 2:num_layers-1
        push!(layers, Dense(hidden_size, hidden_size, activation))
     end
     push!(layers, Dense(hidden_size, output_size))
     return layers
  end

  input_size = 1
  hidden_size = 12
  output_size = 1
  num_layers = 12
  activation = tanh

  df=CSV.read("n4all.csv", DataFrame; delim='\t')

  N0 = nrow(df);
  N = 800;
  t=Vector{Float32}(df[1:N0,1]) # 1 Time
  m=Vector{Float32}(df[1:N0,2]) # 2 Model (LTE)
  d=Vector{Float32}(df[1:N0,3]) # 3 Data
  f=Vector{Float32}(df[1:N0,4]) # 4 Forcing (LTE)
  w=d[1:N]   # deepcopy(d)  # Window for training

  display(plot(t, d, ylims=(-5,5), label="True"))  # seriestype=:scatter

  model = Chain(create_repeated_dense_layers(input_size, hidden_size, output_size, num_layers, activation))
  loss(input, output) = Flux.Losses.mse(model(input), output)
  optimiser = Descent(0.1);
  train_data = [(Array(f[1:N]'), Array(w'))];

  for Epochs in 1:Iterations
    Flux.train!(loss, Flux.params(model), train_data, optimiser)
  end

  y = deepcopy(d);
  for i in 1:N0
    y[i] = model([f[i]])[1];
  end

  display(plot!(t[1:N],  y[1:N],  label="Trained"))  
  display(plot!(t[N:N0], y[N:N0], label="Predicted")) 
  (cor(w,y[1:N]), cor(d[N:N0],y[N:N0]))

end
