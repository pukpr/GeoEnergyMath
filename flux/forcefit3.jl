using Flux, Plots, CSV, DataFrames, Distributions, Statistics, Printf, DSP;

function forcefit3(n_loop, n_hidden, n_layers, N1, N::Integer, file_n::String, x::AbstractArray{Int,1}=[4,2,1], 
                   w::Integer=3, slope::Float64=1.0, Exclude::Bool=true, MA::Bool=true, Default::Bool=true)
           ## Parameterize training NN
  function create_repeated_dense_layers(n_in, n_hidden, n_out, n_layers, activation)
     layers = []
     push!(layers, Dense(n_in, n_hidden, activation))
     for _ in 2:n_layers-1
        push!(layers, Dense(n_hidden, n_hidden, activation))
     end
     push!(layers, Dense(n_hidden, n_out))
     return layers
  end
           ## Set up training data
  df=CSV.read(file_n, DataFrame; delim='\t')
  N0 = nrow(df);
  n_input = length(x)
  t=Vector{Float32}(df[1:N0,1]) # 1 Time
  m=Vector{Float32}(df[1:N0,2]) # 2 Model (LTE)
  d=Vector{Float32}(df[1:N0,3]) # 3 Data
  f=Matrix{Float32}(df[1:N0,x]) # 4 Forcing (LTE)
  for i in 1:N0
    n_input > 1 ? f[i,n_input] = slope*(f[i,n_input] - 1900)/1000.0 : nothing;
  end
           ## Set up training model
  tans(x) = -(x*2.0)*exp(-(2.0*x)^2/2)
  if Default
    model = Chain(create_repeated_dense_layers(n_input, n_hidden, 1, n_layers, tanh))
  else
    model = Chain(create_repeated_dense_layers(n_input, n_hidden, 1, n_layers, tans))
  end
  loss(input, output) = Flux.Losses.mse(model(input), output)
  if Exclude
     train_data = [(Matrix(vcat(f[1:N1,:], f[N:N0,:])'), Array(vcat(d[1:N1], d[N:N0])'))];
  else
     train_data = [(Matrix(f[N1:N,:]'), Array(d[N1:N]'))];
  end
  Default ? optimiser = Adam(0.001) : optimiser = Descent(0.1);

           ## Train model and periodically evaluate
  Index = 1.0;
  for Epoch in 1:n_loop
    Flux.train!(loss, Flux.params(model), train_data, optimiser)
    if Epoch % 1000 == 1
       for i in 1:N0
         MA ? m[i] = (model(f[i,:])[1] + (Index-1.0)*m[i])/Index : m[i] = model(f[i,:])[1]
       end
       Index = Index + 1.0
       cc = [cor(d[1:N1],m[1:N1]), cor(d[N1:N],m[N1:N]), cor(d[N:N0],m[N:N0])]
       ccs = @sprintf("%d %.3f %.3f %.3f", Epoch, cc[1], cc[2], cc[3] )
       title_str = @sprintf("%d hidden %d layers : #%s", n_hidden, n_layers, ccs)  
       plot(t, d, ylims=(-w,w), lw=0.5, label="Data " * file_n, xlabel="Year", ylabel="index value", title=title_str)  
       if Exclude
          plot!(t[1:N0], m[1:N0], lw=0.5, color=:green, label=@sprintf("Train on %d, weight[1,%.4f]", n_input, slope)) 
          plot!(t[N1:N], m[N1:N], lw=0.5, color=:red, label=@sprintf("Predicted >%.0f <%.0f", t[N1], t[N]))
       else  
          plot!(t[1:N0], m[1:N0], lw=0.5, color=:red, label=@sprintf("Predicted <%.0f >%.0f", t[N1], t[N]))
          plot!(t[N1:N], m[N1:N], lw=0.5, color=:green, label=@sprintf("Train on %d, weight[1,%.4f]", n_input, slope));
       end
       display(plot!()); println(cc)
    end
  end
  return model
end;
