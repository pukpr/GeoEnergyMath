using Flux, Plots, CSV, DataFrames, Distributions, Statistics, Printf, DSP;

function compare_forcing(w::Integer, correl::Bool)
  df1=CSV.read("nino4_1875_dlod.csv", DataFrame; delim='\t')
  N1 = nrow(df1);
  t1=Vector{Float32}(df1[1:N1,1]) # 1 Time
  m1=Vector{Float32}(df1[1:N1,4]) # 
  
  df2=CSV.read("n4_1875.csv", DataFrame; delim='\t')
  N2 = nrow(df2);
  t2=Vector{Float32}(df2[1:N2,1]) # 1 Time
  m2=Vector{Float32}(df2[1:N2,4]) # 

  if correl
    cc=cor(m1,m2)
    scatter(m1, m2, markersize=0.3, ylims=(-w,w), lw=0.5, label=@sprintf("Corr. Coeff. %1.6f", cc), xlabel="adjusted", ylabel="calibrated", title="comparison forcing")  
  else
    plot(t1, m1, w=0.5, label="calibrated to dLOD", xlabel="year", ylabel="amplitude", title="tidal forcing")  
    plot!(t2, m2, w=0.5, label="adjusted")  
  end
end