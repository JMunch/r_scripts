histogramm = function(x, x_vec, nbins){
  n = length(x_vec)
  x0 = min(x_vec)
  h = (max(x_vec) - x0) / nbins
  fh = 0
  for(j in 1:nbins){
    for(i in 1:n){
      x_intv = ((x0 + (j - 1) * h) <= x) && ((x0 + j * h) > x)
      xi_intv = ((x0 + (j - 1) * h) <= x_vec[i]) && ((x0 + j * h) > x_vec[i])
      fh = fh + (x_intv * xi_intv)
    }
  
  }
  return(fh * (1 / n) * (1 / h))
}





histogramm_2 = function(x, x_vec, nbins){
  x0 = min(x_vec)
  h = (max(x_vec) - x0) / (nbins)
  n = length(x_vec)
  # Vector with interval limits of the bins
  bins = numeric(nbins + 1)
  bins[1] = x0
  for(j  in 2 : (nbins + 1)){
    bins[j] = x0 + j * h
  }
  bin_nr = findInterval(x_vec, bins)
  bin_count = table(bin_nr)
  fh = bin_count[findInterval(x, bins)]
  return(fh * (1 / n) * (1 / h))
}

x_norm = rnorm(100)

sapply(x_norm, histogramm,  x_vec = x_norm, nbins = 10) == sapply(x_norm, histogramm_2,  x_vec = x_norm, nbins = 10)


