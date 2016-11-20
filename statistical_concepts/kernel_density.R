gausian_kernel = function(u){
  (1 / sqrt(2 * pi)) * exp(- (u^2 / 2))
}

kernel_density = function(x, x_vec, h){
  n = length(x_vec)
  
  if(h == 0){
    h = 1.06 * sqrt((1 / n) * sum((x_vec - mean(x_vec)) ^2)) * n ^ -(1 / 5)
  }
  
  sum(gausian_kernel((x - x_vec) / h)) * (1 / n) * (1 / h)
}

x_norm = rnorm(50000) 
seq_vec = seq(-3, 3, 0.02)



f_h = sapply(seq_vec, kernel_density,  x_vec = x_norm, h = 0)
plot(seq_vec, f_h, type = "l")
