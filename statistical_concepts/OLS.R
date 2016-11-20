compute_ols = function(y, x, constant = TRUE){
  
  y = as.vector(y)
  x = as.matrix(x)
  
  dimension = dim(x)
  if(constant){
    x = cbind(rep(1, dimension[1]), x)
  }
  dimension = dim(x)
  
  beta = solve(t(x) %*% x) %*% t(x) %*% y
  
  return(beta)
  
}

a = compute_ols(depend, independ)
a