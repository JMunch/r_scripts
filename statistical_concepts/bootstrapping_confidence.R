
m = 200
S = seq(100,m,1)
mat_dif = matrix(, nrow = length(S), ncol = 2)


# Real confidence inteval
confid_norm = qnorm(limits)


j = 0
for(n in S){
j = j + 1
X = rnorm(n) # standardnormaldistributed sequence
alpha = 0.05
limits = c(alpha / 2, 1 - alpha / 2)

# Confidintervals from quantiles
confid_qua = quantile(X, limits)

# Confidence interval from bootstrapping
resamps = 1000 
confid_res = matrix(, nrow = resamps, ncol = 2)
for(i in 1:resamps){
  X_res = sample(X, n, n)
  confid_res[i, 1] = quantile(X_res, limits[1])
  confid_res[i, 2] = quantile(X_res, limits[2])
}
confid_boots = apply(confid_res, MARGIN = 2, FUN = mean)

mat_dif[j, 1] = confid_norm[1] - confid_boots[1]
mat_dif[j, 2]  = confid_norm[1] - confid_qua[1]

}

plot(S, mat_dif[,1], type = "l", col = "blue")
lines(S, mat_dif[,2], type = "l", col = "yellow")
mat_dif

mat_dif[,1]
confid_norm[1] - confid_boots[1]

mean(mat_dif[,1])
mean(mat_dif[,2])
