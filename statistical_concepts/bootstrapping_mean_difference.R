# Der Vergleich zweier Durchschnitte mithilfe von Bootstrapping

n = 500
resamps = 10000
  
A = rnorm(n, mean = 0, sd = 1)
B = rnorm(n, mean = 1, sd = 1)

#A = rchisq(n, 5)
#B = rchisq(n, 10)

# Empty matrix for the mean differences
meandiffs = matrix(, nrow = resamps, ncol = 1)

# Resample and calculate the mean difference
for(i in 1:resamps){
  A_res = sample(A, n, n)
  B_res = sample(B, n, n)
  meandiffs[i] = mean(A_res) - mean(B_res)
}

# 0.95 Confidence Interval
confi_bo = quantile(meandiffs, c(0.025, 0.975))

ttest = t.test(A, B)
confi_tt = ttest$conf.int


confi_bo
confi_tt


hist(meandiffs, breaks = 80)
plot(density(meandiffs, kernel = "gaussian"), col = "blue")

abline(v = confi_bo, col = "blue")
abline(v = confi_tt, col = "red")

