install.packages("hexbin")
install.packages("RColorBrewer")
install.packages("QRM")
library(MASS)
library(stats)
library(hexbin)
library(RColorBrewer)
library(QRM)

# Simulating multivariat standard distribution
ro = 0.6
sigma = matrix(c(1, ro, ro, 1), nrow = 2, byrow = TRUE)
sigma
sim_len = 5000
sim_mat= matrix(data = NA, ncol = 2, nrow = sim_len)
for(i in 1:sim_len){
sim_mat[i, 1:2] = mvrnorm(1, mu = c(0,0), Sigma = sigma )
}

mvrnorm(1, mu = c(0,0), Sigma = sigma )
# Plots

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

hex = hexbin(sim_mat, xbins = 10)
plot(hex, colramp = rf)


k = kde2d(sim_mat[,1], sim_mat[,2], n = 100)
image(k, col=r)

# copula
gum = rcopula.gumbel(10000, theta = 3, 2)
k = kde2d(gum[,1], gum[,2], n = 100)
image(k, col=r)

gau = rcopula.gauss(10, Sigma = sigma)
k = kde2d(gau[,1], gau[,2], n = 100)
image(k, col=r)

tcop = rcopula.t(10000, 10, Sigma = sigma)
k = kde2d(tcop[,1], tcop[,2], n = 100)
image(k, col=r)

