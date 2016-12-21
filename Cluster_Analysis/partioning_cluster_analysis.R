
# Installing and Loading Required Packages --------------------------------


library(cluster)
library(factoextra)


# K-Means -----------------------------------------------------------------


set.seed(123)

# Simuklate some data wiith two clusters
df = rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1,  sd = 0.3), ncol = 2))

# Compute kmeans with k = 2
set.seed(123)
km.res = kmeans(df, 2, nstart = 25)

# Cluster number of each observation
km.res$cluster

# Size of each cluster
km.res$size

# Centroids
km.res$centers

# Plot the data points with coloring
plot(df, col = km.res$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 2")
points(km.res$centers, col = 1:2, pch = 8, cex = 3)

# Compute kmeans with k = 4
set.seet(123)
km.res = kmeans(df, 4, nstart = 25)

plot(df, col = km.res$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 4")
points(km.res$centers, col = 1:4, pch = 8, cex = 3)


# K-Means on real data ----------------------------------------------------


# Load the data set
data("USArrests")

# Remove any missing value (i.e. NA values for not available)
# That might be present in the data
df = na.omit(USArrests)
head(df, n = 5)

# First some discreptive analysis
desc_stats = data.frame(
  Min = apply(df, 2, min), # minimum
  Med = apply(df, 2, median), # median
  Mean = apply(df, 2, mean), # mean
  SD = apply(df, 2, sd), # standard deviation
  Max = apply(df, 2, max) # Maximum
  )
round(desc_stats, 2)

# scale the variables
df_scaled = scale(df)
head(df)

# Determine number of clusters
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Compute k-means clustering with k = 4
set.seed(123)
km.res = kmeans(df_scaled, 4, nstart = 25)


aggregate(USArrests, 
          by = list(km.res$cluster),
          mean)

# Plot the result
fviz_cluster(km.res, data = df_scaled)


# PAM clustering ----------------------------------------------------------


pam.res = pam(df_scaled, 4)
pam.res$medoids

head(pam.res$cluster)

# plot with cluster package
clusplot(pam.res, main = "Cluster plot k = 4",
         color = TRUE)

# plot with factoextra
fviz_cluster(pam.res)

# plot Silhouette
plot(silhouette(pam.res), col = 2:5)
fviz_silhouette(silhouette(pam.res)) 

# Check which observations have negative silhouette
sil = silhouette(pam.res)
neg_sil_index = which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]


# CLARA: Clustering Large Applications ------------------------------------


set.seed(1234)
# Generate 500 objects, divided into 2 clusters
x = rbind(cbind(rnorm(200, 0, 8), rnorm(200, 0, 8)),
          cbind(rnorm(300, 50, 8), rnorm(300, 50, 8)))

# Compute clara
clarax = clara(x, 2 , sample = 50)


# Cluster plot
fviz_cluster(clarax, stand = FALSE, geom = "point",
             pointsize = 1)

# Silhouette plot
plot(silhouette(clarax), col = 2:3, main = "Silhoette plot")

# Medoids
clarax$medoids






