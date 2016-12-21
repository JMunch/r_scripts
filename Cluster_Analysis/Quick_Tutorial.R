

# Load, Clean and Scale Data ----------------------------------------------


# Load data 
data("USArrests")
data = USArrests

# check for Nas
table(is.na(USArrests))

# Remove any missing values
data = na.omit(USArrests)

# scale data
scaled_data = scale(data)

# Having a closer look at the scaled data
summary(data)
summary(scaled_data)
var(data)
var(scaled_data)
cor(data)
cor(scaled_data)


# Installing and Loading Required Packages --------------------------------


# Install factoextra
install.packages("factoextra")

# Install cluster packages
install.packages("cluster")

# Load packages
library(factoextra)
library(cluster)


# Clatifying Distance Measures --------------------------------------------


res_dist = get_dist(scaled_data, stand = TRUE, method = "pearson")
fviz_dist(res_dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


# Partitioning Clustering -------------------------------------------------

# K-Means -----------------------------------------------------------------


# Finding optimal number of clusters
fviz_nbclust(scaled_data, FUN = kmeans, method = "gap_stat")

# Using kmeans to to cluster data
km_res = kmeans(scaled_data, centers = 4, nstart = 25)

# Visualize
km_clus = fviz_cluster(km_res, data = scaled_data, frame.type = "convex", title = "K-Means")


# PAM-Clustering ----------------------------------------------------------


library("cluster")
pam_res = pam(scaled_data, 4)

# Visualize
pam_clus = fviz_cluster(pam_res, title = "PAM")


# Compare K-Means and PAM Clustering --------------------------------------


library(gridExtra)
grid.arrange(km_clus, pam_clus, ncol = 2)



