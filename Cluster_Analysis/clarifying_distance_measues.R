
# Data Preparation --------------------------------------------------------


data("USArrests")
data = USArrests

# Subset of the data
set.seed(123)
ss = sample(1:(nrow(data)), 10) # Pick ten random rows
sample = USArrests[ss, ]

table(is.na(sample)) # No NA in the data


# Descriptive  Statistics -------------------------------------------------


desc_stats = data.frame(
  Min = apply(data, 2, min), #minimum
  Max = apply(data, 2, max), #maximum
  Mean = apply(data, 2, mean), #mean
  Median = apply(data, 2, median), #median
  SD = apply(data, 2, sd) #standard deviation
)

desc_stats


# Scaling the data --------------------------------------------------------

data_scaled = scale(data)
head(round(data_scaled, 2))


# Computing Distances -----------------------------------------------------

# Comput euclidean pairwise distance
dist_eucl = dist(data_scaled, method = "euclidean")
mat_eucl = round(as.matrix(dist_eucl), 2)


# Correlation Base Distance Measures --------------------------------------


# Compute correlation matrix
res_cor = cor(t(data_scaled), method = "pearson")
dist_cor = as.dist(1 - res_cor)
round(as.matrix(dist_cor)[1:6, 1:6], 1)


# The function daisy() ----------------------------------------------------

daisy_mat = as.matrix(daisy(data_scaled, metric = c("euclidean", "manhattan", "gower"),
                            stand = FALSE))
daisy_mat

# daisy() example with mixed type data
data("flower")
head(flower)
str(flower)

dd = as.matrix(daisy(flower))
head(round(dd[,1:6], 2))


# Viszalizing Distance Matrices -------------------------------------------


#install.packages("corrplot")
library(corrplot)

# Eucldean distance
corrplot(as.matrix(dist_eucl)[1:6, 1:6],
         is.corr = FALSE, method = "color")

# Visualize only the upper triangle
corrplot(as.matrix(dist_eucl)[1:6, 1:6], is.corr = FALSE,
         method =  "number", order = "hclust", type = "upper")

