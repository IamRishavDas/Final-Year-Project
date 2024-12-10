GSE11882_expression_data <- read.csv("C:/Users/Risha/Desktop/GSE_11882/GSE11882_expression_data.csv")

GSE_11882 <- GSE11882_expression_data
head(GSE_11882)

min_max_normalize <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  } else {
    return(x) # Return non-numeric columns as is
  }
}


normalized_GSE_11882 <- as.data.frame(lapply(GSE_11882, min_max_normalize))
head(normalized_GSE_11882)

write.csv(normalized_GSE_11882, "C:/Users/Risha/Desktop/GSE_11882/Normalized_GSE11882_data.csv", row.names = FALSE)

class(normalized_GSE_11882[, 2])
typeof(normalized_GSE_11882[, 2])

nrow(normalized_GSE_11882)
ncol(normalized_GSE_11882)

head(normalized_GSE_11882[1, ])

# Determine the optimal number of clusters (optional)
# Elbow Method
wss <- numeric() # Within-cluster sum of squares
for (k in 1:10) {
  kmeans_model <- kmeans(normalized_GSE_11882[, 2: ncol(normalized_GSE_11882)], centers = k, nstart = 25)
  wss[k] <- kmeans_model$tot.withinss
}


plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)", ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k GSE11882")

n_data <- normalized_GSE_11882

set.seed(123) # For reproducibility

first_col <- n_data[, 1]
main_data <- data.frame(lapply(n_data[, -1], as.numeric))
main_data <- na.omit(main_data)

# k means for k - 2
km.out <- kmeans(main_data, centers = 2)
km2 <- km.out
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "C:/Users/Risha/Desktop/GSE_11882/cluster_centers11882_k2.csv", row.names = FALSE)

# k means for k - 3
km.out <- kmeans(main_data, centers = 3)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "C:/Users/Risha/Desktop/GSE_11882/Cluster_Centers11882_k3.csv", row.names = FALSE)

# k means for k - 4
km.out <- kmeans(main_data, centers = 4)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "C:/Users/Risha/Desktop/GSE_11882/Kmeans_Cluster_Results/Cluster_Centers11882_k4.csv", row.names = FALSE)


# k means for k - 5
km.out <- kmeans(main_data, centers = 5)
print(km.out)
cluster_centers <- km.out$centers
cluster_centers_df <- data.frame(cluster_centers)
cluster_centers_with_strings <- cbind(first_col[1:nrow(cluster_centers_df)], cluster_centers_df)
print(cluster_centers_with_strings)
write.csv(cluster_centers_with_strings, "C:/Users/Risha/Desktop/GSE_11882/Cluster_Centers11882_k5.csv", row.names = FALSE)


install.packages("mclust")
library(mclust)
ari <- adjustedRandIndex(normalized_GSE_11882[, 1], km2$cluster)
print(ari)




dist_matrix <- dist(normalized_GSE_11882[, 2:ncol(normalized_GSE_11882)])
silhouette_result <- silhouette(kmeans_result$cluster, dist_matrix)
summary(silhouette_result)
avg_silhouette_score <- mean(silhouette_result[, 3])
print(paste("Average Silhouette Score: ", avg_silhouette_score))




# Davies-Bouldin Index calculation
install.packages("cluster.stats")
library(cluster.stats)
db_index <- cluster.stats(dist_matrix, kmeans_result$cluster)$db
print(paste("Davies-Bouldin Index: ", db_index))



dbscan_result <- dbscan(normalized_GSE_11882, eps = 0.5, minPts = 5)