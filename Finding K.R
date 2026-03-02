library(tidyverse)
library(ggthemes)
library(reshape2)
library(skimr)
library(ggcorrplot)
library(cluster)

phone_users <- read.csv("test_scores.csv")
phone_users

#Training Model
#Z - Score standardization
personality <- phone_users[1:6]
personality_z <- as.data.frame(lapply(personality, scale)) #standardized version

summary(personality$pretest_grade)

#Find the K-means value
library(factoextra)
library(NbClust)

#Elbow Method
fviz_nbclust(personality_z, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 1) +
  labs(subtitle = "Elbow method")
#-------------------------------------------------------------------------------
#Hierarchical Clustering
d <- dist(personality_z, method = "euclidean")
fit1 <- hclust(d, method="ward.D2")
plot(fit1, main = "Hierarchical Clustering", sub = "Values")
#Selecting from in line with height 20
group1 <- cutree(fit1, k=3)
rect.hclust(fit1, k=3, border="red")
#Final Plot
fviz_dend(fit1, k = 4,
          cex = 0.5,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "red"),
          color_labels_by_k = TRUE,
          rect.hclust = F,
          main = "Hierarchical Clustering")
#-------------------------------------------------------------------------------
#Silhouette Optimal Number of Clusters
fviz_nbclust(personality_z, kmeans, method = "silhouette") +
  labs(title = "Silhouette Method", subtitle = "Optimal number of clusters")
