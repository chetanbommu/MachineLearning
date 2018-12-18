##########################################
######## Example on Clustering ###########
##########################################

## Good Read : https://trainings.analyticsvidhya.com/courses/course-v1:AnalyticsVidhya+Python-Final-Jan-Feb+Python-Session-1/courseware/73167b5cca8447dfa535a80d3961dc61/821ac77ab0104820bfa494cb55b237d9/?activate_block_id=block-v1%3AAnalyticsVidhya%2BPython-Final-Jan-Feb%2BPython-Session-1%2Btype%40sequential%2Bblock%40821ac77ab0104820bfa494cb55b237d9

## Types of Clustering
  # a. Hard Clustering : Each data point either belongs to a cluster completely or not.
  # b. Soft Clustering : Instead of putting each data point into a separate cluster, a probability or likelihood of that data point to be in those clusters is assigned.

## Types of Clustering Algorithms 
  # a. Connectivity Models. Ex : Heirarchical Clustering 
  # b. Centroid Models(Iterative clustering algorithms). Ex : K-Means Clustering
  # c. Distribution Models. Ex: Normal, Gaussian Clustering
  # d. Density Models. Ex: DBSCAN, OPTICS.

##########################################
########### K-Means Clustering ###########
##########################################

## dataset used : universalBank.csv

##########################################
################# NOTES ##################
##########################################
# K means is an iterative clustering algorithm that aims to find local maxima in each iteration.This algorithm works in these 5 steps :
# 1. Specify the desired number of clusters K
# 2. Randomly assign each data point to a cluster
# 3. Compute cluster centroids
# 4. Re-assign each point to the closest cluster centroid 
# 5. Re-compute cluster centroids
# Now Repeat step-4 & step-5 until no improvements are possible i.e., until we’ll reach global optima


##########################################
######## Hierarchical Clustering #########
##########################################

##########################################
################# NOTES ##################
##########################################
# Hierarchical Clustering is an algorithm that builds hierarchy of clusters.
# The results of hierarchical clustering is shown using Dendrogram.
# The height in the dendrogram at which two clusters are merged represents the distance between two clusters in the data space.
# The decision of the no. of clusters that can best depict different groups can be chosen by observing the dendrogram. 
  # i.e., The best choice of the no. of clusters is the no. of vertical lines in the dendrogram cut by a horizontal line that can transverse the maximum distance vertically without intersecting a cluster.
# The decision of merging two clusters is taken on the basis of closeness of these clusters. There are multiple metrics for deciding the closeness of two clusters :
  # Euclidean distance: ||a-b||2 = √(Σ(ai-bi))
  # Squared Euclidean distance: ||a-b||22 = Σ((ai-bi)2)
  # Manhattan distance: ||a-b||1 = Σ|ai-bi|
  # Maximum distance:||a-b||INFINITY = maxi|ai-bi|
  # Mahalanobis distance: √((a-b)T S-1 (-b))   {where, s : covariance matrix}

############################################################
## Difference between K Means and Hierarchical clustering ##
############################################################
# 1. Hierarchical clustering can’t handle big data well but K Means clustering can. This is because the time complexity of K Means is linear i.e. O(n) while that of hierarchical clustering is quadratic i.e. O(n2).
# 2. In K Means clustering, since we start with random choice of clusters, the results produced by running the algorithm multiple times might differ. While results are reproducible in Hierarchical clustering.
# 3. K Means is found to work well when the shape of the clusters is hyper spherical (like circle in 2D, sphere in 3D).
# 4. K Means clustering requires prior knowledge of K i.e. no. of clusters you want to divide your data into. But, you can stop at whatever number of clusters you find appropriate in hierarchical clustering by interpreting the dendrogram

############################################################
################ Applications of Clustering ################
############################################################
# Recommendation engines
# Market segmentation
# Social network analysis
# Search result grouping
# Medical imaging
# Image segmentation
# Anomaly detection

##########################################
######## K-Means Clustering Code #########
##########################################

## load data
k_means_dataset = read.csv('Clustering/universalBank.csv')
## remove unnecessary columns
k_means_dataset$Personal.Loan = NULL
k_means_dataset$ZIP.Code = NULL
k_means_dataset$ID = NULL
## check summary
summary(k_means_dataset)
## correlation
library(corrplot)
corrplot(cor(k_means_dataset), method = 'number')

k_means_dataset$Experience = NULL ## Age & Experience are very highly correlated.

## Scaling
# function for min-max scaling
min_max_scaling_function = function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
# scale
for(i in 1:ncol(k_means_dataset)) {
  k_means_dataset[,i] = min_max_scaling_function(k_means_dataset[,i])
}

## Build model
# Find the ideal number of clusters using Elbow plot
withinByBetween = c()
for(i in 2:15){
  k_means_model = kmeans(k_means_dataset,i)
  withinByBetween = c(withinByBetween, mean(k_means_model$withinss) / k_means_model$betweenss)
}
# Elbow Plot
plot(2:15,withinByBetween, type='l')  ## type =l => represents line curve
  # 7 or 8 seems to be ideal number of clusters, Rule of Thumb: Always choose minimum.
k_means_model = kmeans(k_means_dataset,7)
## check the summary of model
k_means_model

k_means_model$centers ## shows centers for each variable of each cluster
k_means_dataset$cluster_number = k_mean_model$cluster ## Assigning cluster number to original datset

# withinss:
  # It is the within cluster sum of squares. So it results in a vector with a number for each cluster.
  # One expects, this ratio, to be as lower as possible for each cluster, since we would like to have homogeneity within the clusters.
k_means_model$withinss
# Total Sum of withinss i.e, sum(k_means_model$withinss)
k_means_model$tot.withinss

# betweenss : 
  # It is the between clusters sum of squares. In fact it is the mean of distances between cluster centers. 
  # One expects, this ratio, to be as higher as possible, since we would like to have heterogenous clusters.
k_means_model$betweenss

# Total Sum of squares (totss) = tot.withinss + betweenss
k_means_model$totss

###############################################
######## Hierarchical Clustering Code #########
###############################################

# dataset used : mtcars

## load data
data("mtcars")
mtcars # showss data

## check summary
summary(mtcars)

## Scaling
for(i in 1:ncol(mtcars)){
  mtcars[,i] = min_max_scaling_function(mtcars[,i])
}
# verify scaled
summary(mtcars)

## Calculate Distance Matrix
distance_matrix = dist(as.matrix(mtcars),method = 'euclidean') 
  # maximum, manhattan, canberra, binary & minkowski can also be used.

View(as.matrix(distance_matrix))

## Build Heirachical Clustering model
hierachical_model = hclust(distance_matrix) # hclust takes a dissimilarity structure as produced by dist.

## Plotting Cluster Dendogram
plot(hierachical_model)

## Cut the dendogram into several groups by specifying the desired number(s) of groups or the cut height(s)
cluster_groups = cutree(hierachical_model, h = 1.5)
table(cluster_groups)

## Assign cluster number to original dataset
mtcars$cluster_number = cluster_groups
