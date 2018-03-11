library(TSP)
library(stringi)

location <- maml.mapInputPort(1)

# Compute locations in polar coordinates (to work better for euclidean distances)
location$LONG_RAD <- location$LONGITUDE * (2 * pi)/360
location$LAT_RAD <- (location$LATITUDE * 2) * (2 * pi)/360
R <- (6378 + 6356)/2
location$X = R * cos(location$LAT_RAD) * cos(location$LONG_RAD)
location$Y = R * cos(location$LAT_RAD) * sin(location$LONG_RAD)
location$Z = R * sin(location$LAT_RAD)

# Group cusomer addresses into n clusters, where n is the number of representative addresses
n_cluster <- length(location$TYPE[location$TYPE == "VENDOR"])
group1 <- kmeans(x=location[location$TYPE=="CUSTOMER", colnames(location) %in% c("X", "Y", "Z")], centers=n_cluster)


closest_cluster <- function(x, c) {
  cluster_dist <- apply(c, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster_dist)[1])
}

# Assign each representative to its closest customer cluster
# Not an optimal solution, but it is a good approximation
group2 <- NULL
centers <- group1$centers
addresses <- location[location$TYPE=="VENDOR", colnames(location) %in% c("X", "Y", "Z")]
for(i in 1:(nrow(addresses)-1)) {
  address <- addresses[i, ]
  closest <- closest_cluster(address, centers[, colnames(centers) %in% c("X", "Y", "Z")])
  group2 <- c(group2, as.integer(names(closest)))
  centers <- centers[-closest,]
}
group2 <- c(group2, setdiff(1:nrow(group1$centers), group2))

location$GROUP <- c(group1$cluster, group2)
location$ID <- paste0("C", 1:nrow(location))
location$ID[location$TYPE=="VENDOR"] <- paste0("R", 1:n_cluster)

maml.mapOutputPort("location")
