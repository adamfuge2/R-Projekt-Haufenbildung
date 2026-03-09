# DBSCAN Algorithm
# Was dieser Code am Ende können soll:
# (gefühlt ist alles was man findet also werden Begriffe sehr inkonsistent deutsch und englisch gemixt bis ich lust habe das zu vereinheitlichen <3)
# 1.: Dichte abschätzen / estimate density
#   um jeden Datenpunkt wird die Anzahl der Punkte innerhalb eines angegebenen Umfelds
#   (Variable "epsilon") gezählt und die drei Arten an Punkten (Kernobjekte, Dichte-erreichbare
#   Objekte, Rauschpunkte) mit einer angegebenen Schwelle "minPts" als minimale Anzahl an
#   Dichte-erreichbare Objekte (inkl. Datenpunkt) ermittelt
# 2.: Kernpunkte zu einem Cluster zusammenfügen, wenn sie "density-reachable" sind,
#    also über Dichte-erreichbare Objekte verbunden werden können
# 3.: Dichte-erreichbare Punkte Clustern zuweisen mit Parametern epsilon und minPts

dbscan <- function(data, epsilon, min_Pts) {

 stopifnot("data must have at least one row" = nrow(data) >= 1)
 stopifnot("data must have at least one columns" = ncol(data) >= 1)
 stopifnot("data must contain only numeric values" = #we can discuss if we want that check
             all(tapply(data, is.numeric, logical(1)))) # Sonderfall einbauen; wenn alles numeric machen wir den so, sonst dissimillarity-matrix draufwerfen
 stopifnot("epsilon must be positive and numeric" = is.numeric(epsilon) && epsilon > 0)
 stopifnot("min_Pts must be a positive integer" = is.wholenumber(min_Pts) && min_Pts > 0)
 stopifnot("data must be data.frame or tibble" = is.data.frame(data))

 x <- as.matrix(data)    # data will be created by generateClusterTestDataSimple2D() or similiar function
 dist_x <- as.matrix(stats::dist(x)) # die Distanzmatrix von data
 n <- nrow(x) # number of data points
 cluster_id = 0L
 visited <- rep(FALSE, n)
 cluster_labels <- rep(0L, n)

 regionQuery <- function(point) {
   which(dist_x[point, ] <= epsilon) # Liste an Punkten, die innerhalb von Distanz epsilon um Punkt point liegen, point inklusive
 }

 expandCluster <- function(point, area, cluster_id) {
   cluster_labels[point] <<- cluster_id # must be super-assigned, or vanished after function call
   # add point to cluster / assign cluster to point
   i <- 1
   while(i <= length(area)) { # for each element p in region:
     pt <- area[i]
     if (visited[pt] == FALSE) { # if p not visited:
       visited[pt] <<- TRUE # mark p as visited
       new_area <- regionQuery(pt) # new_region = data.regionQuery(p, eps)

       if(length(new_area) >= min_Pts) { # if sizeof(new_region) >= min_Pts
         area <- unique(c(area, new_area)) # remove duplicate, points exist only once in cluster; region = region joined with new_region
       }
     }
     if(cluster_labels[pt] == 0L) { # if p not in any cluster
       cluster_labels[pt] <<- cluster_id # unmark p as noise
     }
     i <- i + 1
   }
 }


 # for each element in data (point in n)
 for(point in 1:n)
  if(visited[point] == FALSE) {
    visited[point] <- TRUE    # mark point as visited
    area <- regionQuery(point) # get neighbors
    if(length(area) < min_Pts) {
      cluster_labels[point] <- 0L # mark i as noise
    }
    else {
      cluster_id <- cluster_id + 1
      expandCluster(point, area, cluster_id)
    }
  }

 clustered_data <- data
 clustered_data$cluster <- cluster_labels

 clustering_function <- function(point) {
   point <- as.numeric(point)
   d <- sqrt(rowSums((x - matrix(point, nrow(x), ncol(x), byrow = TRUE))^2))
   neighbors <- which(d <= epsilon)
   if(length(neighbors) < min_Pts){
     return(0L)
   }
   cluster_labels[neighbors[1]]
 }

 return(structure(
   list(
     clustered_data = clustered_data,
     clustering_function = clustering_function
   ),
   description = 'Data clustered by DBSCAN algorithm',
   class= 'clustering'
 ))
}


### let's try it out (to be removed before merging in main)
data <- generateClusterTestDataSimple2D(n=300)
result <- dbscan(data, epsilon=0.2, min_Pts=3)
viewClusters(result$clustered_data)

data$cluster <- factor(
  result$clustered_data,
  levels = sort(unique(result$clustered_data)),
  labels = paste("Cluster", sort(unique(result$clustered_data)))
)
ggplot2::ggplot(data, ggplot2::aes(X, Y, color = cluster)) +
  ggplot2::geom_point() +
  ggplot2::scale_color_manual(
    values = c(
      "Cluster 0" = "black",
      setNames(
        grDevices::rainbow(length(levels(data$cluster)) - 1),
        levels(data$cluster)[-1]
      )
    )
  )
