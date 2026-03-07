#OPTICS Algorithm

optics <- function(data, epsilon, min_Pts) {
  x <- as.matrix(data)    # data will be created by generateClusterTestDataSimple2D() or similiar function
  dist_x <- as.matrix(stats::dist(x)) # die Distanzmatrix von data, verbessern wir möglicherweise noch
  data_entries <- nrow(x) # number of points
  
  processed <- rep(FALSE, data_entries)
  reachability <- rep(NA, data_entries)
  core_distance <- rep(NA, data_entries)
  ordered <- integer(0)
  predecessor <- rep(NA_integer_, data_entries)
  
  getNeighbors <- function(point) {
    which(dist_x[point, ] <= epsilon) # Liste an Punkten, die innerhalb von Distanz epsilon um Punkt point liegen, point inklusive
  }
  
  coreDistance <- function(point, neighbors) {
    if(length(neighbors) < min_Pts) {
      return(NA)
    }
    dists <- dist_x[point, neighbors]
    return(dists[order(dists)][min_Pts])
  }
  
  update <- function(neighbors, point, seeds_points, seeds_reach) {
    coredist <- core_distance[point]
    for (o in neighbors) { # for each o in N
      if(processed[o] == FALSE) { # if (o = unprocessed)
        new_reach <- max(coredist, dist_x[point, o]) # new-reach-dist = max(coredist, dist(p, o))
        if(is.na(reachability[o])) { # if (o.reachability-distance = NULL)
          # o not in Seeds:
          reachability[o] <<- new_reach #o.reachability-distance = new-reach-dist
          predecessor[o] <<- point # for spanning tree (see wiki article)
          seeds_points <- c(seeds_points, o) # Seeds.insert(o, new-reach-dist)
          seeds_reach <- c(seeds_reach, new_reach)
        } else if(new_reach < reachability[o]) { #else: o in Seeds, check improvement// if (new-reach-dist < o.reachability-distance)
          reachability[o] <<- new_reach # o.reachability-distance = new-reach-dist
          predecessor[o] <<- point
          # Seeds.move-up(o, new-reach-dist)
          i <- which(seeds_points == o)
          if(length(i) > 0) {
            seeds_reach[i] <- new_reach
          }
        }
      }
    }
    return(structure(
      list(
        order = ordered,
        reachdist = reachability,
        coredist = core_distance,
        predecessor = predecessor
      ),
      class = "optics"
    ))
  }
  
  for(point in seq_len(data_entries)) {  # for each point in data
    # point.reachability-distance = NULL
    # we can save one more for loop? maybe? 
    if(processed[point] == FALSE) {   # for each unprocessed point in data
      neighbors <- getNeighbors(point) # N <- getNeighbors(point, epsilon)
      processed[point] <- TRUE  # mark point as processed
      ordered <- c(ordered, point)     # place point in ordered list
      core_distance[point] <- coreDistance(point, neighbors)
  
      if(!is.na(core_distance[point])) {    # if (core-distance(point, epsilon, min_Pts) != NULL) 
        # define Seeds, as "priority queue" doesn't exist in base R
        seeds_points <- integer(0)
        seeds_reach <- numeric(0)
        
        seeds <- update(neighbors, point, seeds_points, seeds_reach) # update(N, point, Seeds, epsilon, min_Pts)
        seeds_points <- seeds$points
        seeds_reach <- seeds$reach
        
        while (length(seeds_points) > 0) { # for each next q in Seeds
          i <- which.min(seeds_reach)
          q <- seeds_points[i]
          
          seeds_points <- seeds_points[-i]
          seeds_reach <- seeds_reach[-i]
          
          if(processed[q] == FALSE) {
            neighbors_q <- getNeighbors(q) # N' = getNeighbors(q, epsilon)
            processed[q] <- TRUE # mark q as processed
            ordered <- c(ordered, q) # place q in ordered list
            core_distance[q] <- coreDistance(q, neighbors_q)
            
            if(!is.na(core_distance[q])) { # if (core-distance(q, epsilon, min_Pts) != NULL)
              seeds <- update(neighbors_q, q, seeds_points, seeds_reach) # update(N', q, Seeds, epsilon, min_Pts)# update(N', q, Seeds, epsilon, min_Pts)              
              seeds_points <- seeds$points
              seeds_reach <- seeds$reach
            }
          }
        }
      }
    }
  }
  return(structure(
    list(
      order = ordered,
      reachdist = reachability,
      coredist = core_distance
    ),
    class = "optics"
  ))
}

# let's try!
#data <- generateClusterTestDataSimple2D(n = 100)
#result <- optics(data, epsilon = 0.1, min_Pts = 5)

# Reihenfolge als Rang speichern
#order_rank <- integer(nrow(data))
#order_rank[result$order] <- seq_along(result$order)
#data$order_rank <- order_rank

# Visualisierung: Punkte eingefärbt nach OPTICS-Reihenfolge
#ggplot2::ggplot(data, ggplot2::aes(X, Y, color = order_rank)) +
#  ggplot2::geom_point() +
#  ggplot2::scale_color_viridis_c()


data <- generateClusterTestDataSimple2D(n = 200, nclusters = 3)
result <- optics(data, epsilon = 0.1, min_Pts = 10)
order_rank <- integer(nrow(data))
order_rank[result$order] <- seq_along(result$order)
data$order_rank <- order_rank
cluster <- extractClusters(result, eps = 0.15)
table(cluster)
cols <- cluster_colors(cluster)

ggplot2::ggplot(data, aes(X, Y)) +
  geom_point(color = cols)

plot(result, eps = 0.1)

###
path1 <- function(t) tibble::tibble(X = 0.2, Y = 0.3)
path2 <- function(t) tibble::tibble(X = 0.5, Y = 0.8)
path3 <- function(t) tibble::tibble(X = 0.8, Y = 0.4)
cluster_paths <- list(path1, path2, path3)
data <- generateClusterTestData2DFromPaths(n = 450, list_of_paths = cluster_paths)
result <- optics(data, epsilon = 0.1, min_Pts = 10)
order_rank <- integer(nrow(data))
order_rank[result$order] <- seq_along(result$order)
data$order_rank <- order_rank
cluster <- extractClusters(result, eps = 0.15)
table(cluster)
cols <- cluster_colors(cluster)

ggplot2::ggplot(data, aes(X, Y)) +
  geom_point(color = cols)

plot(result, eps = 0.1)