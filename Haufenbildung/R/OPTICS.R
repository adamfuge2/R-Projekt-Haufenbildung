#OPTICS Algorithm

optics <- function(data, epsilon, min_Pts) {
  x <- as.matrix(data)    # data will be created by generateClusterTestDataSimple2D() or similiar function
  dist_x <- as.matrix(stats::dist(x)) # die Distanzmatrix von data
  data_entries <- nrow(x)
  
  processed <- rep(FALSE, data_entries)
  reachability <- rep(NA, data_entries)
  core_distance <- rep(NA, data_entries)
  ordered <- 1L
  
  getNeighbors <- function(point) {
    which(dist_x[point, ] <= epsilon) # Liste an Punkten, die innerhalb von Distanz epsilon um Punkt point liegen, point inklusive
  }
  
  coreDistance <- function(point, neighbors) {
    if(length(neighbors) < min_Pts) {
      return NA
    }
    dists <- dist_x[point, neighbors]
    return(sort(dists)[min_Pts])
  }
  
  for(point in 1:data_entries) {  # for each point in data
    # point.reachability-distance = NULL
    if(processed[point] == FALSE) {   # for each unprocessed point in data
      processed[point] <- TRUE  # mark point as processed
      neighbors <- getNeighbors(point) # N <- getNeighbors(point, epsilon)
      ordered <- c(ordered, point)     # place point in ordered list
      core_distance[point] <- coreDistance(point, neighbors)
      
      if(!is.na(coreDistance(point, neighbors))) {    # if (core-distance(point, epsilon, min_Pts) != NULL) 
        Seeds <- list(points = integer(0), reachability = numeric(0)) # Seeds = empty priority queue
        
      }
    }
    
    return(list(order = ordered, reachability = reachability, core_distance = core_distance))
  }

    

      # update(N, point, Seeds, epsilon, min_Pts)
      # for each next q in Seeds
      # N' = getNeighbors(q, epsilon)
      # mark q as processed
      # place q in ordered list
      # if (core-distance(q, epsilon, min_Pts) != NULL)
        # update(N', q, Seeds, epsilon, min_Pts)
}

update <- function(N, p, Seeds, epsilon, min_Pts) {
  coredist <- core-distance(p, epsilon, min_Pts)
  # for each o in N
    # if (o = unprocessed)
      # new-reach-dist = max(coredist, dist(p, o))
      # if (o.reachability-distance = NULL) o not in Seeds
        # o.reachability-distance = new-reach-dist
        # Seeds.insert(o, new-reach-dist)
    # else      o in Seeds, check improvement
      # if (new-reach-dist < o.reachability-distance)
        # o.reachability-distance = new-reach-dist
        # Seeds.move-up(o, new-reach-dist)
}