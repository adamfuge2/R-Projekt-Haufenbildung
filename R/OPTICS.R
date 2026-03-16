#OPTICS Algorithm
#'
#' Implementation of the OPTICS algorithm
#' (Ordering Points To Identify the Clustering Structure).
#'
#' @param data      a tibble with with every row representing a data point.
#'            The number columns is therefore the dimensionality,
#'            The number of rows is the sample size and called n.
#' @param epsilon maximum neighborhood radius
#' @param min_Pts minimum number of points for core distance
#' @inheritParams getDistanceFunction
#'
#' @returns An object of class `"optics"` containing
#' \itemize{
#'   \item `order` ordering of the points
#'   \item `reachdist` reachability distances
#'   \item `coredist` core distances
#'   \item `data`
#'   \item `minPts` minimum number of points for core distance
#' }
#'
#' @references https://en.wikipedia.org/wiki/OPTICS
#' @export
optics <- function(data, epsilon, min_Pts,
                   distance_method = "euclidean",
                   p = NULL,
                   custom_distance_function = NULL) {

  stopifnot("data must be data.frame or tibble" = is.data.frame(data))
  stopifnot("data must have at least one row" = nrow(data) >= 1)
  stopifnot("data must have at least one columns" = ncol(data) >= 1)
  if(is.null(distance_method) && is.null(custom_distance_function)) {
    stopifnot("data must contain only numeric values for default distance" = all(vapply(data, is.numeric, logical(1))))
  }
  stopifnot("epsilon must be positive and numeric" = is.numeric(epsilon) && epsilon > 0)
  stopifnot("min_Pts must be a positive integer" = is.wholenumber(min_Pts) && min_Pts > 0)

  x <- as.matrix(data)
  if(is.null(distance_method)) {
    dist_x <- as.matrix(stats::dist(x))
  } else {
    dist_x <- dissimilarityMatrix(
      data,
      distance_method = distance_method,
      p = p,
      custom_distance_function = custom_distance_function)
  }
  n <- nrow(x)

  processed <- rep(FALSE, n)
  reachability <- rep(Inf, n)
  core_distance <- rep(NA, n)
  ordered <- integer(0)
  predecessor <- rep(NA_integer_, n)

  getNeighbors <- function(point) {
    which(dist_x[point, ] <= epsilon)
  }

  coreDistance <- function(point, neighbors) {
    if(length(neighbors) < min_Pts) {
      return(NA)
    }
    dists <- dist_x[point, neighbors]
    sort(dists, partial = min_Pts)[min_Pts]
  }

  update <- function(neighbors, point, seeds_points, seeds_reach) {
    coredist <- core_distance[point]
    res <- lapply(neighbors, function(o){ # for each o in N
      if(processed[o]) return(NULL)  # if (o = unprocessed)
        new_reach <- max(coredist, dist_x[point, o]) # new-reach-dist = max(coredist, dist(p, o))
        if(is.infinite(reachability[o])){ # if (o.reachability-distance = NULL)
          # o not in Seeds:
          reachability[o] <<- new_reach #o.reachability-distance = new-reach-dist
          list(add_point = o, add_reach = new_reach)

        } else if(new_reach < reachability[o]){ #else: o in Seeds, check improvement// if (new-reach-dist < o.reachability-distance)
          reachability[o] <<- new_reach # o.reachability-distance = new-reach-dist
          # Seeds.move-up(o, new-reach-dist)
          i <- which(seeds_points == o)
          if(length(i) > 0) seeds_reach[i] <<- new_reach
          NULL
        } else {
          NULL
        }
    })

    adds <- Filter(Negate(is.null), res)
    if(length(adds) > 0){
      seeds_points <- c(seeds_points, sapply(adds, `[[`, "add_point"))
      seeds_reach  <- c(seeds_reach,  sapply(adds, `[[`, "add_reach")) # Seeds.insert(o, new-reach-dist)
    }

    list(
      points = seeds_points,
      reach  = seeds_reach
    )
  }

  for(point in seq_len(n)) {  # for each point in data
    # point.reachability-distance = NULL
    if(processed[point] == FALSE) {   # for each unprocessed point in data
      neighbors <- getNeighbors(point) # N <- getNeighbors(point, epsilon)
      processed[point] <- TRUE  # mark point as processed
      ordered <- c(ordered, point)     # place point in ordered list
      core_distance[point] <- coreDistance(point, neighbors)
        if(!is.na(core_distance[point])) {    # if (core-distance(point, epsilon, min_Pts) != NULL)
        seeds_points <- integer(0)
        seeds_reach <- numeric(0)
        seeds <- update(neighbors, point, seeds_points, seeds_reach) # update(N, point, Seeds, epsilon, min_Pts)
        seeds_points <- seeds$points
        seeds_reach <- seeds$reach

        while (length(seeds_points) > 0) { # for each next q in Seeds
          min_reach <- min(seeds_reach)
          cands <- which(seeds_reach == min_reach)
          if (length(cands) > 1) {
            i <- cands[which.min(seeds_points[cands])]
          } else {
            i <- cands
          }
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
      coredist = core_distance,
      data = data,
      minPts = min_Pts
    ),
    description = "OPTICS ordering",
    class = c("optics", "clustering")
  ))
}
