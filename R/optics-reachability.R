# reachability plot for OPTICS algorithm
#' @export
as.reachability <- function(object, ...) {
  UseMethod("as.reachability")
}

#' Form reachability object from optics clusterin
#'
#' @param object reachability object
#'
#' @export
as.reachability.optics <- function(object, ...) {
  structure(
    list(
      order = object$order,
      reachdist = object$reachdist
    ),
    class = "reachability"
  )
}

#' Reachability plot for OPTICS
#'
#' Visualizes the reachability distances of an OPTICS object.
#'
#' @param x an object of class `"optics"`
#' @param ... additional plotting arguments
#'
#' @returns a reachability plot of the clustered data
#'
#' @export
plot.optics <- function(x, epsilon=NULL,...) {
  reach <- as.reachability(x)
  plot(reach, epsilon=epsilon, x_optics=x,...)
}

#' Plot reachability object
#'
#' @param x reachability object
#' @param epsilon epsilon margin
#'
#' @returns a reachability plot of the clustered data
#'
#' @export
plot.reachability <- function(x, epsilon=NULL, x_optics=NULL,...){
  rd <- x$reachdist[x$order]
  n <- length(rd)
  if(!is.null(epsilon) && !is.null(x_optics)) {
    cluster_original <- extractClusters(x_optics, epsilon)
    cluster_original[is.na(cluster_original)] <- 0
    cluster_ordered <- cluster_original[x$order]
    K <- max(cluster_original, na.rm=TRUE)
    cols <- grDevices::rainbow(K)
    palette <- c("0"="black", stats::setNames(cols,1:K))
    bar_cols <- palette[as.character(cluster_ordered)]
  } else {
    bar_cols <- rep("black",n)
  }

  rd_plot <- rd
  max_val <- max(rd[is.finite(rd)])
  rd_plot[!is.finite(rd)] <- max_val * 1.05

  plot(
    rd_plot,
    type="h",
    col=bar_cols,
    lwd=2,
    xlab="Order",
    ylab="Reachability distance",
    main="Reachability Plot"
  )

  if(!is.null(epsilon))
    abline(h=epsilon, col="red", lty=2)
}

#' @export
extractClusters <- function(object, epsilon) {
  UseMethod("extractClusters")
}

#' Extract clusters from an OPTICS ordering
#'
#' Computes cluster assignments from an OPTICS result using
#' a reachability threshold.
#'
#' @param object an object of class `"optics"`
#' @param epsilon reachability threshold epsilon
#'
#' @returns integer vector of cluster labels
#'
#' @examples
#' data <- generateClusterData(n = 100)
#' result <- optics(data, epsilon = 0.1, min_Pts = 5)
#' cluster <- extractClusters(result, epsilon= 0.05)
#'
#' @export
extractClusters.optics <- function(object, epsilon){

  if(is.null(object$reachdist) || length(object$reachdist)==0) {
    return(integer(nrow(object$data)))
  }

  if(is.null(object$order) || length(object$order) == 0)
    return(integer(nrow(object$data)))

  ord <- object$order
  ord <- ord[!is.na(ord)]
  if(length(ord) == 0)
    return(integer(nrow(object$data)))

  rd <- as.numeric(object$reachdist[ord])
  cd <- as.numeric(object$coredist[ord])
  n <- length(rd)

  cluster_ordered <- integer(n)
  cluster_id <- 0L
  tol <- .Machine$double.eps * 100 # toleranz, weil Täler noch nicht gut aussehen
  # ^ eps is not equal to the function parameter epsilon
  rd[is.na(rd)] <- Inf

  for(i in seq_len(n)){

    rd_val <- rd[i]
    if(is.na(rd_val) || length(rd_val) == 0L)
      rd_val <- Inf

    cd_val <- if(length(cd) >= i) {
      cd[i]
    } else NA_real_
    if(length(cd_val) == 0L)
      cd_val <- NA_real_


    if(rd_val > (epsilon + tol)) {
      if(!is.na(cd_val) && cd_val <= (epsilon + tol)){
        cluster_id <- cluster_id + 1L
        cluster_ordered[i] <- cluster_id
      } else {
        cluster_ordered[i] <- 0L
      }
    } else {
      cluster_ordered[i] <- cluster_id
    }
  }

  cluster <- integer(nrow(object$data))
  cluster[ord] <- cluster_ordered

  min_cluster_size <- if(!is.null(object$minPts)) {
    object$minPts
    } else 1L
  cluster_sizes <- table(cluster)
  small <- names(cluster_sizes)[cluster_sizes < min_cluster_size & names(cluster_sizes) != "0"]
  if(length(small) > 0L) {
    cluster[cluster %in% as.numeric(small)] <- 0
  }
  return(cluster)
}

#' Extract clusters from an reachability object
#'
#' Computes cluster assignments from an object using the reachability threshold directly
#'
#' @param object an object of class `reachability`
#' @param epsilon reachability threshold epsilon
#'
#' @returns integer vector of cluster labels
#' @export
extractClusters.reachability <- function(object, epsilon) {
  rd <- object$reachdist[object$order]
  n <- length(rd)
  cluster <- integer(n)
  current_cluster <- 0

  for (i in 1:n) {
    if (is.na(rd[i]) || rd[i] > epsilon) {
      current_cluster <- current_cluster + 1
    }
    cluster[i] <- current_cluster
  }
  cluster
}

