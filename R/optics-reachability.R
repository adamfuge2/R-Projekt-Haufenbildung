# reachability plot for OPTICS algorithm
as.reachability <- function(object, ...) {
  UseMethod("as.reachability")
}

as.reachability.optics <- function(object, ...) {
  structure(
    list(
      order = object$order,
      reachdist = object$reachdist
    ),
    class = "reachability"
  )
}

plot.reachability <- function(x, eps=NULL,...){
  rd <- x$reachdist[x$order]
  n <- length(rd)
  if(!is.null(eps)){
    cluster_original <- extractClusters(x, eps)
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

  if(!is.null(eps))
    abline(h=eps, col="red", lty=2)
}

plot.optics <- function(x, ...) {
  reach <- as.reachability(x)
  plot(reach, ...)
}

extractClusters <- function(object, ...) {
  UseMethod("extractClusters")
}

extractClusters.optics <- function(object, eps){
  rd <- object$reachdist[object$order]
  cd <- object$coredist[object$order]
  n <- length(rd)
  cluster_ordered <- integer(n)
  cluster_id <- 0
  tol <- .Machine$double.eps * 100 # toleranz, weil Täler noch nicht gut aussehen

  for(i in seq_len(n)){
    rd_val <- if(is.na(rd[i])) Inf else rd[i]
    if(rd_val > eps+tol){
      if(!is.na(cd[i]) && cd[i] <= eps+tol){
        cluster_id <- cluster_id + 1
        cluster_ordered[i] <- cluster_id
      } else {
        cluster_ordered[i] <- 0
      }
    } else {
      cluster_ordered[i] <- cluster_id
    }
  }

  cluster <- integer(n)
  cluster[object$order] <- cluster_ordered

  min_cluster_size <- object$minPts
  cluster_sizes <- table(cluster)
  small <- names(cluster_sizes[cluster_sizes < min_cluster_size])
  cluster[cluster %in% as.numeric(small)] <- 0

  return(cluster)
}

extractClusters.reachability <- function(object, eps) {
  rd <- object$reachdist[object$order]
  n <- length(rd)
  cluster <- integer(n)
  current_cluster <- 0

  for (i in 1:n) {
    if (is.na(rd[i]) || rd[i] > eps) {
      current_cluster <- current_cluster + 1
    }
    cluster[i] <- current_cluster
  }
  cluster
}

