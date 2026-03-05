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

plot.reachability <- function(x,
                              eps = NULL,
                              col = NULL,
                              xlab = "Order",
                              ylab = "Reachability distance",
                              main = "Reachability Plot",
                              ...) {
  
  rd <- x$reachdist[x$order]
  rd_plot <- rd
  rd_plot[!is.finite(rd_plot)] <- max(rd_plot[is.finite(rd_plot)]) * 1.05
  n <- length(rd)
  
  if(!is.null(eps)) {
    cluster <- extractClusters(x, eps)
    cluster <- cluster[x$order]
  } else {
    cluster <- rep(1, n)
  }
  
  if(is.null(col)) {
    col <- rainbow(max(cluster)+1)
  }
  
  bar_col <- col[cluster +1]
  bar_col[!is.finite(rd)] <- "black"
  
  plot(
    rd,
    type = "h",
    col = bar_col,
    lwd = 2,
    xlab = "Order",
    ylab = "Reachability distance",
    ...
  )
  
  if (!is.null(eps)) {
    abline(h = eps, col = "red", lty = 2)
  }
}

plot.optics <- function(x, ...) {
  reach <- as.reachability(x)
  plot(reach, ...)
}

extractClusters <- function(object, ...) {
  UseMethod("extractClusters")
}

extractClusters.optics <- function(object, eps) {
  
  rd <- object$reachdist[object$order]
  cd <- object$coredist[object$order]
  
  n <- length(rd)
  
  cluster_ordered <- integer(n)
  current_cluster <- 0
  
  for (i in seq_len(n)) {
    
    if (is.na(rd[i]) || rd[i] > eps) {
      
      if (!is.na(cd[i]) && cd[i] <= eps) {
        current_cluster <- current_cluster + 1
        cluster_ordered[i] <- current_cluster
      } else {
        cluster_ordered[i] <- 0
      }
      
    } else {
      cluster_ordered[i] <- current_cluster
    }
  }
  
  cluster <- integer(n)
  cluster[object$order] <- cluster_ordered
  
  cluster
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

cluster_colors <- function(cluster) {
  if (length(cluster) == 0) {
    return(NULL)
  }
  
  k <- max(cluster)
  cols <- rainbow(k)
  col_vec <- cols[cluster]
  col_vec[cluster == 0] <- "black"
  col_vec
}

plot_optics_tree <- function(data, result){
  
  ord <- result$order
  
  plot(data$X, data$Y, pch=16)
  
  for(i in ord){
    
    p <- result$predecessor[i]
    
    if(!is.na(p)){
      
      segments(
        data$X[i],
        data$Y[i],
        data$X[p],
        data$Y[p],
        col="grey"
      )
      
    }
    
  }
  
}

