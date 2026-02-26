# spectral clustering


#as tibbles:
gaussKernel <- function(data, gamma){
  base::exp(- gamma * base::as.matrix(stats::dist(data)))
}

#' Spectral Reduction
#'
#' Algorithm to decrease the dimension of a dataset to use for spectral clustering.
#'
#' @param data a tibble or atomic vector with n rows representing d-dimensional
#' points
#' @param gamma reduction faktor for the kernel function
#' @param k dimensional reduction (output will be of dimension k)
#' @param kernel kernel function (see algorithm description)
#' @param metric metric used in kernel function
#' @param epsilon minimal distance for reduction
#'
#' @returns tibble with eigenvectors of reduced dimension k
#'
#' @export
spectralReduction <- function(data,
                              gamma,
                              k,
                              kernel = gaussKernel,
                              metric = NULL,
                              epsilon = NULL){
  n <- nrow(data)
  W <- kernel(data, gamma)
  D <- matrix(0, nrow = n, ncol = n)

  #change into |> apply
  for (i in seq(n)){
    D[i,i] <- W[,i] |> sum()
  }

  L <- D - W

  D_sqrt <- 1/sqrt(D)    #D^(-1/2)
  D_sqrt[D_sqrt == Inf] <- 0

  eigenvectors <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)$vectors
  eigenvectors[, (k+1):2] |>
    tibble::as_tibble()
  #t(eigenvectors[, n:1])[2:(k+1), ] |>
}


