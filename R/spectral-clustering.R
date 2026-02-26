# spectral clustering

df <- tibble::tibble(x = c(1,2,3), y = c(4, 5, 6))
df
tibble::as_tibble(as.matrix(dist(df)))

gamma <- 5
k <- 2

W <- as.matrix(dist(df))
exp(- gamma * as.matrix(dist(df)))

D <- matrix(0, nrow = 3, ncol = 3)

for (i in seq(nrow(W))){
  print(W[,i] |> sum())  #D_ii
  D[i,i] <- W[,i] |> sum()
}


L <- D - W

D_s <- 1/sqrt(D)
D_s[D_s == Inf] <- 0

eig <- eigen(D_s %*% L %*% D_s, symmetric=TRUE)
eig$vectors
eig$vectors[, ncol(eig$vectors):1]
t(eig$vectors[, ncol(eig$vectors):1])
t(eig$vectors[, ncol(eig$vectors):1])[2:3,]



#as tibbles:
gaussKernelWeights <- function(data,gamma){
  base::exp(- gamma * base::as.matrix(stats::dist(data)))
}

spectralReduction <- function(data, gamma, k, mercian_kernel = 'gauss', custom_mercian_kernel = NULL, metric = NULL, epsilon = NULL){
  n <- nrow(data)


  if(missing(custom_mercian_kernel)){
    if(mercian_kernel == 'gauss') W <- gaussKernelWeights(data,gamma)
    else stop('Unknown mercian kernel and no custom mercian kernel provided')
  }
  else {
    W <- dissimilarityMatrix(data,custom_mercian_kernel)
  }


  D <- apply(W,1,sum) |> diag()

  L <- D - W

  D_sqrt <- 1/sqrt(D)
  D_sqrt[D_sqrt == Inf] <- 0

  # calculate eigenvectors
  b <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)$vectors[,n:1]

  # transform to be solutions of our optiization problem
  beta <- apply(b,c(2),function(x) n^(-1/2)*D_sqrt %*% x)

  # retrieve the new transformed data points (columns of a).
  # The eigenvector belonging to the lowest eigenvalue gets left out, as it is
  # linearly dependent to (1,1, ... ,1) and thus bears no information.
  a <- t(beta)[2:(k+1), ]


  # prevent conversion to atomic vector, if k = 1
  dim(a) <- c(k,n)

  # save the data points in tibble as usual
  reduced_data <- tibble::as_tibble(t(a))

  # useful names
  colnames(reduced_data) <- paste0('X_',1:k)

  return(list(reduced_data = reduced_data, transform_function = function(...) stop('not implemented yet')))
}

data <- generateClusterTestDataSimple(n=100,dim=3,cluster_amount = 2)

viewClusters(data)
viewClusters(spectralReduction(data,gamma=5,k=1)$reduced_data)
viewClusters(reduced_data)
