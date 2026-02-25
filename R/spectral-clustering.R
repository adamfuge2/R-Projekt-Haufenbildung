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
gaussKernel <- function(data){
  base::exp(- gamma * base::as.matrix(stats::dist(data)))
}

spectralReduction <- function(data, gamma, k, kernel = gaussKernel, metric = NULL, epsilon = NULL){
  n <- nrow(data)
  W <- kernel(data)
  D <- matrix(0, nrow = n, ncol = n)

  #change into |> apply
  for (i in seq(n)){
    D[i,i] <- W[,i] |> sum()
  }

  L <- D - W

  D_sqrt <- 1/sqrt(D)
  D_sqrt[D_sqrt == Inf] <- 0

  eigenvectors <- eigen(D_sqrt %*% L %*% D_sqrt, symmetric=TRUE)$vectors
  t(eigenvectors[, n:1])[2:(k+1), ] |>
    t() |>
    tibble::as_tibble()

}


