install.packages('tibble')
install.packages('dplyr')
install.packages('ggplot2')
library(tibble)
library(dplyr)
library(ggplot2)

generateClusterTestDataSimple2D = function(n=10,nclusters=NULL){
  if(base::missing(nclusters)){
    nclusters <- floor(runif(1,min = 1, max = 2*sqrt(n)))
  }
  
  x_clusters <- runif(nclusters, min=0, max=1)
  y_clusters <- runif(nclusters, min=0, max=1)
  sd_clusters <- runif(nclusters, min=0.001, max=0.25)
  
  
  
  testdata = tibble(selected_clusters = floor(runif(n,1,nclusters+1))) |>
    rowwise() |>
    mutate(X = rnorm(1,x_clusters[selected_clusters],sd_clusters[selected_clusters]),Y = rnorm(1,y_clusters[selected_clusters],sd_clusters[selected_clusters])) |>
    select(X,Y)
  
  return(testdata)
}


ggplot(generateClusterTestDataSimple2D(n=100, nclusters = 3),aes(x=X,y=Y)) + geom_point()
