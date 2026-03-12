# to generate a circle as a path of data points
circle <- function(r) tibble::tibble(X=c(r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0,r/sqrt(2),r),Y=c(0,r/sqrt(2),r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0))

# combine those points to one list of paths
# Note that we include the larger circle more often,
# which causes the density of the clusters to be about equal
circles_paths <- list(circle(0.5),
                      circle(1),circle(1),circle(1))

# generating the resulting data set
two_concentric_circles <- generateClusterDataFromPaths(n=500,list_of_paths =  circles_paths)

usethis::use_data(two_concentric_circles, overwrite = TRUE)
