# to generate a circle as a path of data points
circle <- function(r) tibble::tibble(X=c(r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0,r/sqrt(2),r),Y=c(0,r/sqrt(2),r,r/sqrt(2),0,-r/sqrt(2),-r,-r/sqrt(2),0))

# combine those points to one list of paths
# Note that we include the larger circle more often,
# which causes the density of the clusters to be about equal
circles_paths <- list(circle(0.3),
                      circle(0.6),circle(0.6),
                      circle(1),circle(1),circle(1),circle(1))

# generating the resulting data set
three_concentric_circles <- generateClusterDataFromPaths(n=1000,list_of_paths =  circles_paths)

#viewData(three_concentric_circles)


usethis::use_data(three_concentric_circles, overwrite = TRUE)
