---
title: "The K-Medioids Clustering Algorithm"
output: 
  rmarkdown::html_vignette:
    toc: true
    tabset: true
    number_sections: true
vignette: >
  %\VignetteIndexEntry{The K-Medioids Clustering Algorithm}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




``` r
library(clusterfuck)
```

# The K-Medioids Algorithm
Here we will go over all information needed to use the kMedioids function of the clusterfuck package

## 1. General use
This will illustrate the most basic use case for this clustering function:

Lets create some test data

``` r
data <- generateClusterData(n=100, cluster_amount = 4)
```

This is what it looks like

``` r
viewData(data)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/general_use_2-1.svg" alt="" style="display: block; margin: auto;" />

**Apply the K-Medioids-algorithm with K = 4**       

``` r
kMedioids(data, K = 4)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/general_use_3-1.svg" alt="" style="display: block; margin: auto;" />





## 2. Parameters

### 2.1 data (necessary)
This is the main parameter to be provided and the data upon which the clustering algorithm will be deployed. It is recommended your data is a tibble with every row representing a data point. Must have at least 1 data point. 

Here some examples: 

``` r
data_1 <- generateClusterData(n=100)
data_2 <- generateClusterData(n=1)
data_3 <- study_courses_data
```

The data can even have multiple types

``` r
data_4 <- tibble::tibble(characters = study_courses_names, numbers = 1:19)
```




Lets go clustering!

``` r
kMedioids(data_1,K=3)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/data_examples_4-1.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(data_2,K=1,.print_info = TRUE)
#> [1] "calculating dissimilarity matrix"
#> [1] "Done. \n now find starting medioids"
#> [1] "Done. \n now calculating first costs"
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/data_examples_4-2.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(data_3,K=3,custom_distance_function = study_courses_distance)$clustered_data
#> # A tibble: 19 × 2
#>    study_courses     cluster
#>  * <chr>               <int>
#>  1 architecture            3
#>  2 special education       2
#>  3 english                 2
#>  4 physics                 1
#>  5 mathematics             1
#>  6 computer science        1
#>  7 biology                 1
#>  8 chemistry               1
#>  9 geography               3
#> 10 geology                 3
#> 11 greek history           3
#> 12 economics               1
#> 13 egyptoligy              3
#> 14 medical studies         1
#> 15 law                     1
#> 16 music                   2
#> 17 philosophy              1
#> 18 translation             1
#> 19 theater education       2
kMedioids(data_4,K=3,custom_distance_function = characterAndNumeric_distance)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/data_examples_4-3.svg" alt="" style="display: block; margin: auto;" />

### 2.2 K
The number of Clusters the Algorithm tries to fit the data in.
This number must be
- a positive integer 
- larger than 0
- at maximum equal to the amount of unique data points in data

Some examples:

``` r
kMedioids(data_1,K=3)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/K_examples-1.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(data_1,K=5)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/K_examples-2.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(data_1,K=10)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/K_examples-3.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(data_2,K=1)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/K_examples-4.svg" alt="" style="display: block; margin: auto;" />

### 2.3 distance_method
This function comes with a handful of common distance functions pre made. Use this parameter to choose one of the following:

- 'euclidean' (default)
- 'maximum' 
- 'manhattan'
- 'canberra', 
- 'binary',
- 'minkowski' (see more in 2.4)

#### Examples {.tabset}
##### intended clusterings

``` r
viewClusters(euclidean_clustered_data)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/unnamed-chunk-2-1.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(maximum_clustered_data)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/unnamed-chunk-2-2.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(manhattan_clustered_data)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/unnamed-chunk-2-3.svg" alt="" style="display: block; margin: auto;" />

##### euclidean

``` r
kMedioids(euclidean_cluster_data, K=3, distance_method = 'euclidean')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_euclidean-1.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(maximum_cluster_data, K=2, distance_method = 'euclidean')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_euclidean-2.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(manhattan_cluster_data, K=2, distance_method = 'euclidean')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_euclidean-3.svg" alt="" style="display: block; margin: auto;" />

##### maximum

``` r
kMedioids(euclidean_cluster_data, K=3, distance_method = 'maximum')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_maximum-1.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(maximum_cluster_data, K=2, distance_method = 'maximum')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_maximum-2.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(manhattan_cluster_data, K=2, distance_method = 'maximum')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_maximum-3.svg" alt="" style="display: block; margin: auto;" />

##### manhattan

``` r
kMedioids(euclidean_cluster_data, K=3, distance_method = 'manhattan')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_manhattan-1.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(maximum_cluster_data, K=2, distance_method = 'manhattan')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_manhattan-2.svg" alt="" style="display: block; margin: auto;" />

``` r
kMedioids(manhattan_cluster_data, K=2, distance_method = 'manhattan')
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/metric_examples_manhattan-3.svg" alt="" style="display: block; margin: auto;" />

### 2.4 p
If you chose 'minkowski' for the parameter distance (2.3) then this is where you can input the value for the p-metric. Will be ignored, if other distances are chosen.

Examples:

``` r
set.seed(123)
clustering_1 <- kMedioids(data_1,K=5, distance_method = 'minkowski', p = 1)
set.seed(123)
clustering_2 <- kMedioids(data_1,K=5, distance_method = 'minkowski', p = 2)
set.seed(123)
clustering_3 <- kMedioids(data_1,K=5, distance_method = 'minkowski', p = 4)
```

The differences can be visualized using the Voronoi cells:

``` r
full_data <- generateNoiseData(n=1000,lower_bounds = c(0,0), upper_bounds= c(1,1))
colnames(full_data) <- c('X','Y')
viewClusters(full_data,clustering_1$clustering_function)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/p_examples_2-1.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(full_data,clustering_2$clustering_function)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/p_examples_2-2.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(full_data,clustering_3$clustering_function)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/p_examples_2-3.svg" alt="" style="display: block; margin: auto;" />


### 2.5 custom_distance_function
You can also choose a custom distance function. This distance function must be a function with the following properties. If a custom distance function is provided, the parameters distance_method (2.3) and p (2.4) are ignored. 

But please consider:
- Your custom distance function must applicable on the data
- **Using an unoptimized custom distance function can slow** the calculation of kMedioids down significantly!


Inputs:   
- x and y : Two of the data row type. (The internally chosen names do not matter)

Output: 
- Must be greater or equal to 0 (semi-definite)
- Must be zero, if x and y are identical
- Must have the same output, when x and y are switched. (symmetry)


Example:

``` r
discrete_distance <- function(x,y) !identical(x,y) |> as.numeric()
kMedioids(data_1, K=4, custom_distance_function = discrete_distance)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/custom_distance_function-1.svg" alt="" style="display: block; margin: auto;" />

Because this is a main feature of the package, here is another example:

``` r
# the custom function for distance on a globe
distanceByLongitudeAndLatitude <- function(x,y){
  x <- unlist(x,use.names = FALSE)[2:1] * 2*pi/360
  y <- unlist(y,use.names = FALSE)[2:1] * 2*pi/360
  round(2*6371000 * asin(sqrt(sin((y[1]-x[1])/2)^2 +
                       cos(x[1]) * cos(y[1]) *
                     sin((y[2]-x[2])/2)^2)),2)
}
```



``` r
if(require('maps')){
# load some data of word cities
slice_of_the_world <- tibble::tibble(dplyr::slice_sample(n=1000,maps::world.cities[,c('long','lat')]))

clustering <- kMedioids(slice_of_the_world, K=7, custom_distance_function = distanceByLongitudeAndLatitude)
clustering
} 
#> Lade nötiges Paket: maps
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/globe_example_1-1.svg" alt="" style="display: block; margin: auto;" />


``` r
if(require('maps')){
viewClusters(generateNoiseData(n=4000,lower_bounds = c(-180,-90), upper_bounds=c(180,90)),clustering$clustering_function)
} 
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/globe_example_2-1.svg" alt="" style="display: block; margin: auto;" />
### 2.6 .print_info
A logical of length 1
This is useful for debugging or finding loops caused by custom distance functions (2.5). If TRUE some extra info will be displayed in the console

Example: 

``` r
set.seed(12345)
kMedioids(data_1,K=7, .print_info = TRUE)
#> [1] "calculating dissimilarity matrix"
#> [1] "Done. \n now find starting medioids"
#> [1] "Done. \n now calculating first costs"
#> [1] "Found a new best clustering! The new best cost is 19.2590029792854"
#> [1] "changing medioid 3 with data point 80"
#> [1] "Found a new best clustering! The new best cost is 18.8303572164318"
#> [1] "changing medioid 2 with data point 63"
#> [1] "Found a new best clustering! The new best cost is 18.4839979734381"
#> [1] "changing medioid 1 with data point 62"
#> [1] "Found a new best clustering! The new best cost is 18.2880104363251"
#> [1] "changing medioid 4 with data point 27"
#> [1] "Found a new best clustering! The new best cost is 18.2092792094361"
#> [1] "changing medioid 7 with data point 15"
#> [1] "Found a new best clustering! The new best cost is 18.2086295375632"
#> [1] "changing medioid 1 with data point 62"
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/.print_info_example-1.svg" alt="" style="display: block; margin: auto;" />

## 3 Output
The output is a clustering object:
A list of the class clustering with at least an element called 'clustered_data'.
Reed more about clustering objects here TODO

A clustering generated by K-Medioids also has the following properties (elements of the list):

### 3.1. clustered_data
The original data with a column called 'cluster' added.


``` r
kMedioids(data_1,K=4)$clustered_data
#> # A tibble: 100 × 3
#>      X_1   X_2 cluster
#>  * <dbl> <dbl>   <int>
#>  1 0.872 1.03        4
#>  2 0.535 0.286       1
#>  3 0.981 0.523       3
#>  4 0.766 0.523       1
#>  5 1.28  0.322       3
#>  6 0.473 0.537       1
#>  7 0.596 0.192       1
#>  8 0.309 1.43        4
#>  9 0.530 1.08        4
#> 10 0.607 0.753       1
#> # ℹ 90 more rows
```

### 3.2 clustering_function
A function which can relate any data point to their cluster. 
Why this is special with K-Medioids: **This clustering function CAN be applied to new data!**


``` r
clusters <- list(tibble::tibble(X=0.05,Y=0.05),
                 tibble::tibble(X=0.03,Y=0.02),
                 tibble::tibble(X=0.03,Y=0.08),
                 tibble::tibble(X=0.07,Y=0.04))
training_data <- generateClusterDataFromPaths(n=50, clusters)
unknown_data <- generateClusterDataFromPaths(n=100, clusters)
more_data <- generateNoiseData(5000,lower_bounds=c(0,0), upper_bounds = c(0.1,0.1))

# derive a clustering using K-Medioids
f <- kMedioids(training_data,K=4)$clustering_function

f
#> function(x) 1:K |>
#>     sapply(function(k) distance(x,base::unlist(data[old_medioid_indeces[k],]))) |>
#>     base::which.min()
#> <bytecode: 0x5603dc58aca8>
#> <environment: 0x5603e3998930>
f(c(0.5,0.5))
#> [1] 3

# lets take a look
viewClusters(training_data,f)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/clustering_function_example-1.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(unknown_data,f)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/clustering_function_example-2.svg" alt="" style="display: block; margin: auto;" />

``` r
viewClusters(more_data,f)
```

<img src="/tmp/RtmpG9pWIF/preview-1fc5072a67b8d.dir/K-Medioids_files/figure-html/clustering_function_example-3.svg" alt="" style="display: block; margin: auto;" />

### 3.3 medioids
The clustering is calculated by the distance of the data points to the medioids of each cluster. These medioids are also part of the output.


``` r
kMedioids(data_1,K=4)$medioids
#> # A tibble: 4 × 2
#>     X_1   X_2
#>   <dbl> <dbl>
#> 1 0.598 0.561
#> 2 0.230 0.769
#> 3 1.10  0.564
#> 4 0.795 1.12
```

### 3.4 inner_inequality
This is the sum of the distances of all data points to their cluster.
For fixed K this value can score the clustering: The lower, the better.
Be Aware: This is not enough, to directly* find an optimal value for K, as this sum will always decrease for K -> n.
* Although the inner inequality  can be used in the so called 'elbow-method'. More here TODO


``` r
kMedioids(data_1,K=4)$inner_inequality
#> [1] 25.51805
```

### 3.5 sum_of_squares
Similar to inner_inequality (3.4) but the distances are squared before being summed up.


``` r
kMedioids(data_1,K=4)$sum_of_squares
#> [1] 8.613157
```

### 3.6 mean_silhouette
The mean silhouette quantifies how good the data fits into the derived clusters.
It can be valued between -1 and 1:

- near 1: A very good clustering
- near 0: A worse clustering
- exactly 0: Most likely the clustering is a of a special case i.e. only one data point. In this case the mean silhouette gives no information about how good the clustering is.
- less than 0: A wrong clustering, in the sense that there are data points who should have been assigned to other clusters.


``` r
kMedioids(data_1,K=4)$mean_silhouette
#> [1] 0.3378685
```




