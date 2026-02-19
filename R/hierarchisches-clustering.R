#Test for hierarchical clustering
library(tibble)
library(dplyr)


random_tbl <- function(row, col){
  tibble(x = sample(1:100, row, replace = TRUE),
         y = runif(row, 0,10),
         z = runif(row, 100,1000))
}


#Metrics
eukl_distance <- function(p1, p2){   #Euklidian distance between two points of arbitrary dimension
  stopifnot("Points must be of equal dimension" = length(p1) == length(p2))
  len <- length(p1)
  (p2 - p1)^2 %>% sum() %>% sqrt()
}



#Modes
centroid_det <- function(tbl){   #determine centroid of a tibble (returns a tibble with one row)
  tbl %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
}

centroid <- function(metric, tbl1, tbl2){
  centr_1 <- centroid_det(tbl1)
  centr_2 <- centroid_det(tbl2)
  metric(centr_1, centr_2)
}

average <- function(metric, tbl1, tbl2){

}


hierarchical_clustering <- function(tbl, mode = average, metric = eukl_distance){
  df <- rowid_to_column(tbl, "cluster")   #maybe other name here

  while(df$cluster |> unique() |> length() > 2){
    min_dist <- Inf   #minimal distance between two points
    neighbors <- c(0,0)
    cluster <- df$cluster |> unique()
    #df_group <- group_by(df, cluster)
    for (i in cluster){
      for (j in cluster[cluster != i]){
        tbl1 <- filter(df, cluster == i)
        tbl2 <- filter(df, cluster == j)
        dist <- mode(metric, tbl1, tbl2)
        if (dist <= min_dist) {
          min_dist <- dist
          neighbors <- c(i, j)
        }
      }
    }
    df <- df |> mutate(cluster = ifelse(cluster == neighbors[1], neighbors[2], cluster )) #assign all datapoints within the merged cluster the same id

    #Only for tests
    print(df)
    print(neighbors)
    Sys.sleep(5)
  }
}



test_df <- random_tbl(10, 3)
test_df
test_df %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))


p1 <- slice(test_df, 1)
p2 <- slice(test_df, 2)
eukl_distance(p1, p2)

rowid_to_column(test_df, "cluster") |> select(cluster) |> unique() |> nrow()
hierarchical_clustering(test_df)

rowid_to_column(test_df, "cluster") |> group_by(cluster)
test_df <- add_column(test_df, id = c(1,1,1,2,2,2,2,4,5,5))

#test_df |> group_by(id) |>
clus <- unique(test_df$id)

df <- rowid_to_column(df, "cluster")   #maybe other name here
cluster <- df$cluster |> unique()
for (i in cluster){
  for (j in cluster[cluster != i]){
    print(c(i,j))
  }
}
df <- random_tbl(10,4)
df <- df |> mutate(z = NULL)
df
hierarchical_clustering(df, mode = centroid)
plot(df)# Hierarchisches Clustering
