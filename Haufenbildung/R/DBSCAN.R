# DBSCAN Algorithm
# Was dieser Code am Ende können soll:
# (gefühlt ist alles was man findet also werden Begriffe sehr inkonsistent deutsch und englisch gemixt bis ich lust habe das zu vereinheitlichen <3)
# 1.: Dichte abschätzen / estimate density
#   um jeden Datenpunkt wird die Anzahl der Punkte innerhalb eines angegebenen Umfelds
#   (Variable "epsilon") gezählt und die drei Arten an Punkten (Kernobjekte, Dichte-erreichbare
#   Objekte, Rauschpunkte) mit einer angegebenen Schwelle "minPts" als minimale Anzahl an 
#   Dichte-erreichbare Objekte (inkl. Datenpunkt) ermittelt
# 2.: Kernpunkte zu einem Cluster zusammenfügen, wenn sie "density-reachable" sind, 
#    also über Dichte-erreichbare Objekte verbunden werden können
# 3.: Dichte-erreichbare Punkte Clustern zuweisen mit Parametern epsilon und minPts
     
dbscan <- function(data, epsilon, min_Pts) {
 x <- as.matrix(data)    # data will be created by generateClusterTestDataSimple2D() or similiar function
 data_entries <- nrow(x)
 dist_x <- as.matrix(dist(x))
 
 cluster = 0L
 # for each element in data (i in data_entries)
    # mark i as visited
    area = data.regionQuery(i, epsilon)
    if sizeof(area) < min_Pts
      # mark i as noise
    else
      # cluster = cluster + 1
      expandCluster(i, area, cluster, epsilon, min_Pts)
 
   return  
}

expandCluster <- function(point, region, cluster, epsilon, min_Pts) {
  # add point to cluster
  # for each element p in region:
    # if p not visited:
      # mark p as visited
      # new_region = data.regionQuery(p, eps)
      # if sizeof(new_region) >= min_Pts
        # region = region joined with new_region
    # if p not in any cluster
      # add p to cluster
      # unmark p as noise
}

regionQuery <- function(point, epsilon) {
  region = list() # Liste an Punkten, die innerhalb von Distanz epsilon um Punkt point liegen, point inklusive
  return region
}