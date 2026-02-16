#OPTICS Algorithm

optics <- function(data, epsilon, min_Pts) {
 # for each point in data
    # point.reachability-distance = NULL
  # for each unprocessed point in data
    # N <- getNeighbors(point, epsilon)
    # mark point as processed
    # place point in ordered list
    # Seeds = empty priority queue
    # if (core-distance(point, epsilon, min_Pts) != NULL) 
      # update(N, point, Seeds, epsilon, min_Pts)
      # for each next q in Seeds
      # N' = getNeighbors(q, epsilon)
      # mark q as processed
      # place q in ordered list
      # if (core-distance(q, epsilon, min_Pts) != NULL)
        # update(N', q, Seeds, epsilon, min_Pts)
}

update <- function(N, p, Seeds, epsilon, min_Pts) {
  coredist <- core-distance(p, epsilon, min_Pts)
  # for each o in N
    # if (o = unprocessed)
      # new-reach-dist = max(coredist, dist(p, o))
      # if (o.reachability-distance = NULL) o not in Seeds
        # o.reachability-distance = new-reach-dist
        # Seeds.insert(o, new-reach-dist)
    # else      o in Seeds, check improvement
      # if (new-reach-dist < o.reachability-distance)
        # o.reachability-distance = new-reach-dist
        # Seeds.move-up(o, new-reach-dist)
}