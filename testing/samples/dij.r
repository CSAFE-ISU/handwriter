### Create a distance matrix S.
### Set all the impossible connections between nodes to a large number.
### Because the algorithm is looking for a minimum, very large distances will never be selected

S = matrix(999, 7, 7)
for(row in 1:nrow(S)){
  for(col in 1:ncol(S)){
    #assign random vals between 1-30 for map
    S[row,col] = sample(1:30,1)
  }
}

### List of input parameters for function
n = length(S[, 1]) #number of nodes
v = 1 #source node
dest = n #destination node
cost = S #distance matrix

### Dijkstra's algorithm
dijkstra = function(n, v, cost, dest) {
  #create empty variables to store data
  dest = numeric(n)
  flag = numeric(n)
  prev = numeric(n)
  
  # for every node in the network
  for (i in 1:n) {
    prev[i] = -1
    dest[i] = cost[v, i] #= distance from start node v to every other node i in the network
  }
  
  #initialise counter which keeps track of number of steps through network
  count = 2
  
  # until we have reached our destination node n
  while (count <= n) {
    min = 999
    
    # loop over each node
    for (w in 1:n) {
      #if the new path is less long than the existing smallest one and flag[w] is equal to zero (aka we've not already incuded that node in route)
      if (dest[w] < min && !flag[w]) {
        # overwrite the minimum with the new shortest path and update counter
        min = dest[w]
        u = w
      }
    }
    flag[u] = 1 #indicate that we go to this site
    count = count + 1
    
    # loop over each node again keeping in mind where we have already been
    for (w in 1:n) {
      #if the new route is shorter than the previous route
      if ((dest[u] + cost[u, w] < dest[w]) && !flag[w]) {
        dest[w] = dest[u] + cost[u, w] #update the distance to destination
        prev[w] = u #keep track of the node visited
      }
    }
  }
  return(prev)
}

### create function which returns path
savepath = function(f, x) {
  path = x
  while (f[x] != -1) {
    path = c(path, f[x])
    x = f[x]
    savepath(f, x)
  }
  path = c(path, 1)
  return(path)
}

### Run Dijkstra's algorithm with our distance matrix
prev = dijkstra(n, v, cost, dest)
path = savepath(prev, dest)

### Print path
print(path)
