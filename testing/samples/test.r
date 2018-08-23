
if(FALSE){
  Person <- data.frame(
    name = c("chris","bliss"),
    gender = c("male","female"),
    age = c(12,31),
    height_cm = c(100,34),
    weight_kg = c(52,83)
  )
  print(Person)
  
  chris <- function(x,count){
    switch(x,
           "chris" = {x="og"},
           "bliss" = {x="new"}
           )
    repeat{
      print(paste(c(x,"printed line 14",count)),collapse="") 
      count <- count - 1
      if(count<=0) break
    }
    
  }
  
  v1 = c(1,2,3,4,5,6)
  v2 = c(2,4,6,8,10,12)
  result_colnames = c("col1","col2","col3")
  result_rownames = c("row1","row2","row3")
  result_matrixnames = c("m1","m2")
  result <- array(c(v1,v2),dim = c(3,3,2),dimnames = list(result_rownames,result_colnames,result_matrixnames))
  chris("chris",10)
  print(result)
}
#dijstraks test

dist_matrix <- matrix(9999,10,10)
#print(dist_matrix)
#this iterates thru the matrix
for(row in 1:nrow(dist_matrix)){
  for(col in 1:ncol(dist_matrix)){
    #assign random vals between 1-30 for map
    dist_matrix[row,col] = sample(1:30,1)
  }
}
print(dist_matrix)

dm_len = length(dist_matrix[,1])
startnode = 1
endnode = dm_len
curcost = dist_matrix

dijkstras <- function(count,src,curcost,endnode){
  dest = numeric(count)
  flag = numeric(count)
  prevnode = numeric(count)
  
  for(i in 1:count){
    prevnode[i] = -1
    dest[i] = curcost[startnode,i]
  }
  
  stepcounter = 2
  while(stepcounter <= dm_len){
    min = 999
    for(g in 1:dm_len){
      if(dest[g] < min && !flag[g]){
        min = dest[g]
        u = g
      } 
    }
    flag[g] = 1
    stepcounter = stepcounter + 1
    
    for(k in 1:dm_len){
      if((dest[u]+curcost[u,k] < dest[k]) && !flag[k]){
        dest[k] = dest[u] + curcost[u,k]
        prevnode[k] = u
      }
    }
  }
  return(prevnode)
}

retainpath = function(f,x){
  curpath = x
  while(f[x] != -1){
    curpath = c(curpath,f[x])
    x = f[x]
    curpath(f,x)
  }
  curpath = c(curpath,1)
  return(curpath)
}

prev = dijkstras(dm_len,startnode,curcost,endnode)
resultpath <- retainpath(prev,endnode)
#print("chris")
print(resultpath)
