#install.packages("rjson")
#install.packages("randomForest")
library(rjson)
library(data.table)
library(randomForest)

#Load JSON file
AsJSON <- fromJSON(file = "data/7L1W.json")

#Create a new DF and fill it up from each character entry
dataDF <- data.frame(line=numeric(0),original_height=numeric(0),original_width=numeric(0),height=numeric(0),width=numeric(0),x=numeric(0),y=numeric(0),label=character(0))
for (i in 1:length(AsJSON)){
  for(char in AsJSON[[i]][["completions"]][[1]][["result"]]){
    dataDF[nrow(dataDF) + 1,] =
    list(i,
    char$original_height,char$original_width,
    char$value$height,char$value$width,
    char$value$x,char$value$y,
    char$value$rectanglelabels)
  }
}


#Sort DF By first line number, and then x value
dataDF=dataDF[order( dataDF[,1], dataDF[,6] ),]

#Add proportionalized data
dataDF['height_prop'] <- NA
dataDF['width_prop'] <- NA
dataDF['to_right_prop'] <- NA
dataDF['to_left_prop'] <- NA

for(r in 1:nrow(dataDF)){
  row = dataDF[r,]
  prev_row = dataDF[r-1,]
  next_row = dataDF[r+1,]
  
  dataDF[r, 'height_prop'] = row$height/row$original_height
  dataDF[r, 'width_prop'] = row$width/row$original_width
  
  to_right = next_row$x - (row$x + row$width)
  dataDF[r, 'to_right_prop'] = to_right/row$original_width
  
  if(r==1){next}
  to_left = row$x - (prev_row$x + prev_row$width)
  dataDF[r, 'to_left_prop'] = to_left/row$original_width
} 


dataDF = dataDF[c("label", "height_prop", "width_prop", "to_right_prop", "to_left_prop")]
#Reconfigure DF for to be put into model
dataDF$label = factor(dataDF$label)

#Train a model
set.seed(100)
train <- sample(nrow(dataDF), 0.7*nrow(dataDF), replace = FALSE)
TrainSet <- dataDF[train,]
ValidSet <- dataDF[-train,]
#summary(TrainSet)
#summary(ValidSet)

a=c()
for (i in 1:4) {
  model3 <- randomForest(label ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE, na.action=na.exclude)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i] = mean(predValid == ValidSet$label)
}
print(nrow(dataDF))
print(a)


#Now that model is trained, save it so it can be loaded