library(rjson)
library(randomForest)
library(usethis)

#' makeModel
#'
#' Creates a randomForest word model
#' 
#' @param TaggedJson Json File with tagged letter data
#' @return randomForest model
#' 
#' @importFrom randomForest randomForest
makeModel = function(TaggedJson){
  
  na.exclude <- NULL
  
  #Load JSON file
  AsJSON <- rjson::fromJSON(file = TaggedJson)
  
  #Create a new DF and fill it up from each character entry
  dataDF <- data.frame(line=numeric(0),line_height=numeric(0),line_width=numeric(0),height=numeric(0),width=numeric(0),x=numeric(0),label=character(0))
  for (i in 1:length(AsJSON)){
    for(char in AsJSON[[i]][["annotations"]][[1]][["result"]]){
      dataDF[nrow(dataDF) + 1,] =
      list(i,
      char$original_height,char$original_width,
      char$value$height,char$value$width,
      char$value$x,
      char$value$rectanglelabels)
    }
  }
  
  #Sort DF By first line number, and then x value
  dataDF=dataDF[order( dataDF[,1], dataDF[,6] ),]
  print(nrow(dataDF))
  
  #Add proportionalized data
  dataDF['height_prop'] <- NA
  dataDF['width_prop'] <- NA
  dataDF['to_right_prop'] <- NA
  dataDF['to_left_prop'] <- NA
  
  for(r in 1:nrow(dataDF)){
    row = dataDF[r,]
    prev_row = dataDF[r-1,]
    next_row = dataDF[r+1,]
    
    dataDF[r, 'height_prop'] = row$height/row$line_height
    dataDF[r, 'width_prop'] = row$width/row$line_width
    
    to_right = next_row$x - (row$x + row$width)
    dataDF[r, 'to_right_prop'] = to_right/row$line_width
    
    if(r==1){next}
    to_left = row$x - (prev_row$x + prev_row$width)
    dataDF[r, 'to_left_prop'] = to_left/row$line_width
  } 
  
  dataDF = pmax(dataDF, 0)
  
  trainDF = dataDF[c("label", "height_prop", "width_prop", "to_right_prop", "to_left_prop")]
  aggregateDataDF = aggregate(. ~ label, trainDF, mean)
  print(aggregateDataDF)
  
  #Reconfigure DF for to be put into model
  trainDF$label = factor(trainDF$label)
  
  #Train a model
  train <- sample(nrow(trainDF), 0.7*nrow(trainDF), replace = FALSE)
  TrainSet <- trainDF[train,]
  ValidSet <- trainDF[-train,]
  summary(TrainSet)
  summary(ValidSet)
  
  
  a=c()
  for (i in 1:4) {
    model1 <- randomForest(label ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE, na.action=na.exclude)
    predValid <- predict(model1, ValidSet, type = "class")
    a[i] = mean(predValid == ValidSet$label, na.rm = TRUE)
  }
  
  
  print(a)
  
  #Now that model is trained, save it so it can be loaded
  wordModelNew = NULL
  wordModelNew <- randomForest(label ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE, na.action=na.exclude)
  usethis::use_data(wordModelNew, overwrite = TRUE)
}

