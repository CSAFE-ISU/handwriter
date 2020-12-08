#install.packages("rjson")
library(rjson)
library(data.table)

#Load JSON file
AsJSON <- fromJSON(file = "data/2L1W.json")

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


#Map to function to do line by line math and get ratios


#Put into DF for model training



#Train a model




#Now that model is trained, save it so it can be loaded