#install.packages("rjson")
library(rjson)
library(data.table)

#Load JSON file
AsJSON <- fromJSON(file = "data/2L1W.json")

#Create a new DF and fill it up from each character entry
dataDF <- data.frame(original_height=numeric(0),original_width=numeric(0),height=numeric(0),width=numeric(0),x=numeric(0),y=numeric(0),label=character(0))
for (i in length(AsJSON)){
  for(char in AsJSON[[i]][["completions"]][[1]][["result"]])
    dataDF[nrow(dataDF) + 1,] =
    list(
    char$original_height,char$original_width,
    char$value$height,char$value$width,
    char$value$x,char$value$y,
    char$value$rectanglelabels)
}


print(nrow(dataDF))
#Use function to convert to Nice proportional list objects


#Sort the fuction by x so letters are in order


#Get info based on word dist from left and right


#Put into DF for model training



#Train a model




#Now that model is trained, save it so it can be loaded