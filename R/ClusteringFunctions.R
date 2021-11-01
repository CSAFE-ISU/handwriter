#ClusteringFunctions

ClusterPurity = function(truth, estClass)
{
  x = table(truth, estClass)
  x = apply(x, 1, max)
  return(sum(x)/length(estClass))
}


