library(Rtsne)
#R-shiny:
# -> sliders for dim, perplexity and max_iter
# -> choose category box

tsne <- function(data,dim,perplexity,max_iter,category){
  # Remove duplicates in data:
  data_unique <- unique(data)
  
  #TODO: separate the searched category column from the rest of the dataset
  data <- as.matrix(scale(data_unique[,1:4])) # hier the separation is manual
  
  # For plotting evaluation against colorcode # category (~ classification solution) 
  row_label <- as.factor(rownames(data)) #label from rows.....
  levels_category<-(as.factor(data_unique[,category]))# convert the categorx to levels
  colors <- rainbow(nlevels(levels_category))#set color palete for the category
  colors <- colors[as.numeric(levels_category)] #set colors to the chosen category
  
  
  # Run tSNE:
  tsne <- Rtsne(data, dims = dim,
                perplexity=perplexity, verbose=TRUE,
                max_iter = max_iter)
  
  # Plot data and labels:
  plot(tsne$Y)
  text(tsne$Y, labels=row_label,
       col=colors[row_label])
  #TODO: put in the plot which color is every factor of the searched category (eg. blue is setosa, red is versicolor etc.)
  
}

tsne(iris,2,10,500,"Species")


