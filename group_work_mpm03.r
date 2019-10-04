#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Config                                                                             #######
#
# 1. set directory with setwd to target director
# 2. make sure target file winequality-white.csv is in target folder
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


#Import packages
library("corrplot")
library("gplots")
library("kohonen")
library("plot3D")
library("plot3Drgl")
library("factoextra") 
library("ggplot2") # Data visualization
library("readr") # CSV file I/O, e.g. the read_csv function
library("RColorBrewer")
library("DataExplorer")

# get rid of old stuff
rm(list=ls()) # clear environment
par(mfrow=c(1,1)) # set plotting window to default

# set printing preferences
options(scipen=99) # penalty for displaying scientific notation
options(digits=4) # suggested number of digits to display

# set seed for same results
set.seed(1)

# set working directory
setwd("C:/Users/rmaurhofer/Desktop/HSLU-Msc/3_SemesterHS19/MPM03/GroupWork")


input_data <- read.csv("winequality-white.csv",sep = ",", header = TRUE, stringsAsFactors = TRUE, fill = TRUE)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Test dataset & playground                                                           #######
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

data.dev <- input_data[1:10,]

cleanfiledata <-   as.data.frame(data.dev[complete.cases(data.dev),])

cleanme <- function(dataname){

  
  
}


plot_missing(input_data)


redfac.1 <- reduceThFactors(input_data, "quality")
redNu.1 <- reduceThNull(input_data, "quality")
conFtN <- convertFactorToNumeric(input_data, "quality")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Create preprocessing functions                                                      #######
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# make built in function to reduce the number of factros in a dataset to a threshold
reduceThFactors <- function(dataset, yvar, threshold = 10, varTypes = c("integer", "double", "logical", "numeric")){
  
  # reduce number of predictors, remove factors with more unique observations then threshold (th)
  # transform characters to numeric
  dim(dataset)
  red <- c()
  for(colnr in 1:ncol(dataset)){
    if(!class(dataset[,colnr]) %in% varTypes && length(unique(dataset[,colnr]))> threshold && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("Is", colnames(dataset)[colnr],"in varType list '", class(dataset[,colnr]), "' and has more than ", threshold, "uniques: ", length(unique(dataset[,colnr]))))
      red <- append(red,c(-1*colnr))
    }
  }
  
  print(red)
  if(!is.null(red)){dataset <- dataset[,red]}
  print(paste("Col Nr: ",colnr, "     -      reduced by: ", length(red), "                - dims: ",dim(dataset)))
  dim(dataset)
  return(dataset)
  
}

# make built in function to reduce null containing columns in a dataset to a threshold
# threshold set based on plot_missing
reduceThNull <- function(dataset, yvar, threshold = 40, omitRest = FALSE){
  
  # reduce number of predictors, remove factors with more percentage of null observations then threshold (th)
  # transform characters to numeric
  dim(dataset)
  red <- c()
  for(colnr in 1:ncol(dataset)){
    perNull <- 100 / length(dataset[,colnr]) * sum(is.na(dataset[,colnr]))
    if(perNull > threshold && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("Has", colnames(dataset)[colnr]," more Nulls '(", sum(is.na(dataset[,colnr])), " -> ", perNull, "%" ,")' than threshold: ", threshold))
      red <- append(red,c(-1*colnr))
    }
  }
  print(red)
  if(!is.null(red)){dataset <- dataset[,red]}
  print(paste("Col Nr: ",colnr, "     -      reduced by: ", length(red), "                - dims: ",dim(dataset)))
  dim(dataset)
  
  if(omitRest){
    dataset <- na.omit(dataset)
    print("Nulls omitted")
  }
  
  return(dataset)
  
}

# make built in function to convert factors to numeric variables
convertFactorToNumeric <- function(dataset, yvar){
  
  for(colnr in 1:ncol(dataset)){
    if(class(dataset[,colnr]) %in% c('factor') && colnames(dataset)[colnr] != as.character(yvar)){
      print(paste("IS factor and gets converted to numeric", colnames(dataset)[colnr]))
      dataset[,colnr] <- as.numeric(dataset[,colnr])
    }
  }
  
  return(dataset)
  
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Create ML functions                                                                 #######
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# KMEans
useKMeans <- function(data = data, k = 4, nstart = ncol(data), plotDimIndexX = 0, plotDimIndexY = 0){
  # Exception handling & preprocessing missing
  #
  #data: numeric matrix of data
  #k: the number of clusters
  #nstart: if centers is a number, how many random sets should be chosen?
  #plotDimIndexX: for two dimensional plot of clustered entries, value of x-axis
  #plotDimIndexY: for two dimensional plot of clustered entries, value of y-axis
  
  
  # Run Kmeans
  km.out <- kmeans(x = data, centers = k, nstart = nstart)
  # only works with two dimensions
  
  if(plotDimIndexX>0 && plotDimIndexY > 0){
    plot (data[,c(plotDimIndexX,plotDimIndexY)], col =(km.out$cluster+1), main ="K-Means Clustering" , xlab = colnames(data[plotDimIndexX]) , ylab = colnames(data[plotDimIndexY]) , pch =20 , cex =2)
  }else if(plotDimIndexX < 0 || plotDimIndexY <0){
    print(paste("X-axis and Y-axis dimensions have to be bigger than 0, but are x:",plotDimIndexX, " and y:",plotDimIndexY))
  }
  
  plot(1:k, km.out$withinss, type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares")
  
  # Keep this as an exercise, check multiple pre-determined values of K and choose the right one.  
  
  #wss <- 0 # intialise
  #for (i in 1:k){
  #  km.out <- kmeans(data,i,nstart = nstart)
  #  wss[i] <- km.out$tot.withinss
  #}
  
  #plot(1:k, wss, type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares")
  
  return(km.out)
  
  
}

# Hierarchical clsutering
useHC <- function(data = data, method = "euclidean", k = 3, predictors = TRUE, heatmap = TRUE){
  # Exception handling & preprocessing missing
  #
  #data: numeric matrix of data
  #method: method to measure the distance
  #k: an integer scalar or vector with the desired number of groups
  #predictors: boolean indicationg if hc should also be applied to predictors
  #heatmap: boolean indicating if heatmap should be plotted
  
  # scale data
  scaled_data <- as.matrix(scale(data))
  
  # calculate distances between data
  distances <- dist(scaled_data, method = method)
  
  # compute hierarchical clustering based in distances calculated above:
  hc <- hclust(distances)
  
  # computes dendrogram graphical representation:
  dend <- as.dendrogram(hc)
  
  # graphical representation
  plot(dend)
  
  # Row- and column-wise clustering, with wished linkage and distance method:
  #hc1 <- hclust(as.dist(1-cor(t(scaled_data), method="kendall")), method="ward")
  #hc2 <- hclust(as.dist(1-cor(scaled_data, method="spearman")), method="single")
  
  # alternative, standard output representation
  # (can be useful for ctrl+find specific things in big trees)
  #str(dend)
  
  #CUT TREE - show level for k
  cutree(hc, k=k)
  
  if(predictors){
    hc.T <- hclust(dist(t(scaled_data), method = method))
    dend.T <- as.dendrogram(hc.T)
    plot(dend.T)
    return(hc.T)
  }
  
  # create heatmap
  if(heatmap){
    heatmap(scaled_data)
  }
  return(hc)
  
  
}



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
#
# Run functions                                                                       #######
#
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# run KMeans with parameters
kmeans.df <- useKMeans(input_data, 2, plotDimIndexX = 1, plotDimIndexY = 2)

# run hierarchical clustering
df.HCT <- useHC(input_data, method = "euclidean", predictors = TRUE, k = 3, heatmap = TRUE)
