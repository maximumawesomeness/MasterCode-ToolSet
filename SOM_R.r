#Wine SOMR

install.packages("kohonen")
library(kohonen)


set.seed(4)
# input dataset
# (nice to check accuracy by color code)
# Remove duplicates in data:
input_data <- read.csv("winequality-white.csv",sep = ",", header = TRUE, stringsAsFactors = TRUE, fill = TRUE)
data <- as.matrix(scale(input_data[,1:4]))

# For plotting evaluation against colorcode
# category (~ classification solution)
row_label <- as.factor(rownames(data))
colors <- c("red", "black", "blue")
colors <- colors[as.numeric(iris$Species)]
data_train_matrix <- as.matrix(scale(data))
# Define the neuronal grid
som_grid <- somgrid(xdim = 4, ydim = 4,
                    topo="hexagonal")
# Train the model
som_model <- som(data_train_matrix,
                 grid=som_grid,
                 rlen=1000,
                 alpha=c(0.05,0.01),
                 keep.data = TRUE)
# Check training progress
plot(som_model, type="changes")
# Check how many samples are mapped to each
# node on the map. (5-10 samples per node)
plot(som_model, type="count")
plot(som_model, type="mapping",
     col=colors[row_label])
plot(som_model, type="mapping",
     labels = (rownames(data)),
     col=colors[row_label])

# U-Matrix: measure of distance between each node and its neighbours.
# (Euclidean distance between weight vectors of neighboring neurons)
# Can be used to identify clusters/boundaries within the SOM map.
# Areas of low neighbour distance ~ groups of nodes that are similar.
plot(som_model, type="dist.neighbours")
# Codes / Weight vectors: representative of the samples mapped to a node.
# highlights patterns in the distribution of samples and variables.
plot(som_model, type="codes")
# Heatmaps: identify interesting areas on the map.
# Visualise the distribution of a single variable (defined in [,x])
# across the map
# colnames(data) # to check index to put in [,x]
plot(som_model, type = "property", property = getCodes(som_model, 1)[,2])
# → Make a loop over all the variables ?
# Same as above but with original, unscaled data (can also be useful)
var_unscaled <- aggregate(as.numeric(input_data[,1]),
                          by=list(som_model$unit.classif),FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property",property=var_unscaled)
# Clustering: isolate groups of samples with similar metrics
tree <- as.dendrogram(hclust(dist(as.numeric(unlist(som_model$codes)))))
plot(tree, ylab = "Height (h)")
# Cut the tree somewhere based on the above tree
som_cluster <- cutree(hclust(dist(as.numeric(unlist(som_model$codes)))),
                      h=2) # k groups or at h hight
# Visualize mapping based on HC
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728',
                    '#9467bd', '#8c564b', '#e377c2')
plot(som_model, type="mapping",labels = (rownames(data)),
     bgcol = pretty_palette[som_cluster], col=colors[row_label])
add.cluster.boundaries(som_model,som_cluster)

