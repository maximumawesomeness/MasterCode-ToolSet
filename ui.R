library(shiny)
#install.packages("d3heatmap")
library(d3heatmap)
library(shinythemes)
library(RColorBrewer)

fluidPage(
  theme=shinytheme("readable"),
  navbarPage(
    "Smashing Toolbox",
    
    # TAB UPLOAD ----     
    tabPanel("Data Upload",
             
             titlePanel("Uploading Files"),
             
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose CSV File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),
                 #checkboxInput("reduceThFactors", "Reduce factors to values", FALSE),
                 #checkboxInput("replaceNull", "Replace NULLs to medians", FALSE),
                 #checkboxInput("reduceThNull", "Reduce NULL columns to a threshold", FALSE),
                 
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 # Input: Select quotes ----
                 radioButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 h4("Data Source"),
               "The database is about the quality of the wines and its characteristics which include 13 parameters about containing sugar, pH, acidity etc.. Further information about the open data source and its characteristics can be found here: https://s3.amazonaws.com/udacity-hosted-downloads/ud651/wineQualityInfo.txt. The 13th variable is about the wine type (white=1, red=2) as we have combined the white and red data set. Reason: Interesting correlations might be missed otherwise and differentiating because of colours seems somewhat old fashioned.
",
               tags$hr(),
                 # Output: Data file ----
                 tableOutput("contents"),
               tags$hr(),
               h4("Data Set"),
               "The data set doesn't contain NA values. All the values are numeric. It contains 14 columns (including the index)."
              
                 
               )
               
             )
             
            
             
    ), # finish TAB UPLOAD ----
    
    
    
    
    # TAB K-MEANS   ----   
    tabPanel("K-means", 
             
             # Sidebar panel for inputs  ----
             absolutePanel(
               bottom = 20, right = 20, width = 200,
               draggable = TRUE,
               style = "opacity: 0.92",
               
               selectInput('xcol', 'X Variable', ""),
               selectInput('ycol', 'Y Variable', ""),


               sliderInput("slider_k", "Number of clusters to be analysed:", 1, 20 , 15),
               sliderInput("slider_nstart", "Choose a nstart:", 1, 100,50),
               sliderInput('clusters', "Choose a cluster", 1, 20, 3)
             ),
             
             # MAIN PANEL ----
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          h4("K-means"),
                          "K-Means is an unsupervised, parametric method (need to only pre-specify K number of clusters. The possibility to define the number of clusters can be an advantage in certain situations.

It is a simple way to group data based on similar properties as only a few assumptions are needed. Due to a certain randomness of the algorithm, output can be different with each time the user runs the code. This needs to be considered for interpretation reasons.
",
                          tags$hr(),
                          #h4("Scree Plot"),
                          plotOutput("distPlot"),
                          pre(includeText("KPlotScree.txt")),
                          #h4("K-mean Clustering Plot"),
                          plotOutput("dfClusterPlot"),
                          pre(includeText("KPlotCluster.txt"))
                          
                          
                 ),
                 tabPanel("Informations",
                          
                          h4("What it is:"),
                          "Unsupervised, parametric method (need to pre-specify K number of clusters).",
                          
                          
                          h4("When:"),
                          "First exploration of multidimensional data (few assumptions needed, i.e., K).",
                          h4("What it does:"),
                          "Groups data based on their similarity, the resulting clusters have similar properties.
                          Push the data into K pre-determined categories (can be an advantage in certain situations)",
                          h4("How it does this"),
                          
                          "Choose a number K of cluster centers
                          Place the centers randomly in the data space (initial cluster assignment)
                          Iteratively move the centers to minimize the total within cluster variance (data-center distance) "  
                          
                 )
               ) #end tabsetPanel 
               ) #end main panel 
               ), #end TAB K-MEAN 
    
    
    
    
    
  
    
    
    #TAB HC ----
    tabPanel("HC",
             #SIDEBAR PANEL ----
             absolutePanel(
               bottom = 20, right = 20, width = 150,
               draggable = TRUE,
               style = "opacity: 0.92",
               
               selectInput("method_dis", "Distance methods:",
                           c("Euclidean" = "euclidean",
                             "Maximum" = "maximum",
                             "Manhattan" = "manhattan",
                             "Canberra" = "canberra",
                             "Binary"= "binary",
                             "Minkowski"="minkowski")),

               selectInput("method_link", "Linkage methods:",
                           c("ward.D" = "ward.D",
                             "ward.D2" = "ward.D2",
                             "single" = "single",
                             "complete" = "complete",
                             "average"= "average",
                             "mcquitty"="mcquitty",
                             "median"="median",
                             "centroid"="centroid")),    

               
               checkboxInput("transpose", "Check it to get a tree for the predictors (table column)", FALSE)
               
             ), #end SIDEBAR PANEL
             
             # MAIN PANEL ----
             mainPanel(
               tabsetPanel(
                 # tab Plots ----
                 tabPanel("Plots",
                          h4("HC"),
                          "HC is short for Hierarchical Clustering. It is an Unsupervised, non-parametric method (no labelled data needed) where no assumptions are needed. In comparison to K-Means, there is  no need to specify K number of clusters beforehand. 

The Transposed Dendogram plot shows the distance between the predictors: The lower the altitude of a branch is, the closer the predictors are to each other. Choice of where to cut the dendrogram is not always clear

Height of cut has comparable role as the K in K-means and controls the number of clusters obtained.

With the Matrix of Dissimilarity the similarity in the dataset can be evaluated. Diagonal members equal zero dissimilarity",
                          tags$hr(),
                          
                          plotOutput("dendPlot",width = "120%",height="800px"),
                          pre(includeText("HCPlotDend.txt")),
                          
                          #d3heatmapOutput("heatmap", height="8000px", width="100%"),
                          plotOutput("heatmatPlot",height="800px",width = "120%"),
                          pre(includeText("HCPlotHeat.txt"))
                          
                 ),
                 # tab 2 ----
                 tabPanel("Informations",
                          
                          h4("What it is:"),
                          "Unsupervised, parametric method (need to pre-specify K number of clusters).",
                          
                          
                          h4("When:"),
                          "First exploration of multidimensional data (few assumptions needed, i.e., K).",
                          h4("What it does:"),
                          "Groups data based on their similarity, the resulting clusters have similar properties
                          Push the data into K pre-determined categories (can be an advantage in certain situations)",
                          h4("How it does this"),
                          
                          "Choose a number K of cluster centers
                          Place the centers randomly in the data space (initial cluster assignment)
                          Iteratively move the centers to minimize the total within cluster variance (data-center distance) "  
                          
                 )
               ) #end tabsetPanel
               ) #end main panel
               
            
             ),#end tabPanel TAB HC
    
    
    #TAB PCA ----
    tabPanel("PCA",
             # # Sidebar panel for inputs ----
             # sidebarPanel(
             #   "sidepanel"
             # ), #end SIDEBAR PANEL
             
             # MAIN PANEL ----
             mainPanel(
               tabsetPanel(
                 # Tab plot ----
                 tabPanel("Plots",
                          h4("PCA"),
                          "Unsupervised, linear, non-parametric method. This method is especially meaningful with datasets of more than three dimension as it  reduces dimensionality of a dataset. It is used for grouping variables with similar behaviour and there’s no need for assumptions. The output is easily interpretable as the dimensions got reduced. ",
                          tags$hr(),
                          
                          
                          
                          
                          plotOutput("biPlot",width = "130%",height="800px"),
                          sliderInput("slider_midpoint", "midpoint:", 0, 10 , 0.5,step = 0.1),
                          pre(includeText("pcaPlotBiplot.txt")),
                          
                          plotOutput("scree.pca.Plot"),
                          pre(includeText("pcaPlotBiplot.txt")),
                          plotOutput("pvePlot"),
                          pre(includeText("pcaPlotPVE.txt")),
                          plotOutput("cumulative.pvePlot"),
                          pre(includeText("pcaPlotCPVE.txt")),
                          plotOutput("starPlot"),
                          pre(includeText("pcaPlotStar.txt"))
                  
                 ),
                # Tab Info ----
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",
                          
                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",
                          
                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",
                          
                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions 
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set." 
  
                 ) #end tabPanel
               ) #end tabsetPanel
               ) #end main panel
    ), #end PCA panel
    
    
    #TAB TSNE ----      
    tabPanel("tSNE",
             #SIDEBAR PANEL
             absolutePanel(
               bottom = 20, right = 20, width = 150,
               draggable = TRUE,
               style = "opacity: 0.92",
               selectInput('category.tSNE', 'Select a category', ""),
               sliderInput("slider_dim", "Dimension:", 1, 3 , 2),
               sliderInput("slider_perplexity", "Perplexity:", 1, 50 , 10),
               sliderInput("slider_max_iter", "Max iteration:", 1, 2000 , 500)
             ), #end SIDEBAR PANEL
             
             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          h4("tSNE"),
                          "Here we have an unsupervised, non-linear, parametric method for dimensionality reduction useful for Exploration & visualization of data and well-suited for high-dimensional data. tSNE minimizes the difference between the similarity of points in high & in low-dimensional space. It is easy to apply but not always intuitive to interpret the plots. The output can be different with every time the user runs the code.Distances have (almost) no meaning.",
                          tags$hr(),
                          
                          
                          ## Error message: function plotlyOutput not found
                          #plotlyOutput("tSNEPlot", width = "130%"),
                          pre(includeText("tSNEPlottSNE.txt"))
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",
                          
                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",
                          
                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",
                          
                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions 
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set." 
                          
                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ), #end tSNEpanel        
               
    
    #TAB SOMs ----
    tabPanel("SOMs",
             #SIDEBAR PANEL
             sidebarPanel(
               selectInput('category.SOMs', 'Select a set of feature to be classified', "",multiple = TRUE),
               sliderInput("slider_xdim", "xdim", 1, 50 , 4),
               sliderInput("slider_ydim", "ydim:", 1, 50 , 4),
               checkboxInput("check_scaled", label = "Check if you want scaled data", value = TRUE),
               #sliderInput("slider_max_iter", "Max iteration:", 1, 2000 , 500)
             ), #end SIDEBAR PANEL

             mainPanel(
               tabsetPanel(
                 tabPanel("Plots",
                          h4("SOMs"),
                          "SOM is an unsupervised, nonlinear, parametric method and can be considered as an artificial neural network. It includes mapping from a higher-dimensional input space to a lower-dimensional map space with competitive learning and therefore reduces dimensionality of a datasets. Neural network uses competitive learning. It is used for data visualization of high-dimensional data. This method is somewhat similar to K-means (SOMs with a small number of nodes behave similar to K-means) and similar to PCA as it can be considered to be a nonlinear generalization of PCA.",
                          tags$hr(),

                          
                          
                          
                          
                          
                          plotOutput("somsPlot.change"),
                          pre(includeText("somsPlotChange.txt")),
                          plotOutput("somsPlot.count"),
                          pre(includeText("somsPlotCount.txt")),
                          plotOutput("somsPlot.mapping"),
                          pre(includeText("somsPlotMapping.txt")),
                          plotOutput("somsPlot.dist"),
                          pre(includeText("somsPlotDist.txt")),
                          plotOutput("somsPlot.codes"),
                          pre(includeText("somsPlotCodes.txt")),
                          selectInput('property.SOMs', 'Select a feature to be classified', ""),
                          plotOutput("somsPlot.property"),
                          pre(includeText("somsPlotProperty.txt")),
                          plotOutput("somsPlot.tree",width = "150%"),
                          pre(includeText("somsPlotTree.txt")),
                          numericInput("soms.tree.h", "Height (h) to cut the tree", 1, min = 1, max = 10000),
                          plotOutput("somsPlot.map.hc"),
                          pre(includeText("somsPlotMapHC.txt"))
                 ),
                 tabPanel("Informations",
                          h4("What it is:"),
                          "Unsupervised, linear, non-parametric method.",

                          h4("When:"),
                          "First exploration of multidimensional data (no assumptions needed). Data driven!",

                          h4("What it does:"),
                          "PCA reduces the dimensionality of a dataset in order to be more understandable representation.",

                          h4("How it does this"),
                          "Find new axes (principal components) that represent the data space in a reduced set of dimensions
                          in order capture the most important information in the data.",
                          h4(""),
                          "Capture the maximal variance of the data.",
                          h4(""),
                          "Highlight the global patterns in the data set."

                 ) #end tabPanel
               ) #end tabsetPanel
             ) #end main panel
    ) #end SOMs panel

             )#end navbarpage
)#end Fluid page
  
  
  
  
  
  
  
  
  
  
  
  