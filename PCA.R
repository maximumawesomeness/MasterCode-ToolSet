##############
## Get Data ##
install.packages("bazar")
library(bazar)

setwd("C:/Users/Bornatico Adeline/Documents/HSLU/MachineLearning2/wine-quality-selection/")
white <- read.csv("winequality-white.csv")
str(white)
head(white)

#############
## fit PCA ##

pcafunction<- function(data, componentx, componenty){
    data.scaled<- as.matrix(scale(data))
    pca.data <- prcomp(data.scaled, scale = TRUE)



    ##############
    ## Loadings ##
    cat("--------------------------\n
    -------------------------------Overview of PCA---------------\n
    ---------------------------")
    print(pca.data)
    ## PC1 ~= average
    ## PC2 ~= shape
    
    cat("-------------\n
    ---------------Interpretation:--------------\n
    We can find an overview of the loadings of the principal components.\n
    From this we can try to interpret the meaning of the components.\n
    Together they explain 41.1% of the total variance
    In this case PC1 seems to represent the density with low values for heavy, sugary wines and low values for light wines with hgher alcohol amount.
    PC2 seems to represent Acidity or the Ph-Value
    PC3 seems to represent (bad) quality of the wine: The higher the amount of undesired volatile acidity the higher pc3.-------\n
    ----------------")
    

    cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
    pause(duration = Inf)

    ########################
    ## variance explained ##
    cat("-------------\n
    ---------------PCA Summary and Screeplot:--------\n
    ----------------")
    print(summary(pca.data))
    
    cat("-------------\n
    ---------------Interpretation:--------------\n
    The first Component is explaining 27.9% of the total Veriance,\n
    the second Component is explaining 13.2% of the Variance.\n
    Together they explain 41.1% of the total variance.\n
    This seems to be a rather 'weak' result, comparing to other usecases, where the first component alone iy explaining more than 50%.
    In this case, we would probably have also a look on the third component to explain more of the variance.-------\n
    ----------------")

    cat("-------------\n
    ---------------From PCA, get the standard deviation, and variance.--------\n
    ----------------")
    print(pca.data$sdev)
    pca.var=pca.data$sdev^2
    print(pca.var)

    screeplot(pca.data)
    
    
    cat("-------------\n
    ---------------Interpretation:--------------\n
   Relatively we see, that the 1PCa explains by far the moste variance. 
   After the first the explanatory value of the other components kind of king and stick on thea same level. 
   If we woul consider the ellbowcriteria we would only take the first component.-------\n
    ----------------")

    pause(duration = Inf)

    ###Screeplot
    # In order to compute the proportion of variance explained by each principal component (variance explained
    #by each principal component / total variance explained by all four principal components)
    pve=pca.var/sum(pca.var)
    pve
    plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",ylim=c(0,1),type='b')
    plot(cumsum(pve),xlab="PrincipalComponent",ylab="Cumulative Proportion of Variance
    Explained",ylim=c(0,1),type='b')




    cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
    pause(duration = Inf)

    ###########################
    # Access all stuff computed by PCA.
    cat("--------------\n
    --------------------Means and standard deviations of the variables that were used for scaling prior to implementing PCA------\n
    ------------------")
    print(pca.data$center)
    print(pca.data$scale)

    cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
    pause(duration = Inf)

    ####### 
    # Rotations
    cat("----------\n
    -------------Rotation matrix provides the principal component of the loadings.----------\n
    ------------")
    print(dim(pca.data$rotation))
    print(pca.data$rotation)

    cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
    pause(duration = Inf)

    ########
    cat("--------------\n
    ----------------x matrix provides the principal component of the scores.-----\n
    --------------")
    print(dim(pca.data$x))
    print((pca.data$x))

    cat("\n\n\n\n\n\n\n Press ENTER to continue \n\n\n\n\n\n")
    pause(duration = Inf)

    ################################
    ## Visualisation##


    ####
    cat("--------------\n
    ----------------Display 2 first components.-----\n
      --------------")
    biplot(pca.data, choices=c(componentx,componenty), scale=0)
    
    cat("-------------\n
    ---------------Interpretation:--------------\n
   If we display the first two dimensions of the PCA, we can see the loadings and the scores spread in the space of density as x-asis and PH-value as y axis.
   wE can also detect two outlies which seem to have very low alcohol and acidity rates. For a better representation, those outliers could be excluded.
   If we display the first and third demension dimensions of the PCA, we can see the loadings and the scores spread in the space of density as x-asis and Quality as y axis.
   We find, that density and sugary here go togheter while those features are rather quality neutral.-------\n
    ----------------")
    

    cat("\n\n\n\n\n\n\n Copy PCA into Global Dataset? \n\n\n
    Press ENTER to continue\
    Type 'stop' to stop \n\n\n\n\n\n")
    pause(duration = Inf)

    cat("--------------------------\n
    -------------------------------PCA is copied into Environment:---------------\n
    ---------------------------")
    pca.data<<-pca.data

}


pcafunction(data=white, 1,2)







