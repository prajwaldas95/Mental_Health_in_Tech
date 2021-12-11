#imports
install.packages("poLCA", dependencies = TRUE)
library(poLCA)

#constants
sigLev = 3
percentMul = 100
options(warn=-1) #turns off warnings

clusterFrame = read.csv("data_with_encoding_new.csv")



#make formula
givenForm = cbind(A,b,c,d,e,f,g,h,I,j,k,l,m,n,o,q,p,r,s,T)~1
initMod.lcm = poLCA(givenForm,
                    data = clusterFrame,nclass = 2,verbose = FALSE)







inferenceRows = sample(1:dim(clusterFrame)[1],
                       round(dim(clusterFrame)[1] / 2))

inferenceSet = clusterFrame[inferenceRows,]
selectionSet = clusterFrame[-inferenceRows,]

write.csv(inferenceSet,"clusterData_inference.csv",
          row.names = FALSE)
write.csv(selectionSet,"clusterData_selection.csv",
          row.names = FALSE)

 

#all the columns are encoded and stored in a dictionary.

form = cbind(A,b,c,d,e,f,g,h,I,j,k,l,m,n,o,q,p,r,s,T)~1
getLogLikeCV <- function(numClasses,givenFrame,numFolds = 5){
    #helper for getting cross-validated log-likelihood a dataset given a
    #particular latent class model
    #make folds
    givenFrame$fold = sample(1:numFolds,dim(givenFrame)[1],replace = TRUE)
    cvVec = rep(0,numFolds)
    #then run through folds
    for (foldLev in 1:numFolds){
        #make cv slit
        trainSet = givenFrame[which(givenFrame$fold != foldLev),]
        testSet = givenFrame[which(givenFrame$fold == foldLev),]
        #train on trainSet
        newMod.lcm = poLCA(form,data = trainSet,nclass = numClasses,
                           verbose = FALSE)
        #get log-likelihood on test set
       # cvVec[foldLev] = getLogLike(newMod.lcm,testSet)
    }
    return(mean(cvVec))
}
#then try to get cv vec
numClassesConsidered = 10
consideredClassSizes = 1:numClassesConsidered
cvVec = rep(0,numClassesConsidered)
for (lev in consideredClassSizes){
    cvVec[lev] = getLogLikeCV(lev,selectionSet)
}



finalNumClasses = 3
finalMod.lcm = poLCA(form,inferenceSet,nclass = finalNumClasses,
                     verbose = FALSE)
print(finalMod.lcm)





