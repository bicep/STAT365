knnclass <- function(xtrain, xtest, ytrain) {
  
  #standardize training and test using training set only
  trainMeans <- apply(xtrain, 2, function(y) mean(y))
  trainSD <- apply(xtrain, 2, function(y) sd(y))
  xtest <- (xtest-as.list(trainMeans))/as.list(trainSD)
  xtrain <- (xtrain-as.list(trainMeans))/as.list(trainSD)

  #split training into training and validation
  #set.seed(123)
  ntrain <- nrow(xtrain)
  s <- sample(1:ntrain, (4*ntrain)/5, replace = FALSE)
  trainSet <- xtrain[s,]
  trainY <- ytrain[s]
  valSet <- xtrain[-s,]
  valY <- ytrain[-s]
  testSet <- xtest

  #euc dist to each row in test
  euc.dist <- function(testrow,traindata) {
    sum <- apply(((as.list(testrow)-traindata)^2),1,sum)
    return(sqrt(sum))
  }
  
  #only need to do this once
  getDistMatrix <- function(xtestrow,xtrain) {
    eucDist<-as.vector(euc.dist(xtestrow,xtrain))
    numberedRows <- c(1:nrow(xtrain))
    eucDistDF <- data.frame(numberedRows,eucDist)
    sortDistDF <- eucDistDF[order(eucDistDF$eucDist),]
    return(sortDistDF)
  }
  #Sorted distance vector
  sortDistDFV <- apply(valSet,1,getDistMatrix,xtrain=trainSet)

  #function that gets the classification for each testrow
  getClassfn <- function(kDistDF, y) {
    kYClass <- y[kDistDF[,1]]
    class <- as(names(which.max(table(kYClass))), Class=mode(y))
    return(class)
  }
  
  #classification
  kclass <- function(sortDistDFV,y,k) {
    get.predic <- function(sortDistDF,y,k) {
      ###You only need to compute distance matrix once!!
      sortDistDF <- (sortDistDF)
      kDistDF <- sortDistDF[1:k,]
      #get the corresponding y 
      #use original merged dataset, search for x, in that same row get the y value
      class <- getClassfn(kDistDF,y)
      return (class)
    }
    #predicted <- (mapply(get.predic,sortDistDFV,y,k))
    predicted <- lapply(sortDistDFV,get.predic,y=y,k=k)
    predicted <- (as.vector(unlist(predicted)))
    #predicted <- apply(testSet,1,get.predic,y=y,k=k,sortDistDFV=sortDistDFV)
    return (predicted)
  }
  
  #misclassfication error rate
  mer <- function(pred,actual) {
    matchv <- mapply(function(x,y) {ifelse ((x==y),0,1)}, pred, actual,SIMPLIFY=TRUE)
    return(sum(matchv)/length(pred))
  }
  
  #first apply to validation set for a range of k, and measure error
  nK <- 15
  kCount <- c(2:nK)
  kDF <- data.frame(kCount,rep(NA,nK-1))
  colnames(kDF) <- c("K","MER")
  for (i in 2:nK) {
    pred <- kclass(sortDistDFV,y=trainY,k=i)
    e <- mer(pred,valY)
    kDF$MER[i-1] <- e
  }
  kDF <- kDF[order(kDF$MER),]
  
  #opt k is the one with lowest MER
  optK <- kDF$K[1]
  sortDistDFV_final <- apply(testSet,1,getDistMatrix,xtrain=xtrain)
  finalPred <- kclass(sortDistDFV_final,ytrain,optK)
}