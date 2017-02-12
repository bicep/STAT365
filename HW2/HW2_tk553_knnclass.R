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
  
  #function that calculates euc dist to each row in test. Used in the function below
  euc.dist <- function(testrow,traindata) {
    sum <- apply(((as.list(testrow)-traindata)^2),1,sum)
    return(sqrt(sum))
  }
  
  #function that gets the distance matrix for ONE row in test to all rows in train
  getDistMatrix <- function(xtestrow,xtrain) {
    eucDist<-as.vector(euc.dist(xtestrow,xtrain))
    numberedRows <- c(1:nrow(xtrain))
    eucDistDF <- data.frame(numberedRows,eucDist)
    sortDistDF <- eucDistDF[order(eucDistDF$eucDist),]
    return(sortDistDF)
  }
  
  #Sorted distance list containing distance matrix for each test row. Only need to do this once, each calculation from k=2 to 15 will use the same matrix. test set is the validation set.
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
      sortDistDF <- (sortDistDF)
      kDistDF <- sortDistDF[1:k,]
      class <- getClassfn(kDistDF,y)
      return (class)
    }
    #lapply because sortDistDFV is a list. (apply returns list).
    predicted <- lapply(sortDistDFV,get.predic,y=y,k=k)
    predicted <- (as.vector(unlist(predicted)))
    return (predicted)
  }
  
  #function to calculate misclassfication error rate
  mer <- function(pred,actual) {
    matchv <- mapply(function(x,y) {ifelse ((x==y),0,1)}, pred, actual,SIMPLIFY=TRUE)
    return(sum(matchv)/length(pred))
  }
  
  #We get the misclassfication error rate for each k 2:15. 
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
  
  #optimal k is the one with lowest MER, first item in the ordered vector.
  optK <- kDF$K[1]
  #Now get the distance matrix list for the actual test set. 
  sortDistDFV_final <- apply(testSet,1,getDistMatrix,xtrain=xtrain)
  finalPred <- kclass(sortDistDFV_final,ytrain,optK)
}