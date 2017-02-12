
euc.dist <- function(testrow, trainrow) {
  col <- length(testrow)
  sum <- 0
  for (i in 1:col) {
    sum = sum + (testrow[i] - trainrow[i])^2
  }
  return(sqrt(sum))
}

getY <- function(kDistDF, y) {
  n <- nrow(kDistDF)
  kYDist <- rep(NA, n)
  for (i in 1:n) {
    rowIndex <- kDistDF[i,1]
    kYDist[i] <- y[rowIndex]
  }
  return(kYDist)
}

myknn <- function(xtrain, xtest, ytrain, k) {
  nTest <- nrow(xtest)
  nTrain <- nrow(xtrain)
  predicted <- rep(NA, nTest)
  for (i in 1:nTest) {
    eucDist <- rep(NA, nTrain)
    
    #use apply
    #compute euclidean distance to each row in xtrain
    for (j in 1:nTrain) {
      eucDist[j] <- euc.dist(xtest[i,],xtrain[j,]) 
    }
    print(eucDist)
    #We identify each x row in training set with the row number
    numberedRows <- c(1:nTrain)
    eucDistDF <- data.frame(numberedRows,eucDist)
    sortDistDF <- eucDistDF[order(eucDistDF$eucDist),]
    kDistDF <- sortDistDF[1:k,]
    #use original merged dataset, search for x, in that same row get the y value
    kYDist <- getY(kDistDF,y)
    print(kYDist)
    predicted[i] <- sum(kYDist)/k
    print(predicted[i])
  }
  
  return(predicted)
}

#for (i in 1:10) {
#  pred <- myknn(trainSet,xtest=valSet,ytrain=y,k=i)
#  print(pred)
#}

