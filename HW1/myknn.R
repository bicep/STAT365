euc.dist <- function(testrow,traindata) {
  sum <- apply(((as.list(testrow)-traindata)^2),1,sum)
  return(sqrt(sum))
}

getY <- function(kDistDF, y) {
  n <- nrow(kDistDF)
  kYDist <- rep(NA, n)
  kYDist <- y[kDistDF[,1]]
  return(kYDist)
}

myknn <- function(xtrain, xtest, ytrain, k) {
  nTest <- nrow(xtest)
  predicted <- vector('numeric')
  get.predic <- function(xtestrow,xtrain,ytrain,k) {
    eucDist<-as.vector(euc.dist(xtestrow,xtrain))
    numberedRows <- c(1:nrow(xtrain))
    eucDistDF <- data.frame(numberedRows,eucDist)
    sortDistDF <- eucDistDF[order(eucDistDF$eucDist),]
    kDistDF <- sortDistDF[1:k,]
    
    #use original merged dataset, search for x, in that same row get the y value
    kYDist <- getY(kDistDF,y)
    p<-sum(kYDist)/k
    predicted <- c(predicted,p)
  }
  predicted <- apply(xtest,1,get.predic,xtrain=xtrain,ytrain=ytrain,k=k)
  return(as.vector(predicted))
}

