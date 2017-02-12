#STEP1-Merge data with weather and prepare test/val sets
new_training <- merge(citibike_train,weather,by="date")
y <- new_training$trips
#Transform data to get average temperature
new_training$TAVE <- (new_training$TMAX+new_training$TMIN)/2
original_training <- new_training
new_training <- new_training[,c(-1,-2,-4)]
new_test <- merge(citibike_test,weather,by="date")
new_test$TAVE <- (new_test$TMAX+new_test$TMIN)/2
testSet <- new_test[,c(-1,-3)]
#randomly split the training data into training and validation 
set.seed(123)
ntrain <- nrow(new_training)
s <- sample(1:ntrain, (3*ntrain)/4, replace = FALSE)
trainSet <- new_training[s,]
valSet <- new_training[-s,]

#STEP2-knn
#mse
mse <- function(pred,actual) {
  e <- mean((pred-actual)^2)
  return(e)
}
require(FNN)
nK <- 750
kCount <- c(1:nK)
kDF <- data.frame(kCount,rep(NA,nK))
colnames(kDF) <- c("K","MSE")
for (i in 1:nK) {
  pred <- (knn.reg(trainSet,test=valSet,y=y,k=i))$pred
  e <- mse(pred,citibike_train[-s,]$trips)
  kDF$MSE[i] <- e
}
kDF <- kDF[order(kDF$MSE),]
View(kDF)
#k we will use to get min mse:
k <- kDF[1,]$K

#STEP3-linear regression
trainSet$trips <- citibike_train[s,]$trips
valSet$trips <- citibike_train[-s,]$trips
#try a number of different models:
formu <- c("trips~SNOW+TAVE","trips~SNWD+TAVE","trips~PRCP+TAVE","trips~SNOW+SNWD",
           "trips~SNOW+TAVE+AWND","trips~SNOW+TAVE+n_stations",
           "trips~PRCP+SNOW+TAVE","trips~SNOW+SNWD+TAVE",
           "trips~SNOW+SNWD+AWND","trips~PRCP+TAVE+AWND",
           "trips~PRCP+SNOW+SNWD+TAVE","trips~PRCP+SNOW+SNWD+TAVE+AWND")
lDF <- data.frame(formu,rep(NA,length(formu)))
colnames(lDF) <- c("formula","MSE")
for (i in 1:length(formu)) {
  m1 <- lm(as.formula(formu[i]), data=trainSet)
  p1.new <- predict(m1,newdata=valSet)
  p1.new.c <- predict(m1, newdata=valSet, interval="confidence", level=0.95) 
  p1.new.p <- predict(m1, newdata=valSet, interval="prediction", level=0.95)
  e <- mean((p1.new-valSet$trips)^2)
  lDF$MSE[i] <- e
}
lDF <- lDF[order(lDF$MSE),]
View(lDF)

#STEP4-Choose linear model (because of lower MSE) and predict for test set
#refit on entire training set
mfinal <- lm(trips~SNOW+TAVE,data=original_training)
pfinal <- predict(mfinal,newdata=testSet)
finalDF <- data.frame(new_test$date,pfinal)
colnames(finalDF) <- c("date","trips")
