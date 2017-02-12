#part 1
spam_test <- read.csv("~/ProgramZ/stat365/HW2/spam_test.csv")
spam_train <- read.csv("~/ProgramZ/stat365/HW2/spam_train.csv")

require(FNN)

nacol_name <- "capital_run_length_average"
nacol_test <- which(colnames(spam_test)==nacol_name)
col_resp <- which(colnames(spam_train)=="spam")
nacol_train <- which(colnames(spam_train)==nacol_name)

#scale both test and train data, and remove the response variable
scale_test <- spam_test
scale_train <- spam_train
scale_test[,-nacol_test] <- as.data.frame(scale(scale_test[,-nacol_test]))
scale_train[,-c(nacol_train,col_resp)] <- as.data.frame(scale(scale_train[,-c(nacol_train,col_resp)]))
scale_train$spam <-NULL

#get row in test for which there are na values, run knn and replace the na values in original dataset
na_r <- which(is.na(spam_test$capital_run_length_average))
test<-knn.reg(scale_test[-na_r,-nacol_test],test=scale_test[na_r,-nacol_test],y=scale_test[-na_r,nacol_test],k=15)
spam_test[na_r,nacol_test] <- test$pred

#do the same for train
na_r2 <- which(is.na(spam_train$capital_run_length_average))
train<-knn.reg(scale_train[-na_r2,-nacol_train],test=scale_train[na_r2,-nacol_train],y=scale_train[-na_r2,nacol_train],k=15)
spam_train[na_r2,nacol_train] <- train$pred

#We will be calling the unscaled data with filled in na values spam_train and spam_test

#part 3

spam <- spam_train$spam
##first
spam_train_first <- spam_train
spam_test_first <- spam_test
spam_train_first$spam <- NULL
#get rid of captital_run_length_average predictor
spam_train_first$capital_run_length_average <- NULL
spam_test_first$capital_run_length_average <- NULL
knn_pred1 <- knnclass(spam_train_first,spam_test_first,spam)

##second
spam_train_second <- spam_train
spam_test_second <- spam_test
spam_train_second$spam <- NULL
knn_pred2 <- knnclass(spam_train_second,spam_test_second,spam)
#if k = 15, this is the "model" answer, w/o scaling
#knn_pred22 <- as.vector(FNN::knn(spam_train_second,spam_test_second,as.factor(spam), k=15))

##third (Logistic regression without capital run length ave) 
spam_train_third <- spam_train
spam_test_third <- spam_test
spam_train_third$capital_run_length_average <- NULL
spam_test_third$capital_run_length_average <- NULL
m3 <- glm(spam~.,family=binomial,data=spam_train_third)
logm_pred1 <- predict(m3, newdata=spam_test_third, type="response")

##fourth (Logistic regression with capital run length ave)
spam_train_fourth <- spam_train
spam_test_fourth <- spam_test
m4 <- glm(spam~.,family=binomial,data=spam_train_fourth)
logm_pred1 <- predict(m4, newdata=spam_test_fourth, type="response")

#In 3-4 sentences, provide a quick summary of your second logistic regression 
#model. Which predictors appeared to be most significant? 
#Are there any surprises in the predictors that ended up being significant or 
#not significant?

