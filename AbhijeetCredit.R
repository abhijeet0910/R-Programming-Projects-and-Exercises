#  Loading the train and test data 
credit_trainet <- read.csv("/home/abhijeet/credit_train.csv")
credit_test <- read.csv("/home/abhijeet/credit_test.csv")
# combining the data to make changes simultaneously on train and test
credit <- as.data.frame(rbind(credit_trainet[,1:25], credit_test))
str(credit)
summary(credit)
# some exploratory data analysis 
plot(credit$EDUCATION)
hist(credit$EDUCATION)

# lets just try and subset the variables numerical and factors 
numeric_train = credit




set.seed(16027)
# Loading the required libraries
library(FSelector)
install.packages('ggthemes')
library(ggthemes2)
library(ggplot2)
install.packages("fscaret")
install.packages("fscaret", dependencies = c("Depends", "Suggests"))
library(fscaret)
library(rpart)
library(caret)
library(VIM)
install.packages("kknn")
library(kknn)
library(e1071)
# checking the missing values 

VIM::aggr(credit)
aggr(credit, delimiter = NULL, plot = TRUE)

# lets look at the severity of the imbalanced classes 
round(prop.table(table(credit_trainet$Default_nxt_Month))*100)

# we can see that 78% of our data has majority class as "0"
# which is not a good sign as predictions will get inaccurate 
# we would be able to predict the "0" class but we have to predict the minority class 
# balance the dataset 
install.packages("ROSE")
library(ROSE)
data.rose = ROSE(Default_nxt_Month ~ ., data = credit_trainet, seed = 1)$data
table(data.rose$Default_nxt_Month)

# rose is considered to give good result ... it has been seen.


corrplot::corrplot(cor(credit_trainet[,-c(1,2)]))
# doesnot seem to provide any information that we can make use of interms of deciding which varible to use 
summary(table(credit_trainet$Default_nxt_Month, credit_trainet$EDUCATION))
for(i in 8:13){
  credit[,i] <- as.factor(credit[,i])
}

# Train and Validatin set creation fust to see the important variables and create a baseline 
trainset <- credit_trainet[1:18000,]
testset <- credit_trainet[18001:20000,]
index <- sample(20000, 2000, replace = F)
trainset1 = data.rose[-c(index),]
testset1 = data.rose[c(index),]

# CART Model
model_rpart <- rpart(Default_nxt_Month ~ ., data = trainset, method = "class", control = rpart.control(cp = 0.0025, xval = 10))
pred_rpart <- predict(model_rpart, newdata = testset, type = "class")
confusionMatrix(pred_rpart,testset$Default_nxt_Month)
rattle::fancyRpartPlot(model_rpart)
# we can come to the conclusion that all the payment variables are very important and gives a maximum information PAYSTAT_SEP	PAYSTAT_AUG	PAYSTAT_JUL	PAYSTAT_JUN	PAYSTAT_MAY	PAYSTAT_APR


# Random Forest
library(randomForest)
trainset$Default_nxt_Month <- as.factor(trainset$Default_nxt_Month)
testset$Default_nxt_Month <- as.factor(testset$Default_nxt_Month)
model_rf <- randomForest(Default_nxt_Month ~ ., data = trainset, method = "classification", ntree = 1000)
pred_rf <- predict(model_rf, newdata = testset, type = "class")
confusionMatrix(pred_rf, testset$Default_nxt_Month)


# KKNN Model
model_kknn = kknn(Default_nxt_Month ~ ., trainset1[,-c(1:7)], testset1[,-c(1:7)], k = 94)
confusionMatrix(model_kknn$fitted.values, testset1$Default_nxt_Month)

# Find optimal k and weight kernel. 
knn_train = train.kknn(formula = Default_nxt_Month~.,data = trainset1[,-c(1:7)],kmax = 100, distance = 1, kernel = c("rectangular", "triangular", "epanechnikov", "gaussian","rank", "optimal"),scale = F)

summary(knn_train)
trainset1$Default_nxt_Month <- as.factor(trainset1$Default_nxt_Month)
testset1$Default_nxt_Month <- as.factor(testset1$Default_nxt_Month)
# Supply the optimal values for the final classification
fit.kknn = kknn(Default_nxt_Month~.,train = trainset1[,-c(1:7)],test = testset1[,-c(1:7)],k=knn_train$best.parameters$k, distance = 1,scale = F,kernel = knn_train$best.parameters$kernel)

# Check performance
confusionMatrix(fit.kknn$fitted.values, testset1$Default_nxt_Month)

# SVM model Tuning and Modeling
tunedModel=tune.svm(Default_nxt_Month~.,data=trainset[,-c(1:7)],gamma = 10^(-6:-1), cost=10^(1:2),class.weights =c(c("1"=0.9,"0"=0.1),c("1"=0.8,"0"=0.2),c("1"=0.7,"0"=0.3),c("1"=0.6,"0"=0.4)))

summary(tunedModel)

# SVM model with different parameters
model.svm.new=svm(Default_nxt_Month~.,data = trainset[,-c(1:7)],kernel="radial", scale=T,cost=10,gamma=0.001,class.weight=c("1"=0.7,"0"=0.3))

predict.svm.new=predict(model.svm.new,newdata = testset[,-c(1:7)])

classAgreement(table(predict.svm.new,testset$Default_nxt_Month))

caret::confusionMatrix(predict.svm.new,testset$Default_nxt_Month)



# Trying to model by balancing the dataset
newtrain <- cbind(credit[1:20000,], credit_trainet$Default_nxt_Month[1:20000])
names(newtrain)[40] <- "Default_nxt_Month"
onlydefault <- subset(newtrain, subset = Default_nxt_Month == 1)
others <- subset(newtrain, subset = Default_nxt_Month == 0)
index <- sample(15545, 4455, replace = F)
trainBalanced <- rbind(onlydefault, others[index,])

costmatrix <- matrix(c(0, 100, 10, 0), 2, 2, byrow=TRUE)
rownames(costmatrix) <- colnames(costmatrix) <- c("Not Default", "Default")

model_rpart <- rpart(Default_nxt_Month ~ ., data = trainset_newV[,-c(1,2)], method = "class", control = rpart.control(cp = 0.001, xval = 10), parms=list(split="gini", loss=matrix(c(0,50,110,0), byrow=TRUE, nrow=2)))
pred_rpart <- predict(model_rpart, newdata = testset_newV, type = "class")
confusionMatrix(pred_rpart,testset_newV$Default_nxt_Month)
rattle::fancyRpartPlot(model_rpart)

corrplot::corrplot(cor(trainBalanced[,-c(1:7)]))

# Random Forest
trainset_newV$Default_nxt_Month <- as.factor(trainset_newV$Default_nxt_Month)
model_rf <- randomForest(Default_nxt_Month ~ ., data = trainset_newV[,-c(1,2)], ntree = 1000, replace = TRUE, classwt = c("1"=0.8,"0"=0.2))
pred_rf <- predict(model_rf, newdata = testset_newV, type = "class")
confusionMatrix(pred_rf, testset_newV$Default_nxt_Month)


# SVM Model
model_svm = svm(Default_nxt_Month ~ ., data = trainset_newV[,-c(1:7, 14:26,30)], kernel = "radial", scale = T, cost = 50, gamma = 0.001, class.weight = c("1"=0.7,"0"=0.3), cross = 10)
predict_svm = predict(model_svm, newdata = testset_newV[,-c(1:7, 14:26,30)], type = "class")
confusionMatrix(predict_svm, testset_newV$Default_nxt_Month)


# The normal SVM gives a J-Statistic of 0.42. Hence we can go with that model for prediction.

prediction <- predict(model_svm, newdata = credit[20001:30000,])
submission <- cbind(credit_test[,1],prediction)
write.csv(submission, "prediction_final.csv")
