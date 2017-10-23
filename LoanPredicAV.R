loan_train = read.csv("/home/abhijeet/Documents/Loan prediction analytics vidya/train_u6lujuX_CVtuZ9i.csv",na.strings = "")
loan_test = read.csv("/home/abhijeet/Documents/Loan prediction analytics vidya/test_Y3wMUE5_7gLdaTN.csv", na.strings = "")

# anova for whether the variability is explained by that variabl or not
summary(aov(formula = loan_train$CoapplicantIncome ~ loan_train$Loan_Status))
summary(aov(formula = loan_train$LoanAmount~ loan_train$Loan_Status))
summary(aov(formula = loan_train$ApplicantIncome~ loan_train$Loan_Status))
str(loan_test)
str(loan_train)
loan_marristra = subset(loan_train, subset = Married == "No" & Self_Employed == "Yes",select = c(3,6,8))
table(loan_train$Gender, loan_train$Married)
table(loan_train$Married)
table(loan_train$Gender)
table(loan_train$Dependents)
table(loan_train$Education)
table(loan_train$Self_Employed)


data_chi = loan_train[c(2,3,4,5,6,13)]
chisq.test(loan_train$Loan_Status,loan_train$Married) # variable is not highly dependent 
summary(table(loan_train$Loan_Status,loan_train$Married))
summary(table(loan_train$Loan_Status,loan_train$Dependents)) # variables are not highly dependent
str(loan_train)
summary(table(loan_train$Loan_Status,loan_train$Education))
table(loan_train$Loan_Amount_Term)
table(loan_train$Property_Area)
summary(fivenum(loan_train$ApplicantIncome)) # data is left skewed mean is greater than median 
boxplot(loan_train$ApplicantIncome)
library(ggplot2)
qplot(loan_train$ApplicantIncome)
qplot(loan_train$CoapplicantIncome)
#chisquares of variables
chisq.test(loan_train$Gender,loan_train$Loan_Status)
chisq.test(loan_train$Married,loan_train$Loan_Status)
chisq.test(loan_train$Dependents,loan_train$Loan_Status)
chisq.test(loan_train$Education,loan_train$Loan_Status)
chisq.test(loan_train$Self_Employed,loan_train$Loan_Status)
chisq.test(loan_train$Credit_History,loan_train$Loan_Status)
chisq.test(loan_train$Property_Area,loan_train$Loan_Status)
chisq.test(loan_train$Loan_Amount_Term, loan_train$Loan_Status)
#for feature selection i am using "F" selector  to see whether it matches the variables that i have selected or not 
library('FSelector')
f = paste("Loan_Status ~ ", paste(names(loan_train[,-c(1,13,11)]), collapse = " +"))
f1 <- cfs(formula = as.formula(f), data = loan_train)
f1
# knn imputation for misssing values library(VIM)
# with knn only blocks which has NA's will be imputed.

library(VIM)
loantrain_imputed1 = kNN(loan_train[,-13],k=13) # For imputation, K = # of Var +1 is taken
loantrain_imputed1 = loantrain_imputed1[,-c(13:24)]
# imputing test data
loantest_imputed1 = kNN(loan_test, k=13)
loantest_imputed1 = loantest_imputed1[,-c(13:24)]
# combine the data 
rbind_loandata = rbind(loan_train[-13],loan_test )
# again applying knn on the whole data 
loandata_imputed1 = kNN(rbind_loandata, k=13)
loandata_imputed1 = loandata_imputed1[,-c(13:24)]
# after imputing the whole data we again need to separate the train and the test data.
train_new = loandata_imputed1[1:614,]
test_new = loandata_imputed1[615:981,]

# cbind
train_new = cbind(train_new, loan_train$Loan_Status)
# so data preperation and data cleaning has been done now time to apply the algorithm to
# see how the metrics come 
names(train_new)
names(train_new)[13] <- "Loan_Status" 
names(test_new)
# install.packages("C50")
library(C50)
loan_model1=C5.0(formula = Loan_Status ~ ., data = train_new)
loan_model1
prediction = predict(loan_model1, newdata = test_new)
caret::confusionMatrix(prediction,test_new$type)
submis_data = test_new[1]
submis_data = cbind(submis_data, prediction)
write.csv(submis_data, "loan_predictionIII.csv") # first submissiom was made till here.

# now what i am trying to do here is i am gonna use the trainset and split the data in two to check my workings with different metrics like kappa and later i will try to tune it 
# by establishing my learning and assumptions from those values 
library(caTools)
set.seed(300)
train_split = sample.split(train_new$Loan_Status, SplitRatio = 0.7)
loan_trainsplit = subset(train_new, train_split==TRUE)
loan_testsplit  = subset(train_new, train_split==FALSE)
library(rpart)
library(rpart.plot)
library(rpart)
#install.packages("caret")
#install.packages("rattle") 
library(rattle)
library(caret)
# refer to sai's github pdf
# we need to set the seed 
set.seed(2609)
cnt = rpart.control(xval = 10, cp = 0.004)
model_loan = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History, data = loan_trainsplit,control = cnt)
pred_model_loan=predict(model_loan,newdata = loan_testsplit,type= "class")
rattle::fancyRpartPlot(model_loan)
accu_x = confusionMatrix(pred_model_loan, loan_testsplit$Loan_Status)
#CART Model
loan_model = rpart(Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History)
prp(loan_model)

# trying to test the model with different complexity parameter using for loop 
y = c(0)
for (i in seq(from = 0.001, to = 0.2000, by = 0.001)){
  cnt_i = rpart.control(xval = 10 , cp = i)
  model_loan = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History, data = loan_trainsplit,control = cnt_i)
  pred_model_loan=predict(model_loan,newdata = loan_testsplit,type= "class")
  rattle::fancyRpartPlot(model_loan)
  accu_x = confusionMatrix(pred_model_loan, loan_testsplit$Loan_Status)
  if(accu_x$overall["Accuracy"] < y)
    break()
  y = accu_x$overall["Accuracy"]
  }
y

loan_model1=C5.0(formula = Loan_Status ~ ., data = train_new)
loan_model1
prediction = predict(loan_model1, newdata = test_new)
caret::confusionMatrix(prediction,test_new$type)

# creating a new variable called emi to see whether it makes any difference 

train_new$EMI = train_new$LoanAmount * (.10) * (1+.10)^train_new$Loan_Amount_Term/((1+.10)^train_new$Loan_Amount_Term - 1)
test_new$EMI = test_new$LoanAmount * (.10) * (1+.10)^test_new$Loan_Amount_Term/((1+.10)^test_new$Loan_Amount_Term - 1)

train_new$Hand_income = train_new$ApplicantIncome + train_new$CoapplicantIncome - train_new$EMI

test_new$Hand_income = test_new$ApplicantIncome + test_new$CoapplicantIncome - test_new$EMI

train_split = sample.split(train_new$Loan_Status, SplitRatio = 0.7)
loan_trainsplitemi = subset(train_new, train_split==TRUE)
loan_testsplitemi  = subset(train_new, train_split==FALSE)
# checking whether the variable features in F selector or not
library('FSelector')
f = paste("Loan_Status ~ ", paste(names(train_new[,-c(1,13,11,12)]), collapse = " + "))
f1 <- cfs(formula = as.formula(f), data = train_new)
f1
# Again running rpart to see whather the new variable features in the decision tree or not 
y = 0
z = 0
for (i in seq(from = 0.001, to = 0.2000, by = 0.001)){
  cnt_i = rpart.control(xval = 10 , cp = i)
  model_loan_i = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History + EMI, data = loan_trainsplitemi, control = cnt_i)
  pred_model_loan_i=predict(model_loan_i,newdata = loan_testsplitemi,type= "class")
  accu_x_i = confusionMatrix(pred_model_loan_i, loan_testsplitemi$Loan_Status)
  if(accu_x_i$overall["Accuracy"] < y){
    break()
  }
  y = accu_x_i$overall["Accuracy"]
  z = i
}
y
z
rattle::fancyRpartPlot(model_loan)
# checking with normal complexity parameter 
set.seed(2609)
cnt = rpart.control(xval = 5, cp = 0.00001, minbucket = 15)
model_loan = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History + EMI, data = loan_trainsplitemi,control = cnt)
pred_model_loan=predict(model_loan,newdata = loan_testsplitemi,type= "class")
rattle::fancyRpartPlot(model_loan)
confusionMatrix(pred_model_loan, loan_testsplitemi$Loan_Status)
#CART Model
loan_model = rpart(Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History)
prp(loan_model)
x = (0.99 + 0.4138) - 1 
x
accu_x = confusionMatrix(pred_model_loan, loan_testsplit$Loan_Status)
accu_x
 x = accu_x$overall["Sensitivity"] + accu_x$overall["Specificity"]
 x
 # using a diffeent statistic to see the model
 y = 0
 z = 0
for(i in seq(from = 0.0001, to = 0.0100, by = 0.0005)){
   model_loan_i = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History + EMI, data = loan_trainsplitemi, control = rpart.control(xval = 5, cp = i))
   pred_model_loan_i=predict(model_loan_i,newdata = loan_testsplitemi,type= "class")
   accu_x_i = confusionMatrix(pred_model_loan_i, loan_testsplitemi$Loan_Status)
   print(i)
   print(accu_x_i$byClass["Specificity"])
   print(accu_x_i$byClass["Sensitivity"])
   rm(accu_x_i,model_loan_i,pred_model_loan_i)
}
rm(i)
 y
 z
 # for  accu_x1
 set.seed(2609)
 cnt = rpart.control(xval = 5, cp = 0.006)
 model_loan = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History + Hand_income, data = train_new,control = cnt)
 pred_model_loan=predict(model_loan,newdata = test_new,type= "class")
 rattle::fancyRpartPlot(model_loan)
 pred_model_loan
 # for uploading again 
 submis_data4 = test_new[1]
 submis_data4 = cbind(submis_data4, pred_model_loan)
 write.csv(submis_data4, "loan_predictionIII4.csv")
 # using on the original dataset for second submission
 y = 0
 z = 0
 for (i in seq(from = 0.001, to = 0.2000, by = 0.001)){
   cnt_i = rpart.control(xval = 10 , cp = i)
   model_loan_i = rpart(formula = Loan_Status ~ Married + Education + LoanAmount + Property_Area + Credit_History + EMI, data = train_new, control = cnt_i)
   pred_model_loan_i=predict(model_loan_i,newdata = test_new,type= "class")
   accu_x_i = confusionMatrix(pred_model_loan_i, test_new$Loan_Status)
   if((accu_x1_i$byClass["Sensitivity"] + accu_x1_i$byClass["Specificity"])< y){
     z = i
     break()
   }
   y = accu_x1_i$byClass["Sensitivity"] + accu_x1_i$byClass["Specificity"]
   
 }
 y
 z
 # trying another way for cp
 library(caret)
 library(e1071)
 numFolds = trainControl( method = "cv", number = 5 , repeats = 5,sampling = "up")
 #cpGrid = expand.grid( .cp = seq(0.0001,0.01,0.0005)) 
 
train(Loan_Status ~ Education + LoanAmount + Property_Area + Credit_History + Hand_income, data = train_new, method = "gbm", trControl = numFolds, n.trees = 100, interaction.depth = 3) #, tuneGrid = cpGrid )
 
 # random forest 
 install.packages("randomForest")
 library(randomForest)
LoanForest = randomForest(Loan_Status ~ Education + LoanAmount + Property_Area + Credit_History+ Hand_income , data = train_new, ntree=200, mtry = 3, maxnodes = 3, nodesize = 3)
LoanForest
pred_model_loan <- predict(LoanForest, newdata = test_new)
 # second submission 
 submis_data2 = test_new[1]
 submis_data2 = cbind(submis_data2, pred_model_loan)
 write.csv(submis_data2, "loan_predictionIII2.csv") # first submissiom was made till here.
 
 # Running Anova with hand_income
 summary(aov(formula = train_new$Hand_income ~ loan_train$Loan_Status))
 