##Credit Risk Modeling using Logistic Regression


# Reading & Understanding the Data
traindata=read.csv("Credit_Risk_Train_data.csv")
vdata=read.csv("Credit_Risk_validate_data.csv")

names(traindata)
names(vdata)

#changing names
library(data.table)
setnames(vdata,"outcome","Loan_Status")


names(vdata)

str(traindata)
str(vdata)

summary(traindata)
summary(vdata)

#removing NA values
library(DMwR)
centralImputation(data = traindata)->train
summary(train)
sum(is.na(train))

centralImputation(data = vdata)->validate
summary(validate)
sum(is.na(validate))

levels(train$Gender)
#Building the Logistic Model and checking the model summary
model= glm(Loan_Status~ Credit_History+Property_Area,data = train,family = "binomial")
model
summary(model)
plot(model)
#predition
pred=predict(model,newdata = validate,type = "response")
pred
table(pred)
mean(pred)

#finding Accuracy
#basline model
table(validate$Loan_Status)

#Measuring the accuracy of the model
table(validate$Loan_Status, as.numeric(pred >= 0.5))
error=(19+1)/367
(error)*100
Accu=(1-error)*100
Accu

#Measuring the accuracy of the model using MLmetrics
library(MLmetrics)
validate$predict = ifelse(pred<mean(pred),"N","Y")
MLmetrics::Accuracy(validate$predict,validate$Loan_Status)*100

library(gplots)
library(ROCR)
library(MASS)

#stepAIC

stepAIC(model)->log_step
log_step

#VIF and multi-collinearity

library(car)

vif(log_step)->log_vif
log_vif

predict(log_step,type = "response")->predct
predct

pred <- prediction(predct, train$Loan_Status)
pred

perf <- performance(pred, measure="tpr", x.measure="fpr")
perf

#plot.new(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
perf_auc <- performance(pred, measure="auc")
perf_auc

perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
train$Loan_Status


#####using Confusion Matrix#####
prob_test <- predict(model, validate, type = "response")
preds_test <- ifelse(prob_test > 0.1, "yes", "no")
table(preds_test)

test_data_labs <- validate$Loan_Status
test_data_labs

conf_matrix <- table(test_data_labs, preds_test)
print(conf_matrix)

#Calculating the Specificity
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
specificity

#calculating the Sensitivity
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
sensitivity

#Calculating the Accuracy
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
print(accuracy)

