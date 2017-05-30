#### Hi, I've tried multiple models and my final model was built on Neural Networks. 
####Please consider Neural Network code. However, I haven't commented code of remaining models. 

rm(list=ls())

setwd("C://Users//kisho//Downloads")

bankdatatrain <- read.csv("data_train.csv",header = T,sep=",")
bankdatatest <-read.csv("data_test.csv",header = T,sep=",")
summary(bankdatatrain)
str(bankdatatrain)

#### Pre-Processing Steps 

bankdatatrain$ID <- NULL# removing the id column as it'll not add much information to the model
sum(is.na(bankdatatrain)) # no missing values

# seperating the o/p and i/p variables

output_variable <- bankdatatrain$TARGET
#output_variable <- as.factor(output_variable) # not  for NN
bankdatatrain$TARGET<- NULL

# standardizing the variables
library(vegan)
bankdatatrain = decostand(bankdatatrain, "range")
sum(is.na(bankdatatrain))
bankdatatrain[is.na(bankdatatrain)] <- 0 # as there is much less  or zero variance, for some columns  it returned Nans . So replacing them back

bankdatatrain$Target<-output_variable
bankdatafinal<-bankdatatrain
str(bankdatafinal$Target)

#####################################building a logistic model##############################
lgmout = glm(Target ~.,data=bankdatafinal,family = "binomial")
summary(lgmout)
#library(car)
#viftrain= vif(lgmout)
#library(MASS)
#step1=stepAIC(lgmout)
trainpredict=predict(lgmout,type ="response")
#trainpredict1=predict(lgmout)
summary(trainpredict)
trainpredictmatrix = ifelse(trainpredict>0.5,1,0)
t1=table(bankdatafinal$Target,trainpredictmatrix)
trainaccuracy=(t1[1,1]+t1[2,2])/sum(t1[1,]+t1[2,])
trainrecall = (t1[2,2])/sum(t1[1,2]+t1[2,2])
########## prediction of Logistic model on Test Data########################
## Pre-processing on test data
bankdatatest$ID <- NULL
# standardizing the variables
bankdatatest_deco = decostand(bankdatatest, "range")
sum(is.na(bankdatatest_deco))
bankdatatest_deco[is.na(bankdatatest_deco)] <- 0
bank_test <- bankdatatest_deco
testpredict= predict(lgmout,type="response",newdata =bank_test)
testpredictmatrix = ifelse(testpredict>0.5,1,0)
write.csv(testpredictmatrix, file = "testdata_predictions_logistic.csv")

#########################################################################################
############################# building a neural network #################################
#########################################################################################
library(caret)
set.seed(1234)
x <- bankdatafinal
x <-subset(bankdatafinal, select = -Target )
y <- bankdatafinal$Target
#y2<- subset(bankdatafinal, select = Target )
train.x = data.matrix(x)
train.y = y
require(mxnet)
mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(train.x, y, hidden_node=c(10), out_node=1, activation="tanh", out_activation="logistic",
                num.round=30, array.batch.size=100, learning.rate=0.06, momentum=0.9,
                eval.metric=mx.metric.accuracy)
Sys.time() -> end
paste(end - start)
##################### Prediction on Train Data##################################
preds_train = predict(model, train.x)
preds_train=t(preds_train)
pred_train.label = ifelse(preds_train<0.55, 0, 1)
conf.mat = table(pred_train.label, y);conf.mat
accuracy = sum(diag(conf.mat))/sum(conf.mat);accuracy
precision = conf.mat[2,2]/sum(conf.mat[2,]);precision
recall = conf.mat[2,2]/sum(conf.mat[,2]);recall
############################ Test Data #############################
## Pre-processing on test data
bankdatatest$ID <- NULL
# standardizing the variables
bankdatatest_deco = decostand(bankdatatest, "range")
sum(is.na(bankdatatest_deco))
bankdatatest_deco[is.na(bankdatatest_deco)] <- 0
####################Prediction on Test Data by Neural Network Model################
test.x = data.matrix(bankdatatest_deco)
preds_test = predict(model, test.x)
preds_test=t(preds_test)
pred_test.label = ifelse(preds_test<0.55, 0, 1)
pred_test.label=as.data.frame(pred_test.label)

colnames(pred_test.label) <- c("Predictions")
#a<- as.data.frame(t(pred_test.label))
write.csv(pred_test.label, file = "testdata_predictions_NN_0.55threshold.csv",row.names = FALSE)

####################### STEP-1 Visualisation#######################################
# 1. Relations b/w  Var15 and the Target 

#cor.test(bankdatafinal$var15,bankdatafinal$Target, method=c("kendall"))
cor(bankdatafinal$var15,bankdatafinal$Target, method = c("pearson", "kendall", "spearman"))

# Plotting the variation between the var15 and TargetVariable

library("ggpubr")
ggscatter(bankdatafinal, x = "var15", y = "Target", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Var15", ylab = "TargetVariable")

## from the above correlation and graph plot 
#we can infer that with increase in var15 the targetvariable decreases by 15.15

##2.Do you find any relationship between the Num_Var4 and the Target? 

cor(bankdatafinal$num_var4,bankdatafinal$Target, method = c("pearson", "kendall", "spearman"))

library("ggpubr")
ggscatter(bankdatafinal, x = "num_var4", y = "Target", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Var15", ylab = "TargetVariable")

## from the above correlation and graph plot 
#we can infer that with increase in num_var4 the targetvariable decreases by 10.8

#3.Understanding the distribution of mortgage. Is it normal ?

hist(bankdatafinal$var38)
var38_plot <-bankdatafinal$var38

# we can plot var38 as below 

h<-hist(var38_plot, breaks=10, col="red", xlab="var38plot", 
        main="Histogram with Normal Curve") 

xfit<-seq(min(var38_plot),max(var38_plot),length=40) 
yfit<-dnorm(xfit,mean=mean(var38_plot),sd=sd(var38_plot)) 
yfit <- yfit*diff(h$mids[1:2])*length(var38_plot) 
lines(xfit, yfit, col="blue", lwd=2)


# Kernel Density Plot
d <- density(bankdatafinal$var38) # returns the density data 
plot(d) # plots the results


##4. Most Repeated value of vat38 mortagage

tail(names(sort(table(bankdatafinal$var38))), 1)  # 0.0161654765146268


###############################################################################################################
                      ############### Using SVM MODEL####################
# converting o/p as Factor 

svm_train <-bankdatafinal

str(svm_train$Target)
svm_train$Target<- as.factor(svm_train$Target)
# Building Model
library(e1071)

Sys.time() -> start
tuned <- tune.svm(Target ~., data = svm_train, gamma = 10^(-3:1), cost = 2^(0:9)) # tune
Sys.time() -> end

summary(tuned)

############################################################################################
#################################Using Decision Trees C5.0###################################

dt_test <- bankdatatest
Target_var <- subset(bankdatafinal,select = c(Target))
Target_var$Target = as.factor(Target_var$Target)
Not_Target <-subset(bankdatafinal,select = -c(Target))

dt_train = cbind(Not_Target,Target_var)
library(C50)
#a. Build model
DT_C50 <- C5.0(Target~.,data=dt_train)
summary(DT_C50)
#b. Predict "Revenue" for train and test datasets
pred_Train_DT= predict(DT_C50,newdata=dt_train, type="class")
#c.Confusion Matrix on Train Data
C50_train_Conf_Matrix = table(dt_train$Target,pred_Train_DT);
C50_train_Conf_Matrix
#f. Compute the evaluation metric
accuracy_C50_train = round((sum(diag(C50_train_Conf_Matrix))/sum(C50_train_Conf_Matrix))* 100,2)
accuracy_C50_train

#########prediction on Test Data #######
pred_Test = predict(DT_C50, newdata=dt_test, type="class")
write.csv(pred_Test, file = "testdata_predictions_DT1.csv")
length(pred_Test)


##########################Using R-Part######################
#7. Build classification model using RPART
library(rpart)

#a. Build model
DT_rpart_class<-rpart(Target~.,data=dt_train,method="class")
printcp(DT_rpart_class)
DT_rpart_class
#summary(DT_rpart_class)

#b. Predict "Revenue" for train and test datasets
pred_Train_rpart = predict(DT_rpart_class,newdata=dt_train, type="class")

#c. Confusion Matrix on Train Data
Rpart_train_Conf_Matrix = table(dt_train$Target,pred_Train_rpart)
Rpart_train_Conf_Matrix 
#e. Confusion Matrix on Test Data
Rpart_test_Conf_Matrix = table(Cust_test$Revenue,pred_Test);Rpart_test_Conf_Matrix

#f. Compute the evaluation metric
accuracy_rpart_train = round((sum(diag(Rpart_train_Conf_Matrix))/sum(Rpart_train_Conf_Matrix))* 100,2)
accuracy_rpart_train
