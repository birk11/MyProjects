rm(list = ls())
setwd("C://Users//kisho//Downloads")
dataset=read.csv("DataPatient_Adherence_Batch24_sorted.csv",header=T,sep=",")
dataset$Date=NULL
dataset$Delay3=NULL
dataset$ADHERENT<-NULL
# dataset$PatientID<-NULL
# dataset$Medication<-NULL
summary(dataset)
str(dataset)
dataset$Adherentnew=as.factor(dataset$Adherentnew)
dataset$ActGPI=as.factor(dataset$ActGPI)
k=1;j=1;
#dataset$unique = dataset$PatientID-dataset$Medication
testdata=data.frame()
traindata=data.frame()
sum(is.na(dataset))
t=nrow(dataset);j=1


dataset$PMID = paste(dataset$PatientID,dataset$Medication, sep="_")
dataset$unique <- NA

## loop for NoOfMonths
count=1;j=1
for(j in 1:t){
  if(j==1){
    dataset[j,]$unique=1
    count=count+1
  }
  else if(dataset[j,]$PMID==dataset[j-1,]$PMID){
    dataset[j,]$unique<-count
    count=count+1
  }
  else{
    
    dataset[j,]$unique<-1
    count=2
  }
}

#Loop for Test and Train Split
t=nrow(dataset);j=1

  for(j in 1:t){
    if(j==t && dataset[j,2]==dataset[j-1,2]){
      testdata=rbind(testdata,dataset[j,])  
    }
    
    else if(dataset[j,2]!=dataset[j+1,2] && dataset[j,2]!=dataset[j-1,2])
    {traindata=rbind(traindata,dataset[j,])}
    else if(dataset[j,2]==dataset[j+1,2])
    {
      traindata=rbind(traindata,dataset[j,])
      k=k+1;
      k;
    }
    else{testdata=rbind(testdata,dataset[j,])}
  }  
    
str(traindata)
library(caTools)
library(DMwR)

traindata$PatientID<-NULL
traindata$Medication<-NULL
traindata$PMID<-NULL
names(traindata)[12]<-"ADHERENT"

testdata$PatientID<-NULL
testdata$Medication<-NULL
testdata$PMID<-NULL
names(testdata)[12]<-"ADHERENT"
str(traindata)
traindata$Date = NULL
testdata$Date = NULL


lgmout = glm(ADHERENT ~.,data=traindata,family = "binomial")
summary(lgmout)
#viftrain= vif(lgmout)
#library(MASS)
#step1=stepAIC(lgmout)

trainpredict_logistic = predict(lgmout,type="response")

trainmatrix_logistic  = ifelse(trainpredict_logistic>0.5,1,0)

t1_log=table(actual=traindata$ADHERENT,preds=trainmatrix_logistic)
trainaccuracy_log=(t1_log[1,1]+t1_log[2,2])/sum(t1_log[1,]+t1_log[2,]);trainaccuracy_log
trainrecall_log = (t1_log[2,2])/sum(t1_log[2,1]+t1_log[2,2]);trainrecall_log
train_spec_log = (t1_log[1,1])/sum(t1_log[1,1]+t1_log[1,2]);train_spec_log
testpredict_log <- NULL
testpredict_log= predict(lgmout,type="response",newdata = testdata)
testmatrix_log  = ifelse(testpredict_log>0.5,1,0)
t2_log=table(actuals=testdata$ADHERENT,preds=testmatrix_log)
testaccuracy_log=(t2_log[1,1]+t2_log[2,2])/sum(t2_log[1,]+t2_log[2,]);testaccuracy_log
testrecall_log = (t2_log[2,2])/sum(t2_log[2,1]+t2_log[2,2]);testrecall_log
testspecificity_log=(t2_log[1,1])/sum(t2_log[1,1]+t2_log[1,2]);testspecificity_log

#########################################################################
# Removing the Date column from DataSet and Training the model
# library(randomForest)
# 
# RFtestdata <- testdata
# RFtraindata <- traindata
# 
# RFtraindata$PatientID<-NULL
# RFtraindata$Medication<-NULL
# RFtraindata$PMID<-NULL
# str(RFtraindata)

#RFOut <- randomForest(ADHERENT~., data=RFtraindata,ntree=100,do.trace=20)

##################################################################################
############################ Decision Trees #####################################
library(rpart)
library(rpart.plot)

RFtestdata <- testdata
RFtraindata <- traindata

RFtestdata$PatientID<-NULL
RFtestdata$Medication<-NULL
RFtestdata$PMID<-NULL

Model_rpart= rpart(ADHERENT~., data=RFtraindata, method="class")
Model_rpart
plot(Model_rpart)
#rpart.plot(Model_rpart,type=3,extra=103,fallen.leaves = FALSE)

P1_train_rpart=predict(Model_rpart,RFtraindata,type="class")
t9_log=table(RFtraindata[,12],predicted=P1_train_rpart)

trainaccuracy_DT=(t9_log[1,1]+t9_log[2,2])/sum(t9_log[1,]+t9_log[2,]);trainaccuracy_DT

trainrecall_DT = (t9_log[2,2])/sum(t9_log[2,1]+t9_log[2,2]);trainrecall_DT
train_spec_DT = (t9_log[1,1])/sum(t9_log[1,1]+t9_log[1,2]);train_spec_DT

#Predicting on Test
P1_test_rpart=predict(Model_rpart,RFtestdata,type="class")
t10_log=table(RFtestdata[,12],predicted=P1_test_rpart)
testaccuracy_DT=(t10_log[1,1]+t10_log[2,2])/sum(t10_log[1,]+t10_log[2,]);testaccuracy_DT
testspecificity_DT=(t10_log[1,1])/sum(t10_log[1,1]+t10_log[1,2]);testspecificity_DT
testrecall_DT = (t10_log[2,2])/sum(t10_log[2,1]+t10_log[2,2]);testrecall_DT
#########################################################################
######PLOTS############
attach(dataset)

boxplot(ActGPI~PatientID, main="PatientID VS ActGPI", 
        xlab="PatientID", ylab="ActGPI")

plot(PatientID,ActGPI)

library(ggplot2)
###########################################################################

##############################SVM Algorithm#############################
# svm_dataset=read.csv("DataPatient_Adherence_Batch24_sorted.csv",header=T,sep=",")
# svm_dataset_TEMP=read.csv("DataPatient_Adherence_Batch24_sorted.csv",header=T,sep=",")
# svm_dataset$PatientID<-NULL
# svm_dataset$Medication<-NULL
# str(svm_dataset)
# svm_dataset$ADHERENT<-NULL
# names(svm_dataset)[17]<-"ADHERENT"


svm_dataset <-dataset
svm_dataset$ADHERENT<-NULL
names(svm_dataset)[14]<-"ADHERENT"
svm_dataset$ADHERENT <- as.factor(svm_dataset$ADHERENT)

## removing PatientID,Medication as we've Unique(patientID+MedicationID)
svm_dataset$PatientID<-NULL
svm_dataset$Medication<-NULL
svm_dataset$PMID<-NULL
svm_dataset$Date<-NULL
svm_dataset$ActGPI <- as.factor(svm_dataset$ActGPI)
str(svm_dataset)
#svm_dataset$PMID <- as.factor(svm_dataset$PMID)
svm_target <-svm_dataset$ADHERENT
svm_unique <- svm_dataset$unique
svm_dataset$PatientID<-NULL
svm_dataset$unique<-NULL
svm_dataset$PMID <- NULL
svm_dataset$unique<-dataset$unique
str(svm_dataset)

svm_categ<-subset(svm_dataset,select=-c(unique,AmountPaid,For_How_Many_Days,QTY,Age,ADHERENT))
svm_numeric<-subset(svm_dataset,select=c(AmountPaid,For_How_Many_Days,QTY,Age))

###dummies for numeric
library("dummies") 
svm_categ_dummy = dummy.data.frame(svm_categ)
## standardize
library(vegan)
svm_numeric_standardize = decostand(svm_numeric,"range")
### C binding Numeric, Categorical ,target
svm_final_dataset<- cbind(svm_categ_dummy,svm_numeric_standardize)
svm_final_dataset$target<-svm_target
svm_final_dataset$unique<-svm_unique
#### split the dataset into Train and Test
svm_testdata <- data.frame(NULL)
svm_traindata<- data.frame(NULL)
svm_final_dataset$PMID<-dataset$PMID

j=1;k=1
for(j in 1:t){
  if(j==t && svm_final_dataset[j,]$PMID==svm_final_dataset[j-1,]$PMID){
    svm_testdata=rbind(svm_testdata,svm_final_dataset[j,])  
  }
  
  else if(svm_final_dataset[j,]$PMID!=svm_final_dataset[j+1,]$PMID && svm_final_dataset[j,]$PMID!=svm_final_dataset[j-1,]$PMID){
    svm_traindata=rbind(svm_traindata,svm_final_dataset[j,])
  }
  else if(svm_final_dataset[j,]$PMID==svm_final_dataset[j+1,]$PMID)
  {
    svm_traindata=rbind(svm_traindata,svm_final_dataset[j,])
    k=k+1;
    k;
  }
  else{
    svm_testdata=rbind(svm_testdata,svm_final_dataset[j,])
  }
}
# sum(is.na(svm_traindata$target))
svm_traindata$PMID<-NULL
svm_testdata$PMID<-NULL
library(e1071)
x = subset(svm_traindata, select = -target) #remove response variable
y = svm_traindata$target

sum(is.na(y))
model  =  svm(x = x, y = y, type = "C-classification", kernel = "linear", cost = 10)
summary(model)

# Predict on train data using the model
pred_train  =  predict(model, x) # x is all the input variables
table(pred_train)

# Build confusion matrix ("loan-takers":1; "non loan-takers":0)
# compare actual (i.e. "y") vs. predicted (pred_train)
tb_train = table(actual=y,pred=pred_train)#actual is on left and predicted shown on top

# Calculate error metrics
accuracy_train = sum(diag(tb_train))/sum(tb_train); accuracy_train
recall_train = (tb_train[2,2]/(tb_train[2,2]+tb_train[2,1]));recall_train
spec_train_svm=(tb_train[1,1]/(tb_train[1,1]+tb_train[1,2]));spec_train_svm

# Predict on test data
svm_testdata$PMID<-NULL
a = subset(svm_testdata, select = -target) #remove response variable
b = svm_testdata$target
pred_test = predict(model, a)
table(pred_test)

#Build confusion matrix
tb_test <- table(b,pred_test)
accuracy_test = sum(diag(tb_test))/sum(tb_test);accuracy_test
recall_test = (tb_test[2,2]/(tb_test[2,2]+tb_test[2,1]));recall_test
specif_svm=(tb_test[1,1]/(tb_test[1,1]+tb_test[1,2]));specif_svm


###########################################################################################
############################# Neural networks ######################################
NN_traindata <- svm_traindata
NN_testdata <- svm_testdata

#Bank$outcome <- as.numeric(as.character(Bank$outcome))
NN_traindata$target <- as.numeric(as.character(NN_traindata$target))

NN_testdata$target <- as.numeric(as.character(NN_testdata$target))



library(caret)
set.seed(1234)


NN_train.x = data.matrix(NN_traindata[, -193])
NN_train.y = NN_traindata[, 193]
NN_test.x = data.matrix(NN_testdata[, -193])
NN_test.y = NN_testdata[, 193]


require(mxnet)
mx.set.seed(0)
Sys.time() -> start
model <- mx.mlp(NN_train.x, NN_train.y, hidden_node=c(10), out_node=1, activation="tanh", out_activation="logistic",
                num.round=20, array.batch.size=200, learning.rate=0.09, momentum=0.6,
                eval.metric=mx.metric.accuracy)
Sys.time() -> end
paste(end - start)


preds_NN = predict(model, NN_train.x)

preds_NN=t(preds_NN)
pred.label = ifelse(preds_NN<0.55, 0, 1)

conf.mat_NN = table(pred.label, NN_train.y);conf.mat_NN
accuracy_NN = sum(diag(conf.mat_NN))/sum(conf.mat_NN);accuracy_NN
precision_NN = conf.mat_NN[2,2]/sum(conf.mat_NN[2,]);precision_NN
recall_NN = conf.mat_NN[2,2]/sum(conf.mat_NN[,2]);recall_NN
spec_NN=conf.mat_NN[1,1]/sum(conf.mat_NN[,1]);spec_NN

preds_NN_test = predict(model, NN_test.x)
preds_NN_test=t(preds_NN_test)
pred.label_test = ifelse(preds_NN_test<0.55, 0, 1)

conf.mat_NN_test = table(pred.label_test, NN_test.y);conf.mat_NN_test
accuracy_NN_test = sum(diag(conf.mat_NN_test))/sum(conf.mat_NN_test);accuracy_NN_test
precision_NN_test = conf.mat_NN_test[2,2]/sum(conf.mat_NN_test[2,]);precision_NN_test
recall_NN_test = conf.mat_NN_test[2,2]/sum(conf.mat_NN_test[,2]);recall_NN_test

specificity_NN_test = conf.mat_NN_test[1,1]/sum(conf.mat_NN_test[,1]);specificity_NN_test

###################################### KNN Imputation ##################################

KNN_traindataset <- svm_traindata
KNN_testdata <- svm_testdata

KNN_train_withoutclass <- KNN_traindataset[,-193]
KNN_test_withoutclass <- KNN_testdata[,-193]

library(vegan)
library(class)
library(dummies)

noOfNeigh <- 4
pred_KNN = knn(KNN_train_withoutclass, KNN_test_withoutclass, KNN_traindataset$target, k = noOfNeigh)
a_KNN = table(pred_KNN,KNN_testdata$target)
a_KNN
accu = sum(diag(a_KNN))/nrow(KNN_test_withoutclass)
accu
accu_specificity = sum(a_KNN[1,1])/sum(a_KNN[,1]);

############################ Mode of value################
Log=testmatrix_log
DT=P1_test_rpart
NN=pred.label_test
SVM=pred_test
KNN=pred_KNN

modedata=data.frame(Log,DT,NN,SVM,KNN,testdata$ADHERENT)

write.csv(modedata, file = "modeFile.csv")
# o=nrow(modeData)
# for(g in 1:o){
#   if(modeData[1,]$Log==modeData[1,]$DT){
#     co=2
#     if(modeData[1,]$Log==modeData[1,]$NN){
#       co=3
#       if(modeData[1,]$Log==modeData[1,]$SVM){
#         co=4
#       }
#     }
#   }
#   else if(modeData[1,]$Log==modeData[1,]$DT&&modeData[1,]$Log==modeData[1,]$NN){
#     co=2
#   }
# }

##########################################################################
############################################################################################
###Combining training prediction

train_pred_all_models <- NULL

train_pred_all_models$log <-trainmatrix_logistic
train_pred_all_models$DT <- P1_train_rpart
train_pred_all_models$SVM <- pred_train
train_pred_all_models$NN <-pred.label
train_pred_all_models<- data.frame(lapply(train_pred_all_models, as.factor))
str(train_pred_all_models)
# train_pred_all_models$trainmatrix_logistic <- as.factor(train_pred_all_models$trainmatrix_logistic)
# train_pred_all_models$pred.label=as.factor(train_pred_all_models$pred.label)
# table(train_pred_all_models$trainmatrix_logistic) #CART 
# table(train_pred_all_models$P1_train_rpart) #C5.0
# table(train_pred_all_models$pred_train)
# table(train_pred_all_models$V4)

#length(train_pred_all_models$trainmatrix_logistic)
train_pred_all_models <- cbind(train_pred_all_models, traindata$ADHERENT)
names(train_pred_all_models)[5] = "target"


glm_ensemble <- glm(target ~ ., train_pred_all_models, family = binomial())
summary(glm_ensemble)


pred <- predict(glm_ensemble,type="response")
pred <- ifelse(test = pred > 0.5, 1, 0)
table(pred)
check4 <- table(actual=train_pred_all_models$target,pred=pred);check4
accuracy_train = sum(diag(check4))/sum(check4)
accuracy_train
recall_en_train = sum(check4[2,2])/sum(check4[2,]); recall_en_train
spec_en_train=sum(check4[1,1])/sum(check4[1,]); spec_en_train

################ test ensemble####################

test_pred_all_models=NULL
test_pred_all_models$Log=Log
test_pred_all_models$DT =DT  
test_pred_all_models$SVM=SVM
test_pred_all_models$NN=NN
test_pred_all_models<- data.frame(lapply(test_pred_all_models, as.factor))
str(test_pred_all_models)
# test_pred_all_models$Log = as.factor(test_pred_all_models$Log)
# test_pred_all_models$NN=as.factor(test_pred_all_models$NN)

test_pred_all_models<- cbind(test_pred_all_models, testdata$ADHERENT)
names(test_pred_all_models)[5] = "target"
test_pred_all_models<- data.frame(lapply(test_pred_all_models, as.factor))
str(test_pred_all_models)
sum(is.na(test_pred_all_models))
final_pred <- predict(glm_ensemble,type = "response",newdata = test_pred_all_models)

final_pred <- ifelse(test = final_pred > 0.5, 1, 0)
table(final_pred)

check5 <- table(testdata$ADHERENT, final_pred);check5
test_accuracy <- sum(diag(check5))/sum(check5);test_accuracy
recall_en_test = sum(check5[2,2])/sum(check5[2,]); recall_en_test
