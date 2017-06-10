rm(list = ls())
setwd("C://Users//kisho//Downloads")
classification=read.table("ClassificationProblem1.txt",header = TRUE , sep="")
str(classification)
summary(classification)

# Removing Index as it is an unique identifies=r
classification$Index<-NULL
sum(is.na(classification))

# Fromatting Date Column and Extracting Month From Date
library(lubridate)
classification$F15=parse_date_time(x = classification$F15,orders = c("m/d/y","d/m/y"),locale = "eng")
classification$F16=parse_date_time(x = classification$F16,orders = c("m/d/y","d/m/y"),locale = "eng")
classification$F15_month=format(as.Date(classification$F15,format="%Y-%m-%d"), "%m")
classification$F16_month=format(as.Date(classification$F16,format="%Y-%m-%d"), "%m")
classification$F15_month = as.factor(classification$F15_month)
classification$F16_month = as.factor(classification$F16_month)
classification$F15<-NULL
classification$F16<-NULL

# Splitting Categorical Attributes from Original Dataset
categorical <- subset(classification,select = c(F17,F18,F21,F22,C))
categorical[] <- lapply( categorical, factor)
categorical<-cbind(categorical,classification$F15_month)
categorical<-cbind(categorical,classification$F16_month)
names(categorical)[6]<-"F15"
names(categorical)[7]<-"F16"
str(categorical)

# Splitting Numerical Attributes from Original Dataset
numerical <-subset(classification,select = -c(F15_month,F16_month,F17,F18,F21,F22,C))
all(categorical$F21==categorical$F22)
str(categorical)
#plot(F19,F20)
library(vegan)
# Note: To standardize the data using 'Range' method  # (x-min)/max-min
numeric_deco = decostand(numerical, "range")
classif_merged =cbind(numeric_deco,categorical) 
str(classif_merged)
sum(is.na(classif_merged))

set.seed(1010)

library(caTools)
split= sample.split(classif_merged$C,SplitRatio = 0.7)
train=subset(classif_merged,split==TRUE)
test=subset(classif_merged,split==FALSE)

lgmout = glm(C ~.,data=train,family = "binomial")
summary(lgmout)

viftrain= vif(lgmout)

trainpredict = predict(lgmout,type="response")

trainmatrix=ifelse(trainpredict>0.3,1,0)

t1=table(train$C,trainmatrix);t1

trainaccuracy=(t1[1,1]+t1[2,2])/sum(t1[1,]+t1[2,]);trainaccuracy

trainrecall = (t1[2,2])/sum(t1[2,1]+t1[2,2]);trainrecall

trainspec = (t1[1,1])/sum(t1[1,1]+t1[1,2]);trainspec


testpredict= predict(lgmout,type="response",newdata = test)

testmatrix=ifelse(testpredict>0.3,1,0)

t2=table(test$C,testmatrix);t2

testaccuracy=(t2[1,1]+t2[2,2])/sum(t2[1,]+t2[2,]);testaccuracy

testrecall = (t2[2,2])/sum(t2[2,1]+t2[2,2]);testrecall

testspec = (t2[1,1])/sum(t2[1,1]+t2[1,2]);testspec

########## Predicting on the Given Test Data##########################
classification_test=read.table("Classification1Test.txt",header = TRUE , sep="")
classification_test$Index<-NULL
sum(is.na(classification_test))

# Fromatting Date Column and Extracting Month From Date
library(lubridate)
classification_test$F15=parse_date_time(x = classification_test$F15,orders = c("m/d/y","d/m/y"),locale = "eng")
classification_test$F16=parse_date_time(x = classification_test$F16,orders = c("m/d/y","d/m/y"),locale = "eng")
classification_test$F15_month=format(as.Date(classification_test$F15,format="%Y-%m-%d"), "%m")
classification_test$F16_month=format(as.Date(classification_test$F16,format="%Y-%m-%d"), "%m")
classification_test$F15_month = as.factor(classification_test$F15_month)
classification_test$F16_month = as.factor(classification_test$F16_month)
classification_test$F15<-NULL
classification_test$F16<-NULL

# Splitting Categorical Attributes from Original Dataset
categorical_test <- subset(classification_test,select = c(F17,F18,F21,F22))
categorical_test[] <- lapply( categorical_test, factor)
categorical_test<-cbind(categorical_test,classification_test$F15_month)
categorical_test<-cbind(categorical_test,classification_test$F16_month)
names(categorical_test)[5]<-"F15"
names(categorical_test)[6]<-"F16"
str(categorical_test)

# Splitting Numerical Attributes from Original Dataset
numerical_test <-subset(classification_test,select = -c(F15_month,F16_month,F17,F18,F21,F22))
str(categorical_test)
library(vegan)
# Note: To standardize the data using 'Range' method  # (x-min)/max-min
numeric_deco_test = decostand(numerical_test, "range")
classif_merged_test =cbind(numeric_deco_test,categorical_test) 
str(classif_merged_test)
sum(is.na(classif_merged_test))

######Predicting on building model####

pred_logistic = predict(lgmout,type="response",newdata = classif_merged_test)

### Writing Probabilities of the Test Data######
pred_logistic=data.frame(pred_logistic)
names(pred_logistic)[0]<-"Index"
names(pred_logistic)[1]<-"Probabilities"
write.csv(pred_logistic,"TestPredictions_Probabilities_Log.txt")

#### Writing the Predicted Class of the Test Data

class_Logistic=ifelse(pred_logistic>0.3,1,0)
class_Logistic=data.frame(class_Logistic)
names(class_Logistic)[0]<-"Index"
names(class_Logistic)[1]<-"Class"
write.csv(class_Logistic,"TestPredictions_Class_Log.txt")
