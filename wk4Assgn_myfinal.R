
#Please note: explanation of my assignment is in my assignment write up also in this repo

library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
setwd("H:/R")

#Read data & remove fields
#fields removed were time stamps and fields containing mostly blanks
pml.training <- read.csv("~/R/pml-training.csv")[,c(7:11,37:49,60:68,84:86,102,113:124,140,151:160)]

#turn target variable into a factor
pml.training[,54]<-factor(pml.training[,54])

#partitioning to test/train
set.seed(33833)
InTrain<-createDataPartition(y=pml.training[,54],p=.30,list=FALSE)
training<-pml.training[InTrain,]
testing<-pml.training[-InTrain,]

#Random Forest Model.  A desicion tree was also tested but produced very poor results.
rfmod<-train(classe~.,data=training,method="rf",trControl=trainControl(method="cv",number=5),prox=TRUE,allowParallel=TRUE)
predictions<-predict(rfmod,newdata=testing)

#predictions: this shows an out of sample error rate of 1%.
confusionMatrix(predictions,testing$classe)

#validation: 100% accuracy on the validation set.
pml.testing <- read.csv("~/R/pml-testing.csv")[,c(7:11,37:49,60:68,84:86,102,113:124,140,151:160)]
predict(rfmod,newdata=pml.testing)
