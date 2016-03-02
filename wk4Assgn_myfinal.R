library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
setwd("H:/R")

#Read data & remove fields
pml.training <- read.csv("~/R/pml-training.csv")[,c(7:11,37:49,60:68,84:86,102,113:124,140,151:160)]

summary(pml.training)
pml.training[,54]<-factor(pml.training[,54])

#partitioning to test/train
set.seed(33833)
InTrain<-createDataPartition(y=pml.training[,54],p=.30,list=FALSE)
training<-pml.training[InTrain,]
testing<-pml.training[-InTrain,]

#Random Forest Model
rfmod<-train(classe~.,data=training,method="rf",trControl=trainControl(method="cv",number=5),prox=TRUE,allowParallel=TRUE)
predictions<-predict(rfmod,newdata=testing)

#predictions
confusionMatrix(predictions,testing$classe)

#validation:
pml.testing <- read.csv("~/R/pml-testing.csv")[,c(7:11,37:49,60:68,84:86,102,113:124,140,151:160)]
predict(rfmod,newdata=pml.testing)
