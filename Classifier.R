install.packages("caTools")
install.packages("party")
install.packages("rpart.plot")
install.packages("pROC")
install.packages("FSelector")
install.packages("nnet")

library(caTools)
library(party)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(FSelector)
library(nnet)

EyeColor = read.csv("C:/Users/omerfarukkoc/Desktop/EyeColorData.txt", sep=";")
summary(EyeColor)
View(EyeColor)

EyeColor = EyeColor[sample(nrow(EyeColor), size = 1500),]

plot(EyeColor$FatherEye, main="FatherEye", col=c("deepskyblue", "coral", "green"))
plot(EyeColor$MotherEye, main="MotherEye", col=c("deepskyblue", "coral", "green"))
plot(EyeColor$ChildEye, main="ChildEye", col=c("deepskyblue", "coral", "green"))

EyeColor$ChildEye = as.factor(EyeColor$ChildEye)
sampleSize <- floor(0.70 * nrow(EyeColor))
index = sample(nrow(EyeColor), size = sampleSize)
trainData <- EyeColor[index,]
testData <- EyeColor[-index,]
cat("Training Sample Size: ",sampleSize)

modelCTREE <- ctree(ChildEye ~.,trainData)
plot(modelCTREE)
infGain1 <- information.gain(ChildEye~.,data=trainData, unit = "log"); print(infGain1); cat("Max Inf Gain: ",max(infGain1))
infGain2 <- information.gain(ChildEye~FatherEye+FatherHair+MotherEye+MotherHair+ChildSex,data=trainData, unit = "log"); print(infGain2); cat("Max Inf Gain: ",max(infGain2))
infGain3 <- information.gain(ChildEye~FatherHair+MotherEye+MotherHair+ChildSex,data=trainData, unit = "log"); print(infGain3); cat("Max Inf Gain: ",max(infGain3))

predCtree = predict(modelCTREE, testData)
confMatCtree = table(testData$ChildEye, predCtree)
confusionMatrix(confMatCtree)

modelRPART <- rpart(ChildEye~., trainData, method='class')
rpart.plot(modelRPART)
predRpart = predict(modelRPART, testData, type = "class")
confMatRpart = table(testData$ChildEye, predRpart)
confusionMatrix(confMatRpart)

predMODELRPART <- predict(modelRPART, testData, type = 'prob')
aucMODELRPART <- auc(testData$ChildEye,predMODELRPART[,1])
rocMODELRPART <- (roc(testData$ChildEye,predMODELRPART[,1]))

modelRANDOMFOREST <- randomForest::randomForest(trainData$ChildEye~., trainData)
predRF <- predict(modelRANDOMFOREST, testData, type='class')
confMatRF <- table(predRF, testData$ChildEye)
confusionMatrix(confMatRF)

predMODELRANDOMFOREST <- predict(modelRANDOMFOREST, testData, type = 'prob')
aucMODELRANDOMFOREST <- auc(testData$ChildEye,predMODELRANDOMFOREST[,1])
rocMODELRANDOMFOREST <- (roc(testData$ChildEye,predMODELRANDOMFOREST[,1]))

modelMULTINOM <- multinom(trainData$ChildEye~., trainData)
predMultinom <- predict(modelMULTINOM, testData, type='class')
confMatMultinom <- table(predMultinom, testData$ChildEye)
confusionMatrix(confMatMultinom)

predMODELMULTINOM <- predict(modelMULTINOM, testData, type = 'prob')
aucMODELMULTINOM <- auc(testData$ChildEye,predMODELMULTINOM[,1])
rocMODELMULTINOM <- (roc(testData$ChildEye,predMODELMULTINOM[,1]))

accuracyCtree = (confMatCtree[1,1]+confMatCtree[2,2]+confMatCtree[3,3])/sum(confMatCtree)
accuracyRpart = (confMatRpart[1,1]+confMatRpart[2,2]+confMatRpart[3,3])/sum(confMatRpart)
accuracyRF = (confMatRF[1,1]+confMatRF[2,2]+confMatRF[3,3])/sum(confMatRF)
accuracyMultinom = (confMatMultinom[1,1]+confMatMultinom[2,2]+confMatMultinom[3,3])/sum(confMatMultinom)

par(mfcol=c(1,3))
plot(rocMODELRPART, main="rocMODELRPART", print.auc=TRUE, direction="<",col="blue", lwd=3)
plot(rocMODELRANDOMFOREST, main="rocMODELRANDOMFOREST", print.auc=TRUE, direction="<",col="green", lwd=3)
plot(rocMODELMULTINOM, main="rocMODELMULTINOM", print.auc=TRUE,direction="<",col="red", lwd=3)

cat("\nmodelRPART AUC:", aucMODELRPART); cat("\nmodelRANDOMFOREST AUC:", aucMODELRANDOMFOREST);cat("\nmodelMULTINOM AUC:", aucMODELMULTINOM)
cat("\nmodelRPART Accuracy:", accuracyRpart); cat("\nmodelRANDOMFOREST Accuracy:", accuracyRF); cat("\nmodelMULTINOM Accuracy:", accuracyMultinom)

#pairs(EyeColor, main="Matrix", bg= c("red", "green", "deepskyblue"), unclass(EyeColor$ChildEye), pch=21)

