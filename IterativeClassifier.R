install.packages("caTools")
library(caTools)
install.packages("ineq")
library(ineq)
install.packages("party")
library(party)
library(caret)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("ROCR")
library(ROCR)
install.packages("tree")
library(tree)
install.packages("FSelector")
library(FSelector)

for(i in seq(from=100, to=2000, by=100)){
  EyeColor = read.csv("C:/Users/omerfarukkoc/Desktop/EyeColorData.txt", sep=";")
  EyeColor$ChildEye = as.factor(EyeColor$ChildEye)
  sampleSize <- floor(0.70 * nrow(EyeColor))
  index = sample(nrow(EyeColor), size = i)
  trainData <- EyeColor[index,]
  testData <- EyeColor[-index,]
  
  model <- randomForest::randomForest(trainData$ChildEye~., trainData)
  #model <- multinom(trainData$ChildEye~., trainData)
  predTestData <- predict(model, testData, type='class')
  confMatTestData <- table(predTestData, testData$ChildEye)
  accuracyTestdData = (confMatTestData[1,1]+confMatTestData[2,2]+confMatTestData[3,3])/sum(confMatTestData)
  #accuracyTDArray=stack(accuracyTestdData)
  cat("\n\nSample Size: ",i)
  cat("\nPredict Test Data Accuracy: ", head(round(accuracyTestdData,2)))
  
  predTrainData <- predict(model, trainData, type='class')
  confMatTrainData <- table(predTrainData, trainData$ChildEye)
  accuracyTrainData = (confMatTrainData[1,1]+confMatTrainData[2,2]+confMatTrainData[3,3])/sum(confMatTrainData)
  cat("\nPredict Train Data Accuracy: ", head(round(accuracyTrainData,2)))
  #plot(accuracyTrainData,i)
} 

