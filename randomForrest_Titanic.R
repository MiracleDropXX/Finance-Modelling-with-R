# install.packages("caTools")    # For Logistic regression 
library(caTools)

# install.packages('randomForest') # For generating random forest model
library(randomForest)

# install.packages('caret')                    # classification and regression training : The library caret has a function to make prediction.
library(caret)
# install.packages('e1071', dependencies=TRUE)



require(readxl)
require(openxlsx)
require(tidyverse)

# read Data

read.csv("C:/Users/t.nguyen/Desktop/FIMDEV/Statistics and Machine Learning/titanicTrain.csv") -> trainData
read.csv("C:/Users/t.nguyen/Desktop/FIMDEV/Statistics and Machine Learning/titanicTest.csv") -> testData  

# data exploration, cleaning
# merging train and test into one big set: keep id of train and test
data.full.train <-  trainData[,-2]
data.full.train$isTrain <- TRUE
data.full.test  <- testData  |> mutate(isTrain = FALSE)

data.full <- rbind(data.full.train, data.full.test)
data.full$isTrain <- factor(data.full$isTrain)
tapply(data.full$Sex,data.full$isTrain, summary)

table(data.full[,c("Sex","isTrain")])

data.full  |> summary()

trainData$Age  |> summary()

### try to regress on non missing data 
lm(log(Age)~ Pclass + Sex + SibSp + Parch , data= trainData) -> OlSAge


# Predict age NA on the full data set:
newData <- data.full  |> filter(is.na(Age)); id_NA <- newData$PassengerId
newData   <- newData[,c(2,4,6,7,9)]
# make prediction of Age on OLS
predictLogage <- predict(OlSAge, newData) ; predictLogage  |> print()
predictAge <- exp(predictLogage)

# put Age into our data:

for (i in id_NA) {
  posAge <- which(id_NA == i)
  data.full$Age[which(data.full$PassengerId == i)] <- predictAge[posAge]
  
}

# create sex to be male, female, maleKid, femaleKid

data.full <- data.full  |>
  mutate(Sex = ifelse(Sex == "male"  & Age <= 15, "maleKid", ifelse(Sex =="female"& Age <= 15, "femaleKid", Sex)))


whichNA_fares <- which(is.na(data.full$Fare))
data.full$Fare[whichNA_fares] <- mean(data.full$Fare[which(data.full$isTrain == TRUE)], na.rm = T)

# Train a logit model?
trainData[,-c(1,2)] <- data.full[,-1]  |> filter(isTrain == TRUE)  |> select(-isTrain)
testData [,-c(1)]   <-  data.full[,-1]  |> filter(isTrain == FALSE)  |> select(-isTrain)

# logit model



model <- randomForest(Survived ~ Pclass+ Sex + Age + SibSp + Parch,data= trainData[,-1])


importance(model)                       # returns the importance of the variables : most siginificant - cp followed by thalach and so on......

varImpPlot(model)                                   # visualizing the importance of variables of the model.





pred_test <- ifelse(predict(model, newdata = testData[,c(2,4,5,6,7)], type= "class") > 0.5,1,0)


data <- data.frame(PassengerId = testData$PassengerId, Survived = pred_test)
write.csv(data,"predictSurvival.csv", row.names =  F)


