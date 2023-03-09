### Simple logic prediction #####
#### only 0.76 accuracy #########

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
logitSurvival <- glm(Survived ~ Pclass + Sex + Age, data = trainData,
                     family = binomial)
summary(logitSurvival)

predictSurvival <- as.numeric(predict(logitSurvival, testData[,c("Pclass","Sex", "Age")],type="response") >0.5
)

data <- data.frame(PassengerId = testData$PassengerId, Survived = predictSurvival)
write.csv(data,"predictSurvival.csv")
