install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
library("readxl")
library("rpart")
library("rpart.plot")
library("randomForest")

setwd("/Users/jovan.samke/Documents/GitHub/Primatijada22")
df <- read_xlsx("./data/NBA_DataSet_Version1.xlsx")

df <- na.omit(df)
df <- df[-c(1:3, 5, 24)]

df$`W/L` <- as.factor(df$`W/L`)

df[20] <- 1:2160
colnames(df)[20] <- "index"

dfopp <- df[df$index%%2==0, ]
dfteam <- df[df$index%%2!=0, ]

colnames(dfopp) <- paste(replicate(20, "opp"), colnames(dfopp), sep="")

df <- cbind(dfteam, dfopp)
df <- df[-c(20,21,40)]
colnames(df)[1] <- "WL"
colnames(df)[6:8] <- c("ThreePM", "ThreePA", "ThreePpct")
colnames(df)[24:26] <- c("oppThreePM", "oppThreePA", "oppThreePpct")
colnames(df)[c(5,11,23,29)] <- c("FGpct", "FTpct", "oppFGpct", "oppFTpct")
df <- df[c(-2,-3,-6,-9,-14,-20,-21,-24,-27,-32)]
rm(dfopp)
rm(dfteam)

dfSimple <- data.frame(df[, 1:14])
dfSimple[-1] <- dfSimple[-1] - df[15:27]
colnames(dfSimple) <- paste(replicate(13, "diff"), colnames(dfSimple), sep="")
colnames(dfSimple)[1] <- "WL"

genTrainingData <- function(df, q){
  indeksi <- sample(1:nrow(df), size = q*nrow(df))
  return(indeksi)
}

################################################################################

set.seed(12345)
index <- genTrainingData(dfSimple, 0.9)
trainingData <- dfSimple[index,]
predictionData <- dfSimple[-index,]

prop.table(table(trainingData$WL))
prop.table(table(predictionData$WL))

model <- rpart(WL ~ .-diffFGpct, data = trainingData)
rpart.plot(model)
#plotcp(model)
model <- prune.rpart(model, cp=0.011)
rpart.plot(model)
model$variable.importance

results <- predict(model, predictionData, type = "class")
sum(results == predictionData$WL)/nrow(predictionData)

################################################################################

set.seed(500)
index <- genTrainingData(dfSimple, 0.75)
trainingData <- dfSimple[index,]
predictionData <- dfSimple[-index,]

prop.table(table(trainingData$WL))
prop.table(table(predictionData$WL))

model <- rpart(WL ~ .-diffFGpct, data = trainingData)
rpart.plot(model)
#plotcp(model)
model <- prune.rpart(model, cp=0.012)
rpart.plot(model)
model$variable.importance

results <- predict(model, predictionData, type = "class")
sum(results == predictionData$WL)/nrow(predictionData)

################################################################################

set.seed(1000)
index <- genTrainingData(dfSimple, 0.6)
trainingData <- dfSimple[index,]
predictionData <- dfSimple[-index,]

prop.table(table(trainingData$WL))
prop.table(table(predictionData$WL))

model <- rpart(WL ~ .-diffFGpct, data = trainingData)
rpart.plot(model)
#plotcp(model)
model <- prune.rpart(model, cp=0.011)
rpart.plot(model)
model$variable.importance

results <- predict(model, predictionData, type = "class")
sum(results == predictionData$WL)/nrow(predictionData)
summary(results)


model <- randomForest(WL~., data = trainingData, ntree = 500)
print(model)
model$importance
results <- predict(model,predictionData)
sum(results == predictionData$WL)/nrow(predictionData)