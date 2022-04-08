#install.packages("readxl")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("randomForest")

library("readxl")
library("rpart")
library("rpart.plot")
library("randomForest")

cleanupDataFrame <- function(df){
  df <- na.omit(df)
  df <- df[-c(1:3, 5:7, 10, 13, 18, 24)]
  colnames(df)[c(1, 3:5, 7)] <- c("WL", "FGpct", "ThreePA", "ThreePpct", "FTpct")
  
  df$WL <- as.factor(df$WL)
  
  df[15] <- 1:2160
  colnames(df)[15] <- "index"
  dfopp <- df[df$index%%2==0, ]
  dfteam <- df[df$index%%2!=0, ]
  colnames(dfopp) <- paste(replicate(20, "opp"), colnames(dfopp), sep="")
  df <- cbind(dfteam, dfopp)
  df <- df[-c(15,16,30)]
  
  return(df)
}

simplifyDataFrame <- function(df){
  df2 <- data.frame(df[, 1:14])
  df2[-1] <- df2[-1] - df[15:27]
  colnames(df2) <- paste(replicate(13, "diff"), colnames(df2), sep="")
  colnames(df2)[1] <- "WL"
  
  return(df2)
}

generateTrainingData <- function(df, q, s){
  set.seed(s)
  index <- sample(1:nrow(df), size = q*nrow(df))
  trainingData <- dfSimple[index,]
  predictionData <- dfSimple[-index,]
  
  prop.table(table(trainingData$WL))
  prop.table(table(predictionData$WL))
  
  lista <- list(trainingData, predictionData);
  return(lista)
}

calculateAccuracy <- function(model, predictionData){
  results <- predict(model, predictionData, type = "class")
  sum(results == predictionData$WL)/nrow(predictionData)
}

modelDecisionTree <- function(df, q, s, cp){
  lista <- generateTrainingData(df, q, s)
  trainingData <- lista[[1]]
  predictionData <- lista[[2]]
  
  model <- rpart(WL ~ .-diffFGpct, data = trainingData)
  
  plotcp(model, minline=TRUE, upper = "size")
  model <- prune.rpart(model, cp)
  rpart.plot(model)
  calculateAccuracy(model, predictionData)
}

modelRandomForest <- function(df, q, s, n){
  lista <- generateTrainingData(dfSimple, q, s)
  trainingData <- lista[[1]]
  predictionData <- lista[[2]]
  
  model <- randomForest(WL~.-diffFGpct, data = trainingData, ntree=n)
  model$importance
  print(model)
  
  calculateAccuracy(model, predictionData)
}