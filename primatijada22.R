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
  model$variable.importance
  plotcp(model, minline=TRUE, upper = "size")
  printcp(model)
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