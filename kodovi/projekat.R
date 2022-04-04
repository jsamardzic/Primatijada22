install.packages("readxl")
install.packages("rpart")
install.packages("rpart.plot")

library("readxl")
library("rpart")
library("rpart.plot")

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
rm(dfopp)
rm(dfteam)

dfSimple <- data.frame(df[, 1:19])
dfSimple[-1] <- dfSimple[-1] - df[20:37]
colnames(dfSimple) <- paste(replicate(19, "diff"), colnames(dfSimple), sep="")
colnames(dfSimple)[1] <- "WL"

genTrainingData <- function(df, q){
  indeksi <- sample(1:nrow(df), size = q*nrow(df))
  return(indeksi)
}

set.seed(1000)
index <- genTrainingData(dfSimple, 0.90)
trainingData <- dfSimple[index,]
predictionData <- dfSimple[-index,]

index <- genTrainingData(df, 0.75)
trainingData <- df[index,]
predictionData <- df[-index,]

index <- genTrainingData(dfSimple, 0.60)
trainingData <- dfSimple[index,]
predictionData <- dfSimple[-index,]

#prop.table(table(trainingData$WL))
#prop.table(table(predictionData$WL))

model <- rpart(WL ~ .-PTS-oppPTS-FGpct-oppFGpct-FGM-oppFGM, data = trainingData)
rpart.plot(model)
model$variable.importance
model <- prune.rpart(model, cp=0.03) #plotcp(model) za odnos cp - sizeOfTree
rpart.plot(model)

model <- rpart(WL ~ .-diffPTS-diffFGpct-diffFGM, data = trainingData)
rpart.plot(model)
model$variable.importance
model <- prune.rpart(model, cp=0.03) #plotcp(model) za odnos cp - sizeOfTree
rpart.plot(model)

results <- predict(model, predictionData, type = "class")
sum(results == predictionData$WL)/nrow(predictionData)


# model <- tree(WL ~ .-diffPTS, data = trainingData)
# model <- prune.tree(model, best=5) # broj listova
# plot(model)
# text(model, cex=0.5, pretty=0)