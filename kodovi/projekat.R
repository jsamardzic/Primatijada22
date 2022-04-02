#install.packages("readxl")
#install.packages("writexl")
#install.packages("C50")
install.packages("tree")
install.packages("rpart")
install.packages("rpart.plot")


library("readxl")
library("writexl")
library("C50")
library("tree")
library("rpart")
library("rpart.plot")

setwd("/Users/jovan.samke/Downloads/Primatijada")
df <- read_xlsx("NBA_DataSet_Version1.xlsx")

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

write_xlsx(df, path = "NBA_DataSet_Version2.xlsx")

# Delimo df na deo za pravljenje i predikciju
#
#

set.seed(1234)
index <- sample(seq_len(nrow(df)), size = 0.75*nrow(df))
t75 <- df[index,]
p75 <- df[-index,]

prop.table(table(t75$WL))
prop.table(table(p75$WL))

# model <- C5.0(t75[,c(-1,-2,-3,-20,-21)],t75$WL)

model <- rpart(WL ~ .-PTS-oppPTS-FGpct-oppFGpct-FGM-oppFGM, data = t75)
rpart.plot(model)
model$variable.importance
#printcp(model)

model <- tree(WL ~ .-PTS-oppPTS, data = t75)
plot(model)
text(model, cex=0.5)


# model
# summary(model)

results <- predict(model, p75, type = "class")
sum(results == p75$WL)/nrow(p75)