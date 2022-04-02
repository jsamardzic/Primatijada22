#install.packages("readxl")
#install.packages("writexl")
library("readxl")
library("writexl")

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

write_xlsx(df, path = "NBA_DataSet_Version2.xlsx")