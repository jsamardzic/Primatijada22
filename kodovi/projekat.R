setwd("/Users/jovan.samke/Documents/GitHub/Primatijada22")
source("primatijada22.R")

df <- cleanupDataFrame(read_xlsx("./data/NBA_DataSet_Version1.xlsx"))
dfSimple <- simplifyDataFrame(df)

modelDecisionTree(dfSimple, 0.9, 12345, 0.013)

modelDecisionTree(dfSimple, 0.75, 500, 0.027)

modelDecisionTree(dfSimple, 0.6, 1000, 0.016)

modelRandomForest(dfSimple, 0.75, 1000, 500)