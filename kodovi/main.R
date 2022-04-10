setwd("/Users/jovan.samke/Documents/GitHub/Primatijada22")
source("./kodovi/libPrimatijada22.R")

df <- cleanupDataFrame(read_xlsx("./data/NBA_DataSet_Version1.xlsx"))
dfSimple <- simplifyDataFrame(df)

modelDecisionTree(dfSimple, 0.90, 0.013, 100, 100000)

modelDecisionTree(dfSimple, 0.75, 0.027, 100, 100000)

modelDecisionTree(dfSimple, 0.60, 0.016, 100, 100000)

modelRandomForest(dfSimple, 0.75, 1000, 500)
