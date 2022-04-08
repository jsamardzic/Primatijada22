#setwd("/Users/jovan.samke/Documents/GitHub/Primatijada22")
#setwd("C:/Users/JAVOR/Desktop/R/Primatijada22")
#Napravicu za svaki po jednu tabelu u xl da imamo variable importance
source("./kodovi/libPrimatijada22.R")
df <- cleanupDataFrame(readxl::read_xlsx("./data/NBA_DataSet_Version1.xlsx"))
dfSimple <- simplifyDataFrame(df)

class(modelDecisionTree(dfSimple, 0.9, 12345, 0.013))
writexl::write_xlsx(modelDecisionTree(dfSimple, 0.9, 12345, 0.013),"./data/DT_Var_Imp90.xlsx")

writexl::write_xlsx(modelDecisionTree(dfSimple, 0.75, 500, 0.027),"./data/DT_Var_Imp75.xlsx")

writexl::write_xlsx(modelDecisionTree(dfSimple, 0.6, 1000, 0.016),"./data/DT_Var_Imp60.xlsx")

writexl::write_xlsx(data.frame(names(modelRandomForest(dfSimple, 0.75, 1000, 500)),modelRandomForest(dfSimple, 0.75, 1000, 500)),"./data/RF.xlsx")

                    