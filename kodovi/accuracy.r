install.packages("tidyverse")
library(tidyverse)

model <- c("Decision tree 90:10", "Decision tree 75:25", "Decision tree 60:40", "Random Forest")
accuracy <- c(0.7962963, 0.8148148, 0.8101852, 0.8851852)
dff <- as.data.frame(cbind(model, accuracy));

dff$accuracy <- as.numeric(dff$accuracy)

dff %>% 
  ggplot(aes(model,accuracy)) +
  labs(title="Accuracy rates") +
  geom_col(fill="orange", colour="black") +
  scale_y_continuous(limits=c(0,1))