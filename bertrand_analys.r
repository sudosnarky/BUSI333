library(tidyverse)
library(dplyr)

data <- read_csv('data/bertrand_clean.csv')
view(data)

table(data$black)
table(data$call)
barplot(table(data$call), main = "Barplot of Call Outcomes", xlab = "Call Outcome", col = "orange")

ate.call <- mean(data$call[data$black == 1]) - mean(data$call[data$black == 0])
print(ate.call)