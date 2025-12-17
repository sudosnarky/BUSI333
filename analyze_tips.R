library(tidyverse)
library(dplyr)

data <- read_csv('data/washio.csv')

print(names(data))
print(summary(data))

hist(data$tip_amount, main="Histogram of Tip Amounts", xlab="Tip Amount", col="lightblue", border="black")