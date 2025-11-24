library(tidyverse)
library(dplyr)

data <- read_csv('data/washio.csv')
view

print(names(data))
print(summary(data))

hist(data$tip_amount, main = "Histogram of Tip Amount", xlab = "Tip Amount", col = "blue")

washio <- data %>%
filter(suggestion_condition_real %in% c("G", "A"),
       bill_size >= 40)%>% 
       mutate(treatment_dummy = ifelse(suggestion_condition_real == "G", 1, 0))
  print(head(washio))
table(washio$treatment_dummy)

ate_gva <- washio %>% 
  group_by(treatment_dummy) %>%
  summarise(mean_tip = mean(tip_amount, na.rm = TRUE)) %>% 
  summarise(ate = diff(mean_tip))
print(ate_gva)

ate_satis_gva <- washio %>% 
  group_by(treatment_dummy) %>% 
  summarise(mean_satisfaction = mean(satisfaction_rating, na.rm = TRUE)) %>% 
  summarise(ate = diff(mean_satisfaction)) %>% 
  pull(ate)
print(ate_satis_gva)

washio_fk <- washio %>% 
  mutate(fake_treatment = sample(treatment_dummy))
print(head(washio_fk))




compute_randomized_ate <- function() {
  randomized_data <- washio %>% 
    mutate(fake_treatment = sample(treatment_dummy))
  randomized_ate <- randomized_data %>% 
    group_by(fake_treatment) %>% 
    summarise(mean_tip = mean(tip_amount, na.rm = TRUE)) %>% 
    summarise(ate = diff(mean_tip)) %>% 
    pull(ate)

  return(randomized_ate)
}

distribution_tipamt <- replicate(100000, compute_randomized_ate())
distribution_tipamt[1:5]

observed_ate <- ate_gva$ate
hist(distribution_tipamt, main = "Randomized ATE Distribution for Tip Amount", xlab = "ATE", col = "green")
abline(v = observed_ate, col = "red", lwd = 2)
p_value <- mean(abs(distribution_tipamt) >= abs(observed_ate))
print(p_value)
p_value_ttest <- t.test(tip_amount ~ treatment_dummy, data = washio)$p.value
print(p_value_ttest)
if (p_value < 0.05) {
  print("The observed ATE is statistically significant, suggesting an impact of the recommendation system.")
} else {
  print("The observed ATE is not statistically significant, suggesting no strong evidence of an impact of the recommendation system.")
}
t_test_result <- t.test(tip_amount ~ treatment_dummy, data = washio)
print(t_test_result)

observed_ate_satis <- ate_satis_gva
hist(distribution_tipamt, main = "Randomized ATE Distribution for Satisfaction Rating", xlab = "ATE", col = "purple")
abline(v = observed_ate_satis, col = "red", lwd = 2)
p_value_satis <- mean(abs(distribution_tipamt) >= abs(observed_ate_satis))
print(paste("P-value for satisfaction rating:", p_value_satis))
if (p_value_satis < 0.05) {
  print("The observed ATE for satisfaction rating is statistically significant, suggesting an impact of the recommendation system.")
} else {
  print("The observed ATE for satisfaction rating is not statistically significant, suggesting no strong evidence of an impact of the recommendation system.")
}
t_test_satis_result <- t.test(satisfaction_rating ~ treatment_dummy, data = washio)
print(t_test_satis_result)

