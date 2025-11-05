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
print(ate_trt_gva)

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
hist(distribution_tipamt, main = "Randomized ATE Distribution for Tip Amount", xlab = "ATE", col = "green")

# Calculate p-value
observed_ate <- ate_gva$ate
p_value <- mean(abs(distribution_tipamt) >= abs(observed_ate))
print(p_value)

p_value_ttest <- t.test(tip_amount ~ treatment_dummy, data = washio)$p.value
print(p_value_ttest)