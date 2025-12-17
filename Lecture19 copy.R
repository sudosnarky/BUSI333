#'
#' This is the code for the lecture on
#' natural experiments. 
#' We will learn the following techniques in this code:
#' 1. Matching.
#' 2. Regression discontinuity


pacman::p_load(tidyverse, data.table, kableExtra, estimatr,
               lfe, fixest, modelsummary, 
               broom, forcats, MatchIt)

#__________Propensity score matching______________#

# myntra experiment
# offer free one-year shipping to customers

myntra_data <- 
  read_csv("data/myntra_data.csv")

table(myntra_data$free_shipping)

# Step 1: Preprocessing
matched_data <- 
  matchit(free_shipping ~ age + city + 
                          gender + income,
                        data = myntra_data,
                        method = "nearest",
                        distance = "mahalanobis",
                        replace = TRUE)
myntra_data_matched <- match.data(matched_data)

# Step 2: Estimation
model_matched <- 
  lm(
    average_spend ~ free_shipping, 
    data = myntra_data_matched
  )
tidy(model_matched)

# Run model with weights
model_matched_wt <- 
  lm(
    average_spend ~ free_shipping,
    weights = weights,
    data = myntra_data_matched
  )
tidy(model_matched_wt)

modelsummary(list("Model without weights" = model_matched, 
                  "Model with weights" = model_matched_wt))

# In this method, you have to throw away a lot of data.
# You can try inverse probability weight method
# if you wish to retain the data and model.
model_logit <- glm(free_shipping ~ age + city + 
                     gender + income,
                   data = myntra_data,
                   family = binomial(link = "logit"))

tidy(model_logit)


freeship_prob <- 
  augment_columns(model_logit,
                  myntra_data,
                  type.predict = "response") %>%
  select(-.resid, -.std.resid, 
         -.hat, -.sigma, 
         -.cooksd, -.se.fit) %>%
# The predictions are in a column named ".fitted", 
# so we rename it here
  rename(propensity = .fitted)

# Look at the first few rows of a few columns
freeship_prob %>%
  select(customer_id, free_shipping, propensity) %>%
  head()

# create IPW
freeship_ipw <- freeship_prob %>%
  mutate(ipw = (free_shipping / propensity) + 
              ((1 - free_shipping) / (1 - propensity)))

model_ipw <- lm(average_spend ~ free_shipping,
                data = freeship_ipw,
                weights = ipw)

tidy(model_ipw)

modelsummary::modelsummary(list(model_matched, model_matched_wt,
                                model_ipw))



#_____________Regression Discontinuity______________#
card_data <- 
  read_csv("data/brandedcard_prog.csv") %>%
  mutate(card = factor(card))
  

# plot the data
card_data %>%
  ggplot() +
  aes(credit_score, average_spending,
      color = card) + 
  geom_point() +
  geom_vline(xintercept = 700)


# check if there's any discontinuity in credit score 
# around the cutoff
card_data %>%
  ggplot() +
  aes(x = credit_score, fill = card) +
  geom_histogram(binwidth = 20,
                 color = "white", boundary = 700) +
  geom_vline(xintercept = 700) +
  labs(x = "Credit Score", y = "Count", fill = "Card")

# check if there's discontinuity in 
# average spending around the cutoff
card_data %>%
  ggplot() +
  aes(x = credit_score, y = average_spending, color = card) +
  geom_point(size = 0.6, alpha = 0.6) +
  geom_smooth(method = "lm")+
  geom_vline(xintercept = 700) +
  labs(x = "Credit score",
       y = "Average spending",
       color = "Branded Card")

# Run a simple model
card_centered <- card_data %>%
  mutate(creditscore_c = credit_score - 700)

model_simple <- lm(average_spending ~ creditscore_c + card,
                   data = card_centered)
tidy(model_simple)

# Run a model for a bandwidth
# +/- 100
model_bw_100 <- lm(average_spending ~ creditscore_c + card,
                  data = filter(card_centered,
                                creditscore_c >= -100 &
                                creditscore_c <= 100))
tidy(model_bw_100)

# +/- 50
model_bw_50 <- lm(average_spending ~ creditscore_c + card,
                   data = filter(card_centered,
                                 creditscore_c >= -50 &
                                   creditscore_c <= 50))
tidy(model_bw_50)


modelsummary(list("Full sample" = model_simple,
                  "Bandwidth = 100" = model_bw_100,
                  "Bandwidth = 50" = model_bw_50))