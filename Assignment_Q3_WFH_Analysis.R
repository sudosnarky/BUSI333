
pacman::p_load(tidyverse, stargazer, webshot2)

# Read the cleaned WFH dataset
wfh_data <- read_csv("data/wfh_cleaned.csv")

# Display the first few rows
head(wfh_data)

# Display summary statistics
summary(wfh_data)

# Check the structure of key variables
cat("Treatment variable (wfh):\n")
table(wfh_data$wfh)

# Model 1: Simple regression without covariates
# This estimates the average treatment effect (ATE) without controlling for baseline
model1 <- lm(perform_during ~ wfh, data = wfh_data)

summary(model1)

# Model 2: Regression with pre-treatment productivity as a covariate
# This controls for baseline differences and typically improves precision
model2 <- lm(perform_during ~ wfh + perform_before, data = wfh_data)

summary(model2)

# Display table in console
stargazer(model1, model2,
          type = "text",                                    
          title = "Effect of Work From Home on Productivity",
          column.labels = c("Without Covariates", "With Covariates"),
          dep.var.labels = "Productivity (During Experiment)",
          covariate.labels = c("WFH Treatment", "Baseline Productivity", "Intercept"),
          omit.stat = c("f", "ser"),                       # Omit F-stat and residual SE for simplicity
          digits = 3,                                       # Round to 3 decimal places
          star.cutoffs = c(0.05, 0.01, 0.001),            # Significance levels
          notes = "Standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001",
          notes.align = "l")

# Save table as HTML file
stargazer(model1, model2,
          type = "html",
          title = "Effect of Work From Home on Productivity",
          column.labels = c("Without Covariates", "With Covariates"),
          dep.var.labels = "Productivity (During Experiment)",
          covariate.labels = c("WFH Treatment", "Baseline Productivity", "Intercept"),
          omit.stat = c("f", "ser"),
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "Standard errors in parentheses. * p<0.05, ** p<0.01, *** p<0.001",
          notes.align = "l",
          out = "images/regression_table.html")

# Convert HTML to PNG image
webshot2::webshot("images/regression_table.html", 
                  "images/regression_table.png",
                  vwidth = 800,
                  vheight = 600)

