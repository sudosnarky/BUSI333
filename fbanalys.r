library(readr)
fn <- file.path("githubRepos", "BUSI333", "data", "BUSI333-2025-Worksheet - fb_data.csv")
df <- read_csv(fn)

variables <- c("age", "male", "hhld_inc", "white", "republican", "democrat", "college", "fb_min_wins_b", "news_source_6_b", "fb_activepassive")

balance_table <- data.frame(
  variable = variables,
  treatment_mean = sapply(variables, function(var) mean(df[[var]][df$treatment == 1])),
  control_mean = sapply(variables, function(var) mean(df[[var]][df$treatment == 0])),
  p_value = sapply(variables, function(var) t.test(df[[var]] ~ df$treatment)$p.value)
)


write.csv(balance_table, "balance_table.csv", row.names = FALSE)


View(balance_table)
