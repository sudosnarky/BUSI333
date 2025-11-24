
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

data <- read_dta('data/master_establishments_1880_1905.dta')


data_filtered <- data %>%
  filter(year %in% c(1880, 1885))


data_filtered <- data_filtered %>%
  mutate(ctgr = case_when(
    Pshr %in% c("GRO", "BAKER", "FISH", "FRUIT", "PROV", "PROD", "CON", "LIQ") ~ "Food",
    Pshr %in% c("CLO", "B.S.", "DRYGOODS", "HAT", "MENFURN", "MILLINER", "TAILOR") ~ "Clothing",
    TRUE ~ "Other products"
  ))

data_filtered <- data_filtered %>%
  mutate(dist = case_when(
    DistToCBD < 1 ~ "0-1km",
    DistToCBD >= 1 & DistToCBD < 3 ~ "1-3km",
    DistToCBD >= 3 ~ ">3km"
  ))

data_filtered <- data_filtered %>%
  mutate(treat_band = 2 - treat_band)

data_grouped <- data_filtered %>%
  group_by(year, ctgr, dist, treat_band) %>%
  summarise(
    mSole = mean(Sole, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  filter(n >= 20)

data_wide <- data_grouped %>%
  select(year, ctgr, dist, treat_band, mSole) %>%
  pivot_wider(
    names_from = year,
    values_from = mSole,
    names_prefix = "mSole_"
  ) %>%
  mutate(mSole_change = mSole_1885 - mSole_1880)

data_wide <- data_wide %>%
  filter(treat_band %in% c(0, 1)) %>%
  mutate(
    dist = factor(dist, levels = c("0-1km", "1-3km", ">3km")),
    ctgr = factor(ctgr, levels = c("Food", "Clothing", "Other products")),
    treat_band = factor(treat_band, levels = c(0, 1), labels = c("Control", "Treatment"))
  )
p <- ggplot(data_wide, aes(x = dist, y = mSole_change, fill = treat_band)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  facet_wrap(~ ctgr, ncol = 3) +
  scale_fill_manual(values = c("Control" = "#8B4513", "Treatment" = "#2F4F4F")) +
  labs(
    title = "Panel A. 1880-1885",
    x = "",
    y = "1880-1885 changes in the share of\nsole proprietorships, total estimate",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 12),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_blank()
  ) +
  ylim(-0.4, 0.1)

ggsave("images/panel_a_1880_1885.png", plot = p, width = 10, height = 4, dpi = 300)

print(p)

data_q2 <- read_csv('data/a02_q02.csv')

set.seed(333)

available_vars <- c("software", "number_teammates", "min_professor", "min_postdoc", 
                    "min_researcher", "min_student", "max_professor", "max_postdoc", 
                    "max_researcher", "max_student", "combined_follow", "min_gpt_never", 
                    "min_gpt_beginner", "min_gpt_intermediate", "min_gpt_advanced", 
                    "max_gpt_never", "max_gpt_beginner", "max_gpt_intermediate", 
                    "max_gpt_advanced")

selected_vars <- sample(available_vars, 5)
cat("\nRandomly selected variables for balance table:\n")
print(selected_vars)

create_balance_table <- function(data, treatment_var, variables) {
  
  balance_results <- data.frame(
    Variable = character(),
    Control_Mean = numeric(),
    Control_SD = numeric(),
    Treatment_Mean = numeric(),
    Treatment_SD = numeric(),
    Difference = numeric(),
    P_Value = numeric(),
    stringsAsFactors = FALSE
  )
  for (var in variables) {
    control_data <- data %>% filter(branch != "AI-Led") %>% pull(!!sym(var))
    treatment_data <- data %>% filter(branch == "AI-Led") %>% pull(!!sym(var))
    
    control_mean <- mean(control_data, na.rm = TRUE)
    control_sd <- sd(control_data, na.rm = TRUE)
    treatment_mean <- mean(treatment_data, na.rm = TRUE)
    treatment_sd <- sd(treatment_data, na.rm = TRUE)
    
    difference <- treatment_mean - control_mean
    
    t_test_result <- t.test(treatment_data, control_data)
    p_value <- t_test_result$p.value
    
    balance_results <- rbind(balance_results, data.frame(
      Variable = var,
      Control_Mean = control_mean,
      Control_SD = control_sd,
      Treatment_Mean = treatment_mean,
      Treatment_SD = treatment_sd,
      Difference = difference,
      P_Value = p_value
    ))
  }
  
  return(balance_results)
}
balance_table <- create_balance_table(data_q2, "branch", selected_vars)

balance_table_formatted <- balance_table %>%
  mutate(
    Control = sprintf("%.3f (%.3f)", Control_Mean, Control_SD),
    Treatment = sprintf("%.3f (%.3f)", Treatment_Mean, Treatment_SD),
    Difference = sprintf("%.3f", Difference),
    P_Value_formatted = sprintf("%.4f", P_Value),
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, Control, Treatment, Difference, P_Value_formatted, Significance)

table_display <- balance_table_formatted
colnames(table_display) <- c("Variable", "Non-AI-Led\nMean (SD)", "AI-Led\nMean (SD)", 
                              "Difference", "P-Value", "Sig.")

tt <- ttheme_default(
  core = list(
    fg_params = list(cex = 0.7),
    bg_params = list(fill = c(rep(c("white", "grey95"), length.out = nrow(table_display))))
  ),
  colhead = list(
    fg_params = list(cex = 0.8, fontface = "bold"),
    bg_params = list(fill = "lightblue")
  )
)

table_grob <- tableGrob(table_display, rows = NULL, theme = tt)

title <- textGrob("Balance Table: AI-Led vs. Non-AI-Led Teams", 
                  gp = gpar(fontsize = 14, fontface = "bold"))
subtitle <- textGrob("Note: *p<0.05, **p<0.01, ***p<0.001", 
                     gp = gpar(fontsize = 9, fontface = "italic"))

padding <- unit(5, "mm")
table_with_title <- arrangeGrob(
  title, 
  subtitle,
  table_grob,
  heights = unit(c(1, 0.5, nrow(table_display) * 0.4), c("lines", "lines", "cm")),
  padding = padding
)

ggsave("images/balance_table.png", table_with_title, 
       width = 10, height = max(4, nrow(table_display) * 0.3 + 1.5), 
       dpi = 300, bg = "white")