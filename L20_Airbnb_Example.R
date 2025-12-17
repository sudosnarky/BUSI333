# we will analyse the effect of being superhost on the following
# outcomes: monthly bookings, revenue, and speed of booking

pacman::p_load(tidyverse, fixest, modelsummary, rdrobust,
               rddensity)


# load the dataset
airbnb_data <-
  read_csv("data/airbnb_superhost.csv")

# determine if we have sharp or fuzzy RD
airbnb_data |>
  ggplot()+
  aes(x = superhost_score, 
      y = factor(superhost), 
      colour = factor(superhost))+
  geom_jitter()+
  labs(
    x = "Superhost Score",
    y = "Superhost Status"
  ) +
  guides(color = "none")
  
  

# plot the data
airbnb_data |>
  ggplot() +
  aes(superhost_score, monthly_reservations,
      color = factor(superhost)) + 
  geom_smooth(method = "lm") +
  geom_vline(xintercept = 0, lwd = 1.2) +
  theme_bw()


# Density test (test of manipulation)
airbnb_data |>
  ggplot()+
  aes(x = superhost_score)+
  geom_histogram(bins = 40, colour = "white")+
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    x = "Superhost score (running variable)",
    y = "Count of hosts",
    title = "Density of the running variable"
  )

# formally
test_density <-
  rddensity(
    airbnb_data$superhost_score, c = 0
  )
summary(test_density)

# plot the density
plot_density_test <- 
  rdplotdensity(rdd = test_density,
                X = airbnb_data$superhost_score,
                type = "both")  # This adds both points and lines


# let's estimate the LATE
# first using simple linear regression model
model_simple <- 
  lm(monthly_reservations ~ superhost_score + factor(superhost),
                   data = airbnb_data)
tidy(model_simple)


# Estimate the LATE
rd_model1 <-
  rdrobust(
  y = airbnb_data$monthly_reservations,
  x = airbnb_data$superhost_score,
  c = 0
)

summary(rd_model1)


# Plot the RD estimate
rdplot(y = airbnb_data$monthly_reservations, 
       x = airbnb_data$superhost_score,
       c = 0)

# Bandwidth selection
rdbwselect(y = airbnb_data$monthly_reservations, 
           x = airbnb_data$superhost_score, 
           c = 0,
           all = TRUE) |>
  summary()

# Apply a couple of bandwidths to the model
rd_model1 <-
  rdrobust(
    y = airbnb_data$monthly_reservations,
    x = airbnb_data$superhost_score,
    c = 0,
    h = 0.460
  )
summary(rd_model1)