#'
#' Lab 05 guide
pacman::p_load(tidyverse, data.table, broom)
options("scipen"=999)

## Butler/Broockman
data <- 
fread("data/broockman_butler_cleaned.csv")

#Explore data
table(data$reply_atall)

# plot the outcome
data %>% ggplot() +
  geom_bar(aes(x=factor(reply_atall),
               y=stat(count/sum(count))),
           fill = "steelblue") +
  theme_bw() 


table(data$treat_deshawn)

#ATE by hand
ate <- 
  mean(
  data$reply_atall[data$treat_deshawn==1]
  )- 
  mean(
    data$reply_atall[data$treat_deshawn==0]
    )
ate

tidy(lm(reply_atall ~ treat_deshawn, data))

# By party
tidy(lm(reply_atall ~ treat_deshawn, 
        data, leg_republican == 1))
tidy(lm(reply_atall ~ treat_deshawn, 
        data, leg_republican == 0))
tidy(lm(reply_atall ~ 
          treat_deshawn * leg_republican, 
        data))

# By race of the legislator
tidy(lm(reply_atall ~ treat_deshawn,
        data, leg_notwhite == 1))
tidy(lm(reply_atall ~ 
          treat_deshawn, 
        data, leg_notwhite == 0))
tidy(lm(reply_atall ~ 
          treat_deshawn * leg_notwhite, 
        data))

# why did Democrats not discriminate?
tidy(
  lm(reply_atall ~ treat_deshawn, 
     data,leg_notwhite == 1 & 
          leg_republican == 0))
tidy(lm(reply_atall ~ treat_deshawn, 
        data, 
        leg_notwhite == 0 &
          leg_republican == 0))
tidy(lm(reply_atall ~ 
          treat_deshawn * leg_notwhite,
        data, leg_republican == 0))



## Bertrand data
bertrand <- 
  read.csv("data/bertrand_clean_lab5.csv")


#ATE by hand
ate.call <- 
  mean(bertrand$call[bertrand$black==1])- 
  mean(bertrand$call[bertrand$black==0])
ate.call

#Calculate basic ATE of black with regression
tidy(lm(call ~ black, data=bertrand))

#Balance tests - Table 3 in the paper
#female
tidy(lm(black~female, data=bertrand))
#high quality
tidy(lm(black~high_quality, data=bertrand))
#boston
tidy(lm(black~boston, data=bertrand))
# all three
tidy(
  lm(black~boston+high_quality+female, 
     data=bertrand))

# Now put female, high_quality, and boston into the regression.
# boston was not randomly assigned but the others were.
# How do we interpret each coefficient?
tidy(
  lm(call ~ 
       black + female + 
       high_quality + boston, 
     data=bertrand)
  )


##HTE: First start with quality since that's their main finding
#Method 1: separate regressions
tidy(lm(call~black, 
        data=bertrand, 
        subset = bertrand$high_quality==1))
tidy(lm(call~black, 
        data=bertrand, 
        subset = bertrand$high_quality==0))

#Method 2: interaction terms
tidy(lm(call~
          black + high_quality + 
          black*high_quality, 
        data=bertrand))

## YearsExp- 
## Continuous variable where you can only compute the
## HTE with interaction term bc too many levels
tidy(lm(call~
          black + yearsexp + black*yearsexp,
        data=bertrand))


# This is a graphical representation of the regression above.
bertrand$black.resume <- 
  factor(bertrand$black)
levels(bertrand$black.resume) <- 
  c('White', 'Black')
ggplot(bertrand, 
       aes(y = call, x = yearsexp)) + 
  geom_smooth(
    aes(color = black.resume), 
    se = FALSE, 
    method = 'lm') +
  ggtitle(
    'Heterogenous Effect of Black Name by Years of Experience') +
  ylab('Callback Rate') + 
  xlab('Years Experience')




## Income in city
tidy(lm(call ~ black + income_in_city + black * income_in_city, data=bertrand))

# Another one to put on the slide. Regression is extrapolating wildly!
ggplot(bertrand, aes(y = call, x = income_in_city)) + 
  geom_smooth(aes(color = black.resume), se = FALSE, method = 'lm') +
  ggtitle('Heterogenous Effect of Black Name by Income of City') +
  ylab('Callback Rate') + xlab('Income in City (in $10,000s)')

ggplot(bertrand, aes(y = call, x = income_in_city)) + 
  geom_smooth(aes(color = black.resume), se = FALSE, method = 'lm') +
  ggtitle('Heterogenous Effect of Black Name by Income of City') +
  ylab('Callback Rate') + xlab('Income in City (in $10,000s)') + 
  xlim(c(0, 6.4)) + ylim(c(0,1))


## College
tidy(lm(call~black, data=bertrand, subset = bertrand$college==1))
tidy(lm(call~black, data=bertrand, subset = bertrand$college==0))
tidy(lm(call~black + college + black*college, data=bertrand))

## Boston
tidy(lm(call~black, data=bertrand, subset = bertrand$boston==1))
tidy(lm(call~black, data=bertrand, subset = bertrand$boston==0))
tidy(lm(call~black + boston + black*boston, data=bertrand))