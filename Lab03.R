#####Lab 3 Code
pacman::p_load(tidyverse, data.table, broom, kableExtra)


########Broockman and Kalla
# Read in the data.
data <- fread("/home/snarky404/githubRepos/BUSI333/data/bk_data_cleaned.csv")

# Explore the data. 
##Look at the variables and create a histogram of variables we're interested in.
#For this exercise, we'll focus on the distribution of 
names(data)
head(data)
table(data$treat_ind) #balanced treatment
table(data$block)
hist(data$therm_trans_t0)
hist(data$therm_trans_t1)


###Part 1. ATE without blocking
#First, let's look at the variable "therm_trans_t1," 
##which is the feeling thermometer rating of 
#participants at time 1.
# To start, calculate the estimated ATE for therm_trans_t1 and name it trans.ate.t1
ate.estimate <- mean(data$therm_trans_t1[data$treat_ind==1])- 
                mean(data$therm_trans_t1[data$treat_ind==0])
ate.estimate

# Let's calculate the sampling Distribution of the ATE 
##for time 1 under the null hypothesis.
# This is the same process that we've done in class and in lab.
# First, write a function that takes the treatment 
##as it was randomly assigned and re-randomizes.
# To do this, just take the existing random assignment and shuffle the order randomly.
# Remember, a function is defined this way:
# name.of.function <- function() return(results.to.return.here)
# Verify your function works by running name.of.function() a few times.
randomize <- function() return(sample(data$treat_ind))
randomize()

# Now, write a second function that takes a given outcome as an argument and computes
# the sampling distriubtion under the null hypothesis. That is, the function should
# compute a randomization using the first function you wrote and then compute
# the "ATE estimate" one would get under that randomization. This gives
# you some draws from the sampling distribution you would see if the skeptic were correct
# and the experiment was just randomly shuffling outcomes around between groups
# but there was no actual effect.
rand.ate.t1 <- function() {
  treatment.fake <- randomize()
  return(mean(data$therm_trans_t1[treatment.fake == 1]) - 
         mean(data$therm_trans_t1[treatment.fake == 0]))
}

rand.ate.t1()
rand.ate.t1()
rand.ate.t1()

# Now, replicate that function 10,000 times using replicate() and store
# the results in a new variable. This is the sampling distribution under
# the null hypothesis.
null.hypothesis.distrib.ate.t1.nb <- replicate(10000, rand.ate.t1())
null.hypothesis.distrib.ate.t1.nb



# Graph this distribution in a histogram. Insert a 
# vertical line that represents the observed ATE using abline(v=[your ate here]).
hist(null.hypothesis.distrib.ate.t1.nb, breaks=100, col='grey90')
abline(v=ate.estimate, lwd = 3, col="blue")

# Calculate the p-value by comparing the distribution to the observed ATE.
mean(null.hypothesis.distrib.ate.t1.nb >= ate.estimate)

# Estiamte the uncertainty around these estimates First, calcuate the 
# standard deviation of this distribution. Use the sd() function. 
# Then, calculate the interval in which the estimate will be wihin 95% of the time.
sd(null.hypothesis.distrib.ate.t1.nb)
ate.estimate + 1.96*sd(null.hypothesis.distrib.ate.t1.nb)
ate.estimate - 1.96*sd(null.hypothesis.distrib.ate.t1.nb)


#Part 2: ATE with blocking

#Get a sense of whether or not blocking will be useful
table(data$block, data$treat_ind) 

## Is it going to be helpful to block? Let's take a look at how predictive t0 is of t1
tidy(lm(therm_trans_t1 ~ therm_trans_t0, data), conf.int = T)
plot(data$therm_trans_t0, data$therm_trans_t1)


# Let's estimate the treatment effect again, this time accounting for the blocking.
# Remember that you've alread generated the necessary functions, 
## so make small edits and apply
# those functions to this example. This time however, 
## you'll need to make a new randomization function
# that takes into account the block randomization 
##(the variable for the blocks is called "block").
# Then place this function into a new function that 
##generates ATE estimates under the null.
randomize.by.block <- function() {
  treatment <- rep(NA, nrow(data))
  for(b in unique(data$block)){
    treatment[data$block == b] <- sample(c(0,1))
  }
  return(treatment)
}

rand.ate.block <- function() {
   treatment <- randomize.by.block()
   ate.estimate <- mean(data$therm_trans_t1[treatment == 1]) - 
                   mean(data$therm_trans_t1[treatment == 0])
   return(ate.estimate)
}

rand.ate.block()

#Sampling Distribution
null.hypothesis.distrib.ate.t1.block <- replicate(10000, rand.ate.block())
null.hypothesis.distrib.ate.t1.block

#Histogram and plotted ATE
hist(null.hypothesis.distrib.ate.t1.block, breaks = 200, col = 'grey90')
abline(v = ate.estimate, lwd = 3, col="blue")

#Test for significance
mean(null.hypothesis.distrib.ate.t1.block >= ate.estimate)

sd(null.hypothesis.distrib.ate.t1.nb)
sd(null.hypothesis.distrib.ate.t1.block)
ate.estimate - 1.96*sd(null.hypothesis.distrib.ate.t1.block)
ate.estimate + 1.96*sd(null.hypothesis.distrib.ate.t1.block)

#Regressions comparing SE
tidy(lm(therm_trans_t1~treat_ind, data=data), conf.int = T)
tidy(lm(therm_trans_t1~treat_ind + factor(block), data=data),conf.int = T)
tidy(lm(therm_trans_t1~treat_ind + therm_trans_t0, data=data), conf.int = T)


##### LaCour data
lacour <- fread('data/lacour_data_cleaned.csv')

#Calculate the apparent ATE
ate.lacour <- mean(lacour$therm_gay_t1[lacour$treat_ind==1])-
              mean(lacour$therm_gay_t1[lacour$treat_ind==0])
ate.lacour 

# Compare SE with Broockman and Kalla study
tidy(lm(therm_gay_t1 ~ treat_ind, lacour), conf.int = T)
tidy(lm(therm_trans_t1 ~ treat_ind, data=data), conf.int = T)

#Treating therm_gay_t0 as a block--> Standard errors shrink
tidy(lm(therm_gay_t1 ~ treat_ind + therm_gay_t0, lacour), conf.int=T)

#Precision of the data --> therm_gay_t0 a near perfect predictor of therm_gay_t1
tidy(lm(therm_gay_t1 ~ therm_gay_t0, lacour), conf.int=T)
plot(lacour$therm_gay_t0, lacour$therm_gay_t1)
