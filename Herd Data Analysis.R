herd<-read.csv("data/Herd_data_2.csv")
View(herd)
table(herd$Income_Change,herd$Pri_Livelihood)
herd$Income_Change2 <- factor(herd$Income_Change, levels = c("Decreased", "Increased","Remained same"),
                       labels = c(1, 0, 0))

barplot(table(herd$Income_Change2 ))

table(herd$Value_Change)
herd$Value_Change2 <- factor(herd$Value_Change, levels = c("Decreased", "Increased","Remained same"),
                              labels = c(1, 0, 0))
table(herd$Mob_Type)
herd$Mob_Type2 <- factor(herd$Mob_Type, levels = c("Alone", "In group"),
                             labels = c(1, 0))

table(herd$Pri_Livelihood)
herd$Pri_Livelihood2 <- factor(herd$Pri_Livelihood, levels = c("Nomadic Herding", "Sedentary Herding","other_primary"),
                         labels = c(1, 0,0))
table(herd$Pasture_Quality)
herd$Pasture_Quality2 <- factor(herd$Pasture_Quality, levels = c("declined", "Declined","improved","Improved","No change","no_change"),
                               labels = c(1, 1,0,0,0,0))


logit_model <- glm(
  Income_Change2 ~ Pri_Livelihood2 + Mob_Type2 + Value_Change2
    + Age + Dependents+ Pasture_Quality2,
   data = herd,
  family = binomial)

summary(logit_model )

logit_model2 <- glm(
  Income_Change2 ~ Value_Change2,
  data = herd,
  family = binomial)
summary(logit_model2)

library(ggplot2)

boxplot(herd$Age,
        main = "Boxplot of Herders' Age",
        ylab = "Age",
        col = "lightblue")

boxplot(herd$Dependents,
        main = "Boxplot of Herders' Dependents",
        ylab = "Number of Dependents",
        col = "lightgreen")

library(brms)
library(tidyverse)
prop.table(table(herd$Income_Change2))

# Fit Bayesian logistic regression

bayes_model <- brm(
  formula = Income_Change2 ~ Pri_Livelihood2 + Mob_Type2 + Value_Change2 + Age + Dependents + Pasture_Quality2,
  data = herd,
  family = bernoulli(link = "logit"),  # logistic regression
  prior = c(
    prior(normal(0, 5), class = "b"),        # weakly informative prior for coefficients
    prior(normal(0, 5), class = "Intercept") # prior for intercept
  ),
  chains = 4,    # number of MCMC chains
  iter = 4000,   # number of iterations per chain
  warmup = 1000, # burn-in
  seed = 123
)

summary(bayes_model)

# Plot posterior distributions of coefficients
plot(bayes_model)

# Conditional effects (predicted probabilities)
conditional_effects(bayes_model)



