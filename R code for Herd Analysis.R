herd<-read.csv("data/Herd_data_2.csv")
View(herd)
table(herd$Income_Change,herd$Pri_Livelihood)
herd$Income_Change2 <- factor(herd$Income_Change, levels = c("Decreased", "Increased","Remained same"),
                              labels = c(1, 0, 0))
table(herd$Value_Change)
herd$Value_Change2 <- factor(herd$Value_Change, levels = c("Decreased", "Increased","Remained same"),
                             labels = c(1, 0, 0))
table(herd$Mob_Type)
herd$Mob_Type2 <- factor(herd$Mob_Type, levels = c("Alone", "In group"),
                         labels = c(1, 0))

table(herd$Pri_Livelihood)
herd$Pri_Livelihood2 <- factor(herd$Pri_Livelihood, levels = c("Nomadic Herding", "Sedentary Herding","other_primary"),
                               labels = c(1, 2,3))
table(herd$Pasture_Quality)
herd$Pasture_Quality2 <- factor(herd$Pasture_Quality, levels = c("declined", "Declined","improved","Improved","No change","no_change"),
                                labels = c(1, 1,0,0,0,0))


logit_model <- glm(
  Income_Change ~ Pri_Livelihood + Mob_Type + Value_Change
  + Age + Dependent+ Pasture_Quality,
  data = herd,
  family = binomial)

summary(logit_model )
