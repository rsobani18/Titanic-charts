options(digits = 3) #report 3 significant digits
library(tidyverse)
library(titanic)

#define the titanic dataset 
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived), 
         Pclass = factor(Pclass), 
         Sex = factor(Sex))

titanic %>%
  filter(Sex %in% c("female", "male")) %>%
  count(Sex)

age_40 <- titanic %>%
  filter(Age == 40 & Sex %in% c("female", "male")) %>%
  count(Sex)
age_40

max_age <- titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  summarise(max = max(Age))
max_age

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x= Age, group= Sex, fill = Sex)) +
  geom_density(alpha = 0.2, bw= 10)

params <- titanic %>%
    filter(!is.na(Age)) %>%
  summarise(mean = mean(Age), sd = sd(Age))

p<- titanic %>%
  ggplot(aes(sample= Age)) +
  geom_qq(dparams = params) +
  geom_abline()
p

p <- titanic %>%
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
p

p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)
p

box_plot <- titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) +
  geom_jitter()
box_plot

bar_plot1 <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()
bar_plot1

bar_plot2<- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())
bar_plot2

bar_plot3 <- titanic %>%
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())
bar_plot3

surv_density_grid <- titanic %>%
  ggplot (aes(x = Age, y= ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass ~ Sex)
surv_density_grid