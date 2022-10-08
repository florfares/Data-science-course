##FINAL ASSESSMENT of DATA VISUALIZATION
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
#q1
str(titanic)
?titanic_train

#q2
titanic %>% ggplot(aes(Age, y = ..count.., color=Sex, fill=Sex)) + 
  geom_density(alpha = 0.2, bw=2)

#q3
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

#q4
titanic %>% ggplot(aes(sample = Age)) +
  geom_qq(dparams=params)+
  geom_abline()

#q5
titanic %>% ggplot(aes(Survived, fill=Sex)) + 
  geom_bar(position = position_dodge())

#q6
titanic %>% ggplot(aes(Age, y = ..count.., fill=Survived)) + 
  geom_density(alpha = 0.2)

#q6
titanic %>% filter(!Fare==0) %>%
  ggplot(aes(Survived,Fare, fill=Survived)) + 
  geom_boxplot()+ geom_jitter(width = 0.1, alpha = 0.2)+
  scale_y_continuous(trans="log2")

#q7
titanic %>% ggplot(aes(Pclass, y=..count.., fill=Survived))+
  geom_bar()

titanic %>% ggplot(aes(Pclass, y=..count.., fill=Survived))+
  geom_bar(position = position_fill())

titanic %>% ggplot(aes(Survived, fill=Pclass))+
  geom_bar(position = position_fill())

#q8
titanic %>% ggplot(aes(Age,y=..count.., fill=Survived))+
  geom_density()+
  facet_grid(Sex~Pclass)