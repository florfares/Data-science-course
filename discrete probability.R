#Final assesment in discrete probability.
library(gtools)
library(tidyverse)

#In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).
#q1How many different ways can the 3 medals be distributed across 8 runners?
permutations(8,3)
#q2How many different ways can the three medals be distributed among the 3 runners from Jamaica?
permutations(3,3)
#q3What is the probability that all 3 medals are won by Jamaica?
3/8*2/7*1/6

#q4Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

#For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
#Calculate the probability that all the runners are from Jamaica.
set.seed(1)
B<-10000
sim<-replicate(B,{
  game<-sample(runners,3)
  all(game=="Jamaica")
  
})
mean(sim)

#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.
#2a How many meal combinations are possible with the current menu?
s<-nrow(combinations(6,2))
nrow(expand.grid(c(1:6),c(1:s),c(1:2)))

#q2b How many combinations are possible if he expands his original special to 3 drink options?
nrow(expand.grid(c(1:6),c(1:s),c(1:3)))

#The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.
#q2c How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
s<-nrow(combinations(6,3))
nrow(expand.grid(c(1:6),c(1:s),c(1:3)))

#The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.
#Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#Use sapply() to apply the function to entree option counts ranging from 1 to 12.
#q2d What is the minimum number of entree options required in order to generate more than 365 combinations?
s<-nrow(combinations(6,2))
nrow(expand.grid(c(1:9),c(1:s),c(1:3)))

#The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
#Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.
#Use sapply() to apply the function to side counts ranging from 2 to 12.
#q2e What is the minimum number of side options required in order to generate more than 365 combinations?
s<-nrow(combinations(7,2))
nrow(expand.grid(c(1:6),c(1:s),c(1:3)))

##
data(esoph)
head(esoph)
library(tidyverse)
#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
#Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.
#q3a how many groups are in the dataset?   
nrow(esoph)

#q3b how many cases are in the dataset?
all_cases<-sum(esoph$ncases)

#q3c how many controls are there?
all_controls <-sum(esoph$ncontrols)

#q4a What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
#0.672

#q4bWhat is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
#0.0699 

#q4c Given that a person is a case, what is the probability that they smoke 10g or more a day?
1-(sum(esoph$ncases[esoph$tobgp=="0-9g/day"])/all_cases)

#q4d Given that a person is a control, what is the probability that they smoke 10g or more a day?
1-(sum(esoph$ncontrol[esoph$tobgp=="0-9g/day"])/all_controls)

#Q5A For cases, what is the probability of being in the highest alcohol group?
levels(esoph$alcgp)
halc_cases<-sum(esoph$ncases[esoph$alcgp=="120+"])/all_cases

#Q5BFor cases, what is the probability of being in the highest tobacco group?
levels(esoph$tobgp)
sum(esoph$ncases[esoph$tobgp=="30+"])/all_cases

#q5cFor cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
index<-esoph$tobgp=="30+" &esoph$alcgp=="120+"
sum(esoph$ncases[index])/all_cases

#q5dFor cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
index<- (esoph$tobgp=="30+" | esoph$alcgp=="120+")
hor_cases<-sum(esoph$ncases[index])/all_cases

#q6a For controls, what is the probability of being in the highest alcohol group?
levels(esoph$alcgp)
halc_controls<-sum(esoph$ncontrols[esoph$alcgp=="120+"])/all_controls
#0.0284

#q6bHow many times more likely are cases than controls to be in the highest alcohol group?
halc_cases/halc_controls
#7.93

#q6cFor controls, what is the probability of being in the highest tobacco group?
sum(esoph$ncontrols[esoph$tobgp=="30+"])/all_controls
#0.0658

#q6dFor controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
index<-esoph$tobgp=="30+" &esoph$alcgp=="120+"
sum(esoph$ncontrols[index])/all_controls
#0.00387

#q6eFor controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
index<- (esoph$tobgp=="30+" | esoph$alcgp=="120+")
hor_controls<-sum(esoph$ncontrols[index])/all_controls
#0.0903

#q6fHow many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
hor_cases/hor_controls
#3.65