#Assessment: INTRODUCTION 
#q4
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#q5
?Teams

#q6
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

#q7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, R_per_game = R/G) %>%
  ggplot(aes( E_per_game,R_per_game)) + 
  geom_point(alpha = 0.5)

#Q8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes( X3B_per_game,X2B_per_game)) + 
  geom_point(alpha = 0.5)

#Assessment: Correlation
#q7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(R_per_game = R/G, AB_per_game = AB/G) %>%
  summarize(r=cor(R_per_game, AB_per_game))

#q8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  summarize(r=cor(W_per_game, E_per_game))

#q9
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  summarize(r=cor(X2B_per_game, X3B_per_game))

#Assessment Part 1, just theoretical questions
#Assessment: Stratification and Variance Explained, Part 2
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

str(female_heights)
#q8Calculate the mean and standard deviation of mothers' heights, the mean and standard deviation of daughters' heights, and the correlaton coefficient between mother and daughter heights.
mu_x <-mean(female_heights$mother)
s_x<-sd(female_heights$mother)
mu_y<-mean(female_heights$daughter)
s_y<-sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
r

#q9Calculate the slope and intercept of the regression line predicting daughters' heights given mothers' heights. Given an increase in mother's height by 1 inch, how many inches is the daughter's height expected to change?
m <-  r * s_y / s_x
b <- mu_y - m*mu_x

m
b

#q10 What percent of the variability in daughter heights is explained by the mother's height?
r^2*100

#q11 A mother has a height of 60 inches. Using the regression formula, what is the conditional expected value of her daughter's height given the mother's height?
b+m*60


#Assessment: Stratification

#Q1
#As described in the videos, when we stratified our regression lines for runs per game vs. bases on balls by the number of home runs, what happened?
#The slope of runs per game vs. bases on balls within each stratum was reduced because we removed confounding by home runs.

#q2
#lm(son ~ father, data = galton_heights)

#Call:
#  lm(formula = son ~ father, data = galton_heights)

#Coefficients:
#  (Intercept)    father  
#     35.71       0.50  

#Interpret the numeric coefficient for "father."
#For every inch we increase the father's height, the predicted son's height grows by 0.5 inches.

#q3
#galton_heights <- galton_heights %>%
#  mutate(father_centered=father - mean(father))
# > lm(son ~ father_centered, data = galton_heights)

#Call:
#  lm(formula = son ~ father_centered, data = galton_heights)

#Coefficients:
#  (Intercept)    father_centered  
#     70.45          0.50  

#The height of a son of a father of average height is 70.45 inches.

#q4
#b0+b1x1

#q5
#errors are independent from each other, mean=0 and variance is constant.


#Assessment: Least squares estimates
#q1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#q2 The least squares estimates for the parameters  b0, b1, b2... minimize the residual sum of squares.

#q3
library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game= BB/G, 
         HR_per_game = HR / G,
         R_per_game = R / G) 

str(dat) #to know the columns with the indep.var to use with lm
ncol(dat) #to know the columns with the indep.var to use with lm

#q4
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
#What does the central limit theorem tell us about the variables beta_0 and beta_1?
#They are approximately normally distributed.
#The expected value of each is the true value of b0 and b1 (assuming the Galton heights data is a complete population).

#q5
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#q7
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

str(female_heights)
model <-lm(mother ~ daughter, data=female_heights)

#q8
female_heights[,3] <- predict(model)
colnames(female_heights) <- c("mother","daughter","Fitted")

head(female_heights)

#q9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles=mean(singles), mean_bb=mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)


bat_9901 %>% filter(mean_singles>.2) %>% summarize(n())
bat_9901 %>% filter(mean_bb>.2) %>% summarize(n())

#q10
data<-inner_join(bat_02,bat_9901)
cor(data$singles,data$mean_singles)
cor(data$bb,data$mean_bb)

#q11
plot(data$singles,data$mean_singles)
plot(data$bb,data$mean_bb)

#q12
lm(singles ~ mean_singles, data=data)
lm(bb ~ mean_bb, data=data)

#Assessment:Advanced dplyr: summarize with functions and broom
#q1
#The lm() function does not know how to handle grouped tibbles.

#q2
#Data frames

#q3
#Tibbles display better.
#If you subset a tibble, you always get back a tibble.
#Tibbles can have complex entries.
#Tibbles can be grouped.

#q4
#It understands grouped tibbles.
#It always returns a type of data frame.

#q5
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

dat %>% 
  group_by(HR) %>% 
  summarize(get_slope(across()))

#q6
#A tibble

#q7

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

dat %>% 
  group_by(lgID) %>% 
  summarize(tidy(lm(R ~ HR, data = across()), conf.int = T)) %>% 
  filter(term == "HR") 


##part 2
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

str(galton)

#q8

galton %>%  filter(pair=="father_daughter") %>% summarize(n())
galton %>%  filter(pair=="mother_son") %>% summarize(n())

#q9
galton %>% group_by(pair) %>% summarize(cor(parentHeight,childHeight))

#q10
galton %>% group_by(pair) %>% 
  summarize(tidy(lm(childHeight~parentHeight, data=across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()


galton %>% group_by(pair) %>% 
  summarize(tidy(lm(childHeight~parentHeight, data=across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight") 

galton %>% group_by(pair) %>% 
  summarize(tidy(lm(childHeight~parentHeight, data=across()), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>% summarize(dif=conf.high-conf.low)

#Regression Fallacy


#Assessment: Part 1
pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean

#Assessment: Part 2
# Use the Teams data frame from the Lahman package.
# Fit a multivariate linear regression model to obtain the effects of BB
#  and HR on Runs (R) in 1971. Use the tidy function in the broom package
#  to obtain the results in a data frame.

#q9a
dat <- Teams %>% filter(yearID == 1971)
fit <- tidy(lm(R ~ BB + HR, data = dat))
fit

#q9b
#bb no sig., hr sig.

#q10
# Repeat the above exercise to find the effects of BB and HR on runs (R)
#  for every year from 1961 to 2018 using do and the broom package.
fit <- Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .)))

# Make a scatterplot of the estimate for the effect of BB on runs over time
#  and add a trend line with confidence intervals.
fit %>% filter(term == 'BB') %>% 
  select(yearID, estimate) %>%
  ggplot(aes(yearID, estimate)) + 
  geom_line() + geom_smooth(method="lm")


# Fit a linear model on the results from Question 10 to determine the effect
#  of year on the impact of BB.
fit<- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))

fit2 <- fit %>% filter(term == "BB")
fit3 <- lm(estimate ~ yearID, data= fit2)
tidy(fit3)
