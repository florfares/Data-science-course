#Assessment: asics of Evaluating Machine Learning Algorithms
#Q1: For each of the following, indicate whether the outcome is continuous or categorical
# digit reader >categorical
# height > continuous
# spam filter >categorical
# stock prices >continuous
# sex >categorical

#q2How many features are available to us for prediction in the mnist digits dataset?
dd<-read_mnist()
str(dd)
#784 pixeles

#Assessment: Confusion matrix part1
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#q1
str(dat)
total<-dat %>% count(type=="inclass")
total
fem_inclass <- dat %>% filter(type=="inclass") %>% count(sex=="Female") 
fem_inclass[2,2]/total[2,2]

fem_online<- dat %>% filter(type=="online") %>% count(sex=="Female")
fem_online[2,2]/total[1,2]

#q2
dat<-mutate(dat, predicted=ifelse(type=="inclass", "Female", ifelse(type=="online","Male","Female")))
y_hat<-factor(dat$predicted,  c("Female", "Male"))
str(y)
str(y_hat)
mean(y==y_hat)            

#q3
table(y_hat, y)

#q4#q5#q6
confusionMatrix(y_hat, y)

#Assessment part2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#q7
set.seed(76)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
test <- iris[test_index,]
train <- iris[-test_index,]

# #q8
# Next we will figure out the singular feature in the dataset that yields the greatest overall accuracy when predicting species. You can use the code from the introduction and from Q7 to start your analysis.
# Using only the train iris dataset, for each feature, perform a simple search to find the cutoff that produces the highest accuracy, predicting virginica if greater than the cutoff and versicolor otherwise. Use the seq function over the range of each feature by intervals of 0.1 for this search.
# Which feature produces the highest accuracy?
#   
str(train)

cutoff_sl<- seq(min(train$Sepal.Length), max(train$Sepal.Length), by=0.1)
cutoff_sw<- seq(min(train$Sepal.Width), max(train$Sepal.Width), by=0.1)
cutoff_pl<- seq(min(train$Petal.Length), max(train$Petal.Length), by=0.1)
cutoff_pw<- seq(min(train$Petal.Width), max(train$Petal.Width), by=0.1)


accuracy_sl <- map_dbl(cutoff_sl, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

accuracy_sw <- map_dbl(cutoff_sw, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

accuracy_pl <- map_dbl(cutoff_pl, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

accuracy_pw <- map_dbl(cutoff_pw, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff_pl, accuracy_pl) %>% 
  ggplot(aes(cutoff_pl, accuracy_pl)) + 
  geom_point() + 
  geom_line() 

best_cutoff[3] <-max(accuracy_pl)

data.frame(cutoff_pw, accuracy_pw) %>% 
  ggplot(aes(cutoff_pw, accuracy_pw)) + 
  geom_point() + 
  geom_line() 

best_cutoff[4] <-max(accuracy_pw)

data.frame(cutoff_sl, accuracy_sl) %>% 
  ggplot(aes(cutoff_sl, accuracy_sl)) + 
  geom_point() + 
  geom_line() 

best_cutoff[1] <-max(accuracy_sl)

data.frame(cutoff_sw, accuracy_sw) %>% 
  ggplot(aes(cutoff_sw, accuracy_sw)) + 
  geom_point() + 
  geom_line() 

best_cutoff[2] <-max(accuracy_sw)


# foo <- function(x){
#   rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
#   sapply(rangedValues,function(i){
#     y_hat <- ifelse(x>i,'virginica','versicolor')
#     mean(y_hat==train$Species)
#   })
# }
# predictions <- apply(train[,-5], 2, foo)
# dataa <- sapply(predictions,max)

#q9For the feature selected in Q8, use the smart cutoff value from the training data to calculate overall accuracy in the test data.
# What is the overall accuracy?

best_cutoff_pw <- cutoff_pw[which.max(accuracy_pw)]
accuracy_pw_test <- ifelse(test$Petal.Width > best_cutoff_pw , "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))

mean(accuracy_pw_test == test$Species)

#q10 Which feature produces the second highest accuracy?
best_cutoff_pl <- cutoff_pl[which.max(accuracy_pl)]
best_cutoff_pl

#q11
plot(iris, pch=21, bg=iris$Species)

accuracy_pwpl_test <- ifelse(test$Petal.Width > best_cutoff_pw & test$Petal.Length > best_cutoff_pl , "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))

mean(accuracy_pwpl_test == test$Species)


#Assesment: conditional probability
#q1
#nested bayes teorem
#P(A|B)=P(B|A)P(A)/P(B)
#P(disease | test+) = P(test+ | diseased) * P(diseased)/P(test+)
#P(test+) = P(test+|disease)P(disease) + P(test+|healthy)P(healthy)
0.85*0.02/(0.85*0.02 + 0.1*0.98) 

#q2
set.seed(1) 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(test==1)

#q3
mean(test==0 & disease==1)

#q4
mean(disease[test==1]==1)

# #q5
# Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
# If a patient's test is positive, by how many times does that increase their risk of having the disease?
# First calculate the probability of having the disease given a positive test, then divide by the probability of having the disease.

mean(disease[test==1]==1)/mean(disease==1)


#q6
library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#q7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#q8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


#Assessment: Linear Regression for Prediction
library(tidyverse)
library(caret)

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1
#We will build 100 linear models using the data above and calculate 
#the mean and standard deviation of the combined models. 
#First, set the seed to 1 again. Then, within a replicate() loop, 
#(1) partition the dataset into test and training sets with p = 0.5 and 
#using dat$y to generate your indices, (2) train a linear model predicting y from x, 
#(3) generate predictions on the test set, and 
#(4) calculate the RMSE of that model. Then, report the mean and standard deviation (SD) 
#of the RMSEs from all 100 models.

set.seed(1)
B<-100
RMES<- replicate(B,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat<- fit$coef[1]+fit$coef[2]*test$x
  sqrt(mean((y_hat - test$y)^2))
})

mean(RMES)
sd(RMES)

#Q2
#Now we will repeat the exercise above but using larger datasets. 
#Write a function that takes a size n, then 
#(1) builds a dataset using the code provided at the top of Q1 
#but with n observations instead of 100 and without the set.seed(1), 
#(2) runs the replicate() loop that you wrote to answer Q1, 
#which builds 100 linear models and returns a vector of RMSEs, and 
#(3) calculates the mean and standard deviation of the 100 RMSEs.

#Set the seed to 1 and then use sapply() or map() to apply your new function to 
#n <- c(100, 500, 1000, 5000, 10000).
#Note: You only need to set the seed once before running your function; 
#do not set a seed within your function. Also be sure to use sapply() or map() 
#as you will get different answers running the simulations individually due to setting the seed.

library(tidyverse)
library(caret)
set.seed(1)
hundred_models<-function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  B<-100
  RMES<- replicate(B,{
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit <- lm(y ~ x, data = train)
    y_hat<- fit$coef[1]+fit$coef[2]*test$x
    sqrt(mean((y_hat - test$y)^2))
  })
  c(mean(RMES),sd(RMES))
}

n <- c(100, 500, 1000, 5000, 10000)
sapply(n,hundred_models)

#q3
#On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases.

#q4
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1)
B<-100
RMES<- replicate(B,{
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat<- fit$coef[1]+fit$coef[2]*test$x
  sqrt(mean((y_hat - test$y)^2))
})

mean(RMES)
sd(RMES)
#sd=0.06

#q5
#When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y.

#q6
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
test <- dat[test_index,]
train <- dat[-test_index,]
fit_x1 <- lm(y ~ x_1, data = train)
fit_x2 <- lm(y ~ x_2, data = train)
fit_x12 <- lm(y ~ x_1 + x_2, data = train)
y_hatx1<- fit_x1$coef[1]+fit_x1$coef[2]*test$x_1
y_hatx2<- fit_x2$coef[1]+fit_x2$coef[2]*test$x_2
y_hatx12<- fit_x12$coef[1]+fit_x12$coef[2]*test$x_1+fit_x12$coef[3]*test$x_2

RMES_x1<-sqrt(mean((y_hatx1 - test$y)^2))
RMES_x2<-sqrt(mean((y_hatx2 - test$y)^2))
RMES_x12<-sqrt(mean((y_hatx12 - test$y)^2))

matrix(c("RMES_x1",RMES_x1,"RMES_x2",RMES_x2,"RMES_x12",RMES_x12),2,3)

#q7
c("RMES_x12",RMES_x12)

#q8
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE) 
test <- dat[test_index,]
train <- dat[-test_index,]
fit_x1 <- lm(y ~ x_1, data = train)
fit_x2 <- lm(y ~ x_2, data = train)
fit_x12 <- lm(y ~ x_1 + x_2, data = train)
y_hatx1<- fit_x1$coef[1]+fit_x1$coef[2]*test$x_1
y_hatx2<- fit_x2$coef[1]+fit_x2$coef[2]*test$x_2
y_hatx12<- fit_x12$coef[1]+fit_x12$coef[2]*test$x_1+fit_x12$coef[3]*test$x_2

RMES_x1<-sqrt(mean((y_hatx1 - test$y)^2))
RMES_x2<-sqrt(mean((y_hatx2 - test$y)^2))
RMES_x12<-sqrt(mean((y_hatx12 - test$y)^2))

matrix(c("RMES_x1",RMES_x1,"RMES_x2",RMES_x2,"RMES_x12",RMES_x12),2,3)
#Adding extra predictors can improve RMSE substantially, but not when the added predictors are highly correlated with other predictors.


#Assessment: Smoothing
#q1
#In the Wrangling course of this series, PH125.6x, we used the following code to obtain mortality counts for Puerto Rico for 2015-2018:
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

#Use the loess() function to obtain a smooth estimate of the expected number of deaths as a function of date. 
#Plot this resulting smooth function. Make the span about two months (60 days) long and use degree = 1.
#Which of the following plots is correct?
head(dat)
60/length(dat$date) #what to fill in span= in geom_smooth #span says the number of obs taken for the moving avarega as a percentage of the total number of observations. We have 1205 obs, and we want a moving average of 60 days.
dat %>% ggplot(aes(date, deaths)) +
  geom_point() +
  geom_smooth(method = loess, method.args=list(degree=1,span=0.05))

#q2
#Work with the same data as in Q1 to plot smooth estimates against day of the year, all on the same plot, but with different colors for each year.
#Which code produces the desired plot?
str(dat)
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#q3
library(broom)
library(caret)
library(dslabs)
library(tidyverse)

data(mnist_27)
str(mnist_27)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)

train_mnist<-mnist_27$train
train_mnist <- train_mnist %>% mutate(y = ifelse(y == 7, 1, 0))
test_mnist<-mnist_27$test
test_mnist <- test_mnist %>% mutate(y = ifelse(y == 7, 1, 0))

fit_2 <- train_mnist  %>% 
  loess(y ~ x_2, degree=1, data=.)
y_hat<-predict(fit_2, newdata=test_mnist)
y_hat<- as.numeric(ifelse(y_hat > 0.5, 1, 0))
confusionMatrix(as.factor(y_hat), as.factor(test_mnist$y))$overall[["Accuracy"]]

##k-Nearest Neighbors (kNN)
library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)

data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()

knn_fit <- knn3(y ~ ., data = mnist_27$train)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
grid.arrange(p2, p1, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]


#Assessment: Overtraining and Oversmoothing
#q1
library(tidyverse)
library(caret)
library(dslabs)
data("heights")

#create train and test sets
set.seed(1)
index<- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE) 
heights_test<-heights[-index,]
heights_train<-heights[index,]

F_1 <- function(x){
  knn_fit<- knn3(sex ~ . , data = heights_train, k = x)
  y_hat <- predict(knn_fit, heights_test, type = "class")
  F_meas(data = y_hat, reference = heights_test$sex)
  
}

k <- seq(1,101,3)
scores <-sapply(k,F_1)

df<-data.frame(k, scores)
max(scores)
min(df$k[scores==max(scores)])

data.frame(k, scores) %>% 
  ggplot(aes(k, scores)) + 
  geom_point() + 
  geom_line()


#q2
library(tidyverse)
library(dslabs)
library(caret)
library(stringr)
data("tissue_gene_expression")
set.seed(1)
x<-data.frame(tissue_gene_expression$x)
tge<-data.frame(y=rownames(x),x)
tge$y<-as.factor(str_replace(tge$y, '\\_\\d+', ""))

index<- createDataPartition(tge$y, times = 1, p = 0.5, list = FALSE) 
test_set<-tge[index,]
train_set<-tge[-index,]

accuracy <- function(x){
  knn_fit<- knn3(y ~ . , data = train_set, k = x)
  y_hat <- predict(knn_fit, test_set, type = "class")
  confusionMatrix(y_hat, test_set$y)$overall[["Accuracy"]]
}

k <- seq(1,11,2)
scores <-sapply(k,accuracy)

df<-data.frame(k, scores)
df


#Assessment : bootstrap
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

#q1
length(which(indexes$Resample01 == 3))
length(which(indexes$Resample01 == 4))
length(which(indexes$Resample01 == 7))

#q2
c<-seq(1,10,1)
c
indexess<-data.frame(indexes)
foo<- function(x){
  length(which(indexess[,x] == 3))
}
m<-sapply(c, foo)
sum(m)

#q3
set.seed(1)
B<-10000
df<-replicate(B,{
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(df)
sd(df)

#q4
set.seed(1)
y <- rnorm(100, 0, 1)
set.seed(1)
df <- replicate(B, {
  y_sample <- sample(y, 100, replace = TRUE)
  quantile(y_sample, 0.75)
})
mean(df)
sd(df)

#q5
#When doing bootstrap sampling, the simulated samples are drawn from the empirical distribution of the original data.
#True: The bootstrap is particularly useful in situations when we do not have access to the distribution or it is unknown.

#assessment: The caret package & smoothed LOESS fitting
#q1
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results

#q2
pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}
pvals
ind<-pvals<0.01
sum(ind)

#q3
col<-which(ind==T) #crear un vector que tenga el nro de columna de x que pvals<0.01
x_subset <- x[,col] #crear el subset de X
set.seed(1)
fit <- train(x_subset, y, method = "glm")
fit$results  #be careful, in R does not provide the correct result, I suggest trying the same code in posit (https://posit.cloud/)

#q4
set.seed(1)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#q5
#In the previous exercises, we see that despite the fact that x and y are completely independent, we were able to predict y with accuracy higher than 70%. We must be doing something wrong then.
#What is it?
#We used the entire dataset to select the columns used in the model.

#Assessment: other algorithms

library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#q1
fit <- rpart(y ~ ., data = dat) 

#q2
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#q4
library(randomForest)
fit <-  randomForest(y ~ x, data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#q5
plot(fit)

#q6
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

##Caret Package Assessment
#q1
library(rpart)
library(caret)
library(dslabs)
data(tissue_gene_expression)
set.seed(1991)

fit<-train(tissue_gene_expression$x, tissue_gene_expression$y,
           method = "rpart",
           tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

plot(fit)

#q2  
library(rpart)
library(caret)
library(dslabs)
data(tissue_gene_expression)
set.seed(1991)

fit<-train(tissue_gene_expression$x, tissue_gene_expression$y,
           method = "rpart", control = rpart.control(minsplit = 0),
           tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

plot(fit)
fit$resample$Accuracy

#q3
plot(fit$finalModel)
text(fit$finalModel)

#q4
set.seed(1991)
mtry <- seq(50, 200, 25)
tunegrid <- expand.grid(.mtry=mtry)
fit<-train(tissue_gene_expression$x, tissue_gene_expression$y,
           method = "rf", control = rpart.control(minsplit = 0),
           tuneGrid=tunegrid, nodesize = 1)
print(fit)


#Titanic Assessment
#install.packages("titanic")
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

str(titanic_clean)
#q1
set.seed(42)
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE) 
titanic_test <- titanic_clean[test_index,]
titanic_train<- titanic_clean[-test_index,]

nrow(titanic_test)
nrow(titanic_train)

sum(titanic_train$Survived==1)/nrow(titanic_train)

#q2
set.seed(3)
smpl<-sample(c(0,1),nrow(titanic_test),replace=T)
mean(smpl==titanic_test$Survived)

#q3a
ind_female <- titanic_train$Sex=="female"
mean(titanic_train$Survived[ind_female]==1)
mean(titanic_train$Survived[!ind_female]==1)

#q3b
sex_guess <- ifelse(titanic_test$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_guess == titanic_test$Survived)    # calculate accuracy

#q4a
titanic_train %>% group_by(Pclass) %>% summarize(avg=mean(Survived==1))

#q4b
class_guess <- ifelse(titanic_test$Pclass == 1, 1, 0)    # predict Survived=1 if class=1, or Survived==0 otherwise
mean(class_guess == titanic_test$Survived)   

#q4c
titanic_train %>% group_by(Pclass, Sex) %>% summarize(avg=mean(Survived==1))

#q4d
sexclass_guess <- ifelse(titanic_test$Sex == "female" & (titanic_test$Pclass==1 | titanic_test$Pclass==2), 1, 0)
mean(sexclass_guess == titanic_test$Survived)  

#q5a-b
confusionMatrix(data=factor(sex_guess), reference=factor(titanic_test$Survived))$byClass[c("Sensitivity","Specificity", "Balanced Accuracy")]
# 'Positive' Class : 0   
confusionMatrix(data=factor(class_guess), reference=factor(titanic_test$Survived))$byClass[c("Sensitivity","Specificity", "Balanced Accuracy")]
confusionMatrix(data=factor(sexclass_guess), reference=factor(titanic_test$Survived))$byClass[c("Sensitivity","Specificity", "Balanced Accuracy")]

#q6
F_meas(data = as.factor(sex_guess), reference = titanic_test$Survived)
F_meas(data = as.factor(class_guess), reference = titanic_test$Survived)
F_meas(data = as.factor(sexclass_guess), reference = titanic_test$Survived)

#q7
str(titanic_train)
set.seed(1)
train_loess <- train(Survived ~ Fare, 
                     method = "gamLoess",
                     data = titanic_train)

confusionMatrix(data = predict(train_loess, titanic_test), 
                reference = titanic_test$Survived)$overall["Accuracy"]

#q8
set.seed(1)
train_glm <- train(Survived ~ Age, 
                   method = "glm",
                   data = titanic_train)

confusionMatrix(data = predict(train_glm, titanic_test), 
                reference = titanic_test$Survived)$overall["Accuracy"]


train_glm2 <- train(Survived ~ Age + Pclass + Fare + Sex , 
                    method = "glm",
                    data = titanic_train)

confusionMatrix(data = predict(train_glm2, titanic_test), 
                reference = titanic_test$Survived)$overall["Accuracy"]

train_glm2 <- train(Survived ~ . , 
                    method = "glm",
                    data = titanic_train)

confusionMatrix(data = predict(train_glm2, titanic_test), 
                reference = titanic_test$Survived)$overall["Accuracy"]

#q9a
set.seed(6)
fit <- train(Survived ~ ., data=titanic_train, method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)))
fit$results$k[which.max(fit$results$Accuracy)]

#q9b
ggplot(fit)


fit$results$Accuracy[fit$results$k==7]
fit$results$Accuracy[fit$results$k==11]
fit$results$Accuracy[fit$results$k==15]
fit$results$Accuracy[fit$results$k==21]
fit$results$Accuracy[fit$results$k==23]

#q9c
confusionMatrix(predict(fit, titanic_test), titanic_test$Survived)$overall["Accuracy"]

#q10
control <- trainControl(method="cv", number=10, p=0.9)
set.seed(8)
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = titanic_train,
                   trControl = control,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

cv_knn_preds <- predict(train_knn, titanic_test)
mean(cv_knn_preds == titanic_test$Survived)

#q11a
set.seed(10)
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     data = titanic_train,
                     tuneGrid = data.frame(cp=seq(from=0, to=0.05, by=0.002)))
train_rpart$bestTune

rpart_preds <- predict(train_rpart, titanic_test)
mean(rpart_preds == titanic_test$Survived)

#q11b
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)

#q11c
#interpret the decisiontree.

#q12 posit.cloud
set.seed(14)
fit <- train(Survived ~ .,
             method = "rf",
             ntree=100,
             data=titanic_train,
             tuneGrid = data.frame(mtry=seq(1, 7, 1)))
plot(fit)

fit$bestTune
rf_preds <- predict(fit, titanic_test)
mean(rf_preds == titanic_test$Survived)


#Assessment: MNIST case study: knn
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")
library(caret)
library(dslabs)
library(tidyverse)
library(purrr)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
head(fits)

models_predic <- sapply(fits, function(fits){ 
  predict(fits, mnist_27$test)
}) 


dim(models_predic)
str(models_predic)

#q3 accuracy
accuracy <- ifelse(models_predic==mnist_27$test$y,1,0)
mean(accuracy)

#q4
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

majority_preds <- apply(models_predic, 1, getMode)
ensemble<- mean(ifelse(majority_preds == mnist_27$test$y, 1, 0))

#q5
accuracy_permodel<-colSums(accuracy)/nrow(models_predic)
accuracy_permodel>ensemble

#q6
accuracy_train <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(accuracy_train)

#q7
models <- c("glm", "naive_bayes", "knn", "gamLoess", "qda", "rf")

set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

fits$glm$results$Accuracy
fits$naive_bayes$results$Accuracy
fits$knn$results$Accuracy
fits$gamLoess$results$Accuracy
fits$qda$results$Accuracy
fits$rf$results$Accuracy
(0.8010386 + 0.8179446 + 0.8229989 + 0.8412514 + 0.8321801 + 0.8144985)/6


#Assessment: Modeling movie effects
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
str(movielens)

#q1
movielens %>% group_by(year) %>%
  summarize(n_ratings = n()) %>%
  print(n=82) %>%
  ggplot(aes(year, sqrt(n_ratings))) +
  geom_line()

#q2
# What is the average rating for the movie The Shawshank Redemption?
movielens %>% filter(title=="Shawshank Redemption, The") %>%
  summarize(mean_rating = mean(rating))

# What is the average number of ratings per year for the movie Forrest
#  Gump?
movielens %>% filter(title=="Forrest Gump") %>%
  summarize(n_ratings_per_year = n()/(2018-1994))

#q3
movielens %>% filter(year >= 1993) %>%
  group_by(movieId) %>%
  mutate(n_ratings_per_year = n()/(2018-1994)) %>%
  mutate(mean_rating = mean(rating)) %>% 
  ggplot(aes(n_ratings_per_year, mean_rating)) + 
  geom_point() + 
  geom_smooth()

#q4
#Fill in the missing values with a lower value than the average rating across all movies.
#Because a lack of ratings is associated with lower ratings, 
#it would be most appropriate to fill in the missing value with a lower value 
#than the average. You should try out different values to fill in the missing value 
#and evaluate prediction in a test set.

#q5
movielens <- mutate(movielens, date = as_datetime(timestamp))
head(movielens)

#q6
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
#There is some evidence of a time effect on average rating.

#q7
# with f a smooth function of d_u,i

#q8
movielens %>% group_by(genres) %>% 
  filter(length(rating) >= 1000) %>%
  summarize(avg = mean(rating), sd = sd(rating)) %>%
  filter(avg < 3.5) %>%
  ggplot(aes(x=genres, ymin=avg-sd, ymax=avg+sd)) + 
  geom_errorbar()

#q9
#, with X^k_u,i if  g_u,i is genre k


#Assessment: Model Fitting and Recommendation Systems - Regularization
options(digits=7)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#q1
top10_schools <- schools %>% top_n(10, score) %>% arrange(desc(score))
top10_schools
top10_schools$id[1]
top10_schools$score[10]

#q2
median(schools$size)
median(top10_schools$size)

#q3
btm10_schools <- schools %>% top_n(10, -score) %>% arrange(desc(score))
median(btm10_schools$size)

#q4
ggplot() +
  geom_point(data=schools, aes(x=size, y=score), color="blue") +
  geom_point(data=top10_schools, aes(x=size, y=score), color="red")
#The standard error of the score has larger variability when the school is smaller, 
#which is why both the best and the worst schools are more likely to be small.

#q5

overall <- mean(sapply(scores, mean))
schools <- schools %>% mutate(reg = overall+size*(score-overall)/(size+25))
schools %>% top_n(10, score) %>% arrange(desc(reg))


#q6
alphas <- seq(10, 250, 1)
RMSE <- function(alpha){
  temp <- schools %>%
    mutate(score_reg = overall + size*(score - overall)/(size + alpha))
  sqrt(mean((temp$quality - temp$score_reg)^2))
}

rmse <- sapply(alphas, RMSE)
plot(alphas, rmse)
alphas[which.min(rmse)]

#q7
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#q8
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]  


#Assessment: Matrix Factorization
library(tidyverse)
set.seed(1987, sample.kind="Rounding")
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#The students that test well are at the top of the image and there seem to be three groupings by subject.

#q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
#There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.

#q3
s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))
sum(colSums(y^2))

#q4
ss_y <- colSums(y^2)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
ggplot() +
  geom_point(aes(x=1:24, y=ss_y), color="blue") +
  geom_point(aes(x=1:24, y=ss_yv), color="red")
#ss_yv is decreasing and close to 0 for the 4th column and beyond.

#q5
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

#q6
identical(t(s$u %*% diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% t(diag(s$d)), sweep(s$u, 2, s$d, FUN = "*"))
identical(s$u %*% diag(s$d), sweep(s$u, 2, s, FUN = "*"))
