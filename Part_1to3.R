#explore content in a dataset
str(gapminder)

#see class
class(gapminder)

#columnsnames
namescol(gapminder)

#lenght of elements
length()


#sOLVING BASIC MATH AND CALCULATIONS
# assigning values to variables
a <-2
b <--1
c <--4

# solving the quadratic equation
(-b + sqrt(b^2 - 4*a*c))/(2*a)
(-b - sqrt(b^2 - 4*a*c))/(2*a)

?log

log(1024,4)

# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2]
codes[c(1,3)]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

x <- c(1, "canada", 3)
x
y<-seq(1:10)
y
as.character(y)


name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

time<-time/60 #minutes to hours
speed<-distance/time 

my_df<-data.frame(names=name,time=time,distance=distance,speed=speed)


#INDEXING
library(tidyverse)
library(dslabs)
murders<-data(murders)

# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less than or equal to 0.71
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]




#WHICH MATCH
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE 
#(es como un vlookup, que devuelve el indice de la posicion 
#donde esta localizado lo que estoy buscando)
#The function which() gives us the entries of a logical vector that are true.

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
# match returns a vector of the positions of (first) 
# matches of its first argument in its second.
#The function match() looks for entries in a vector and returns the index needed to access them.
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state



#MUTATE, SELECT, FILTER
# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)
new_table


# creating a data frame with stringAsFactors = FALSE (DATAFRAME convierte todo en factor, para evitarlo usar stringAsFactors)
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
grades



 #PLOTS
library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)


#SUMMARIZING
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

# accessing the components with the accessor $
s$median
s$maximum

# average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate




# minimum, median, and maximum murder rate for the states in the West region using quantile
# note that this returns a vector c(0=minimum, 0.5=median, 1=maximum)
murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))

# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))



#pull convierte data.frame en numeric
# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# us_murder_rate is now stored as a number
class(us_murder_rate)



#using dot to replace data
#average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# using the dot to access the rate
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate
class(us_murder_rate)


#group_by
# group by region
murders %>% group_by(region)

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))


#sorting and showing
# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate)

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)


#Exercise
#Complete the line of code to save the average and standard deviation of 
#systolic blood pressure as average and standard_deviation to a variable 
#called ref.
#Use the summarize function after filtering for 20-29 year old females and 
#connect the results using the pipe %>%. When doing this remember there are 
#NAs in the data!

install.packages(NHANES)
library(dplyr)
library(NHANES)
data(NHANES)
## complete this line of code. Para separar los NA, poner na.rm
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average=mean(BPSysAve, na.rm=T) , standard_deviation=sd(BPSysAve, na.rm=T))



#DATA.TABLE PACKAGE
# install the data.table package before you use it!
install.packages("data.table")

# load data.table package
library(data.table)

# load other packages and datasets
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

# convert the data frame into a data.table object
murders <- setDT(murders)

# selecting in dplyr
select(murders, state, region)

# selecting in data.table - 2 methods
murders[, c("state", "region")] |> head()
murders[, .(state, region)] |> head()

# adding or changing a column in dplyr
murders <- mutate(murders, rate = total / population * 10^5)

# adding or changing a column in data.table
murders[, rate := total / population * 100000]
head(murders)
murders[, ":="(rate = total / population * 100000, rank = rank(population))]
head(murders)
# y is referring to x and := changes by reference
x <- data.table(a = 1)
y <- x

x[,a := 2]
y

y[,a := 1]
x

# use copy to make an actual copy
x <- data.table(a = 1)
y <- copy(x)
x[,a := 2]
y


##Compare with dplyr
murders <- mutate(murders, rate = total / population * 10^5)
murders[, rate := total / population * 100000]

# subsetting in dplyr
filter(murders, rate <= 0.7)

# subsetting in data.table
murders[rate <= 0.7]

# combining filter and select in data.table
murders[rate <= 0.7, .(state, rate)]

# combining filter and select in dplyr
murders %>% filter(rate <= 0.7) %>% select(state, rate)



##Comparing Summarizing and grouping between dplyr and data.table
# summarizing in dplyr
s <- heights %>% 
  summarize(average = mean(height), standard_deviation = sd(height))

# summarizing in data.table
s <- heights[, .(average = mean(height), standard_deviation = sd(height))]

# subsetting and then summarizing in dplyr
s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

# subsetting and then summarizing in data.table
s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]

# previously defined function
median_min_max <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
}

# multiple summaries in data.table
heights[, .(median_min_max(height))]

# grouping then summarizing in data.table
heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]



##Sorting  ((|> en lugar de %>%))
murders[, rate := total / population * 100000]

# order by population
murders[order(population)] |> head()

# order by population in descending order
murders[order(population, decreasing = TRUE)] 

# order by region and then murder rate
murders[order(region, rate)]


#Assesment

#Q1
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

average <- summarize(heights, average=mean(height))
ind <-  pull(average) < heights$height
heights$height[ind]
sum(ind)

#Q2
average <- mean(heights$height)
above <- average < heights$height
fem <- heights$sex=="Female"
ind <- above & fem
heights$height[ind] 
sum(ind)

#Q3

heights %>% 
  group_by(sex) %>% 
  summarise(n = n()) %>% 
  mutate(Freq = n/sum(n))

#Q4a
minimum <- heights %>%
  summarize(minimum=min(height)) 

#Q4b
# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(minimum, heights$height)
# match returns a vector of the positions of (first) 
# matches of its first argument in its second.
#The function match() looks for entries in a vector and returns the index needed to access them.
index

#Q4c
heights$sex[index]

#Q5a
maximum <- heights %>%
  summarize(maximum=max(height)) 
maximum

#Q5b
x<-50:82

#Q5c
no_inclu <- x %in% heights$height
sum(!(no_inclu))

#Q6
heights2<- mutate(heights, ht_cm=height*2.54)

#Q6a
heights2$ht_cm[18]

#Q6b
heights2 %>% 
  summarize(average_cm=mean(ht_cm)) %>%
  pull(average_cm)

#Q7
females <- heights2 %>% 
  filter(sex=="Female") %>%
  data.frame()
head(females)

#Q7a
nrow(females)

#q7B
females %>% 
  summarize(average_cm=mean(ht_cm)) %>%
  pull(average_cm)

#Q8
library(dslabs)
data(olive)
head(olive)

#Q8a
plot(olive$palmitic, olive$palmitoleic)

#Q9
hist(olive$eicosenoic)

#q10
boxplot(palmitic~region,data=olive)




#Basic programming
##
# an example showing the general structure of an if-else statement
a <- 0 
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}

# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}

# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)


##functions
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
#my_function <- function(VARIABLE_NAME){
 # perform operations on VARIABLE_NAME and calculate VALUE
  #VALUE
#}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
avg(x, T)
avg(x, F)


##LOOPS
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
for(i in 1:5){
  print(i)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)


#Assement in Datacamp
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}


x<-c(FALSE,FALSE,FALSE)
all(!x)

# Assign the state abbreviation when the state name is longer than 8 characters 
new_names <- ifelse(nchar(murders$state)>8,murders$abb,murders$state) 

# Create function called `sum_n`
sum_n <- function(n){
  x <- 1:n
  sum(x)
}

# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)



##DATA VISUALIZATION IN R
names(heights) #para saber el nombre de las variables de la base
unique(heights$height) #me dice los elementos que son unicos


#In the previous exercise we computed the variable tab which 
#reports the number of times each unique value appears. 
#For values reported only once tab will be 1. Use logicals and 
#the function sum to count the number of times this happens.
library(dslabs)
data(heights)
tab <- table(heights$height)
ind<-tab==1
sum(tab[ind])


#quantiles, percentiles and quartiles
library(dslabs)
data(heights)
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#qnorm (quantiles of normal distribution)
qnorm(p, mu, sigma)
qnorm(p)


#The result of pnorm() is the quantile. Note that:
pnorm(-1.96)
qnorm(0.025)

#qnorm() and pnorm() are inverse functions:
pnorm(qnorm(0.025)) 
  
#theoretical quantiles assuming a normal distribution
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)



# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x) #esta reescalando X como una dist normal.

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)  

library(dslabs)
data(heights)
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
c<-c(0.1,0.3,0.5,0.7, 0.9)
female_percentiles<-quantile(female,c)
male_percentiles<-quantile(male,c)
df<-data.frame(female=female_percentiles, male=male_percentiles)
df



##GGPLOT

library(tidyverse)
library(dslabs)
data(murders)

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

# change the size of the points
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# move text labels slightly to the right
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# simplify code by adding global aesthetic
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# local aesthetics override global aesthetics
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))



p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# log base 10 scale the x-axis and y-axis
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# efficient log scaling of the axes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()


p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")


# redefine p to be everything except the points layer
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010")

# make all points blue
p + geom_point(size = 3, color = "blue")

# color points by region
p + geom_point(aes(col = region), size = 3)


# define average murder rate
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# basic line with average murder rate for the country
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))    # slope is default of 1

# change line to dashed and dark grey, line under points
p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)


p <- p + scale_color_discrete(name = "Region")    # capitalize legend title




# theme used for graphs in the textbook and course
library(dslabs)
ds_theme_set()

# themes from ggthemes
library(ggthemes)
p + theme_economist()    # style of the Economist magazine
p + theme_fivethirtyeight()    # style of the FiveThirtyEight website


##FINAL PLOT
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

#HISTOGRAMS
# load heights data
library(tidyverse)
library(dslabs)
data(heights)

# define p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

# basic histograms
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histogram with blue fill, black outline, labels and title
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")


#density
p + geom_density()
p + geom_density(fill = "blue")


# basic QQ-plot
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


#grids of plots
# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)


##GAPMINDER

# load and inspect gapminder data
library(dslabs)
data(gapminder)
head(gapminder)

# compare infant mortality in Sri Lanka and Turkey
gapminder %>%
  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)


# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

# add color as continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point() 



# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea", "Germany")
# line plot fertility time series for two countries - one line per country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

# fertility time series for two countries - lines colored by country
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()


labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")



#scales
# add dollars per day variable
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

# histogram of dollars per day
past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")


# number of regions
length(levels(gapminder$region))

# boxplot of GDP by region in 1970
past_year <- 1970
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p + geom_boxplot()

# rotate names on x-axis
p + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)



# reorder by median income and color by continent
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("")
p

# log2 scale y-axis
p + scale_y_continuous(trans = "log2")

# add data points
p + scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)


##faceting
past_year <- 1970
# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# facet by West vs devloping
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ group)

# facet by West/developing and year
present_year <- 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


#some countries were created post 1970, so we keep only countries in both years
# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)



p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))



# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()

# jittered, alpha blended point plot
heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.1, alpha = 0.2)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))


# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)


# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)



# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 


##exercise9
data(gapminder)
gapminder %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(continent=="Africa" & (year==1970 | year==2010)  & !is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day)) + 
  geom_density() + 
  scale_x_continuous(trans = "log2")  +
  facet_grid(year ~ .)


gapminder<-gapminder %>%
  mutate(dollars_per_day=gdp/population/365) %>%
  filter(continent=="Africa" & (year==1970 | year==2010)  & !is.na(dollars_per_day)) %>%
ggplot(aes(dollars_per_day, fill=region)) + 
  geom_density(bw=0.5, position = "stack") + 
  scale_x_continuous(trans = "log2")  +
  facet_grid(year ~ .)

dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()



library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")


library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")


# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")


# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

#para redondear numeros o decimales
round()
signif()

##FINAL EXERCISE FROM DATA VISUALIZATION
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


##Probability

#discret probability 

# joining strings with paste
number <- "Three"
suit <- "Hearts"
paste(number, suit)

# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

# generating combinations of 2 vectors with expand.grid
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))


suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

hands <- permutations(52,2, v = deck)
first_card <- hands[,1]
second_card <- hands[,2]
sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands

# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))



# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays




# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

x <- 1:10
sqrt(x)    # sqrt operates on each element of the vector

y <- 1:10
x*y    # * operates element-wise on both vectors

compute_prob(n)    # does not iterate over the vector n without sapply

x <- 1:10
sapply(x, sqrt)    # this is equivalent to sqrt(x)

prob <- sapply(n, compute_prob)    # element-wise application of compute_prob to n
plot(n, prob)


# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 





#EXERCISES IN DATACAMP

#Exercise 4. Probability the Celtics win a game
#Two teams, say the Celtics and the Cavs, are playing a seven game series. The Cavs are a better team and have a 60% chance of winning each game.

#What is the probability that the Celtics win at least one game? Remember that the Celtics must win one of the first four games, or the series will be over!

#Calculate the probability that the Cavs will win the first four games of the series.
#Calculate the probability that the Celtics win at least one game in the first four games of the series.

# Assign the variable `p_cavs_win4` as the probability that the Cavs will win the first four games of the series.
p_cavs_win4 <- 0.6^4

# Using the variable `p_cavs_win4`, calculate the probability that the Celtics win at least one game in the first four games of the series.
1 - p_cavs_win4


#Exercise 5. Monte Carlo simulation for Celtics winning a game
#Create a Monte Carlo simulation to confirm your answer to the previous problem by estimating how frequently the Celtics win at least 1 of 4 games. Use B <- 10000 simulations.

#The provided sample code simulates a single series of four random games, simulated_games.

#Use the replicate function for B <- 10000 simulations of a four game series. The results of replicate should be stored to a variable named celtic_wins.
#Within each simulation, replicate the sample code to simulate a four-game series named simulated_games. Then, use the any function to indicate whether the four-game series contains at least one win for the Celtics. Perform these operations in two separate steps.
#Use the mean function on celtic_wins to find the proportion of simulations that contain at least one win for the Celtics out of four games.

# This line of example code simulates four independent random games where the Celtics either lose or win. Copy this example code to use within the `replicate` function.
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))

# The variable 'B' specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)  ##va junto con el sample <- este me tira la muestra aleatoria.

# Create an object called `celtic_wins` that replicates two steps for B iterations: (1) generating a random four-game series `simulated_games` using the example code, then (2) determining whether the simulated series contains at least one win for the Celtics.
celtic_wins  <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})

# Calculate the frequency out of B iterations that the Celtics won at least one game. Print your answer to the console.
mean(celtic_wins)

#celtic_wins es un vector que tiene guardados 10000 elementos. 
#cada uno de los elementos dice true o false dependiendo de si en la muestra
#aleatoria en alguno de los 4 intentos le salio win, teniendo en cuenta que 
#al ser un peor equipo, la probabilidad de tener win es del 40%,


B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})
mean(stick)    # probability of choosing prize door when sticking


switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching



#DATACAMP ASSESMENT MONTYHALL

#Two teams, say the Cavs and the Warriors, are playing a seven game championship series. The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?
#Assign the number of remaining games to the variable n.
#Assign a variable outcomes as a vector of possible outcomes in a single game, where 0 indicates a loss and 1 indicates a win for the Cavs.
#Assign a variable l to a list of all possible outcomes in all remaining games. Use the rep function to create a list of n games, where each game consists of list(outcomes).
#Use the expand.grid function to create a data frame containing all the combinations of possible outcomes of the remaining games.
#Use the rowSums function to identify which combinations of game outcomes result in the Cavs winning the number of games necessary to win the series.
#Use the mean function to calculate the proportion of outcomes that result in the Cavs winning the series and print your answer to the console.

# Assign a variable 'n' as the number of remaining games.
n<-6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes <-c(0,1)

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the `rep` function on `list(outcomes)` to create list of length `n`.
l<-rep(list(outcomes),n)

# Create a data frame named 'possibilities' that contains all combinations of possible outcomes for the remaining games.
possibilities<-expand.grid(l)

# Create a vector named 'results' that indicates whether each row in the data frame 'possibilities' contains enough wins for the Cavs to win the series.
results<-rowSums(possibilities)

# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results>=4)


##REPLICAR LO ANTERIOR CON UN MONTECARLO
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# Create an object called `results` that replicates for `B` iterations a simulated series and determines whether that series contains at least four wins for the Cavs.
results <-replicate(B,{
  game<-sample(c(0,1),6, replace = TRUE)
  sum(game)>=4
})
# Calculate the frequency out of `B` iterations that the Cavs won at least four games in the remainder of the series. Print your answer to the console.
mean(results)




#Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a P>0.5 chance of winning each game.
#Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
#Then plot the result plot(p, Pr)

# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr<-sapply(p,prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)


#Repeat the previous exercise, but now keep the probability that team  wins fixed at p <- 0.75 and compute the probability for different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.
#Use the seq function to generate a list of odd numbers ranging from 1 to 25.
#Use the function sapply to compute the probability, call it Pr, of winning during series of different lengths.
#Then plot the result plot(N, Pr).

p<-0.75
# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

# Assign the variable 'N' as the vector of series lengths. Use only odd numbers ranging from 1 to 25 games.
N<-seq(1:25)

# Apply the 'prob_win' function across the vector of series lengths to determine the probability that team B will win. Call this object `Pr`.
Pr<-sapply(N,prob_win)

# Plot the number of games in the series 'N' on the x-axis and 'Pr' on the y-axis.
plot(N, Pr)



#Final assesment in discrete probability.


library(gtools)
library(tidyverse)

medals<-c("J1","J2","J3","O1","O2","O3","O4","O5")
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


##Continuous probability
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches



library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x))


# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

#general formula
dnorm(69, 64, 10)


#Continuous probability assessment
#The ACT is a standardized college admissions test used in the United States. The four multi-part questions in this assessment all involve simulating some ACT test scores and answering probability questions about them.
#For the three year period 2016-2018, ACT standardized test scores were approximately normally distributed with a mean of 20.9 and standard deviation of 5.7. (Real ACT scores are integers between 1 and 36, but we will ignore this detail and use continuous values instead.)

#For R 3.6 or later version
set.seed(16, sample.kind = "Rounding")

#First we'll simulate an ACT test score dataset and answer some questions about it.
#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.
act_scores<-rnorm(10000,20.9,5.7)

#q1a What is the mean of act_scores?
m_sc<-mean(act_scores)

#q1b What is the standard deviation of act_scores?
sd_sc<-sd(act_scores)

#q1c A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
index <- act_scores>=36
act_scores[index]
length(act_scores[index])

#q1d In act_scores, what is the probability of an ACT score greater than 30?
1-pnorm(30,mean(act_scores),sd(act_scores))

#q1e In act_scores, what is the probability of an ACT score less than or equal to 10?
pnorm(10,mean(act_scores),sd(act_scores))

#q2 Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x<-seq(1:36)
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)

#In this 3-part question, you will convert raw ACT scores to Z-scores and answer some questions about them.
#Convert act_scores to Z-scores. Recall from Data Visualization (the second course in this series) that to standardize values (convert values into Z-scores, that is, values distributed with a mean of 0 and standard deviation of 1), you must subtract the mean and then divide by the standard deviation. Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.

#act_scores<- (act_scores-m_sc)/sd_sc
#q3a What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
1-pnorm(2)

#q3b What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
(2*sd_sc)+m_sc

#A Z-score of 2 corresponds roughly to the 97.5th percentile.
#Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?
qnorm(0.975,m_sc,sd_sc)


#In this 4-part question, you will write a function to create a CDF for ACT scores.
#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF).
#Apply this function to the range 1 to 36.

F <- function(a) pnorm(a,m_sc,sd_sc)
v<-F(seq(1:36))
min(which(v>=0.95))
#or
ceiling(qnorm(0.95, mean(act_scores), sd(act_scores)))

#q4b Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
#What is the expected 95th percentile of ACT scores?
qnorm(0.95, 20.9,5.7)

#q4c As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
#In what percentile is a score of 26?
#Your answer should be an integer (i.e. 60), not a percent or fraction. Note that a score between the 98th and 99th percentile should be considered the 98th percentile, for example, and that quantile numbers are used as names for the vector sample_quantiles.
sample_quantiles<- seq(0.01, 0.99, 0.01)
quantile(act_scores,sample_quantiles)
max(which(quantile(act_scores,sample_quantiles)<26))

#q4d Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
#Which of the following graphs is correct?
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles<-qnorm(p,20.9,5.7)
plot(theoretical_quantiles, sample_quantiles)
#or
qqplot(y=sample_quantiles, x=theoretical_quantiles)




##Random variables, Sampling models, and Central limit theorem
# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#monte carlo simulations
# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money


library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#Distributions versus Probability Distributions

#A random variable X  has a probability distribution function F(a) that defines Pr(X<=a) over all values of a.
#Any list of numbers has a distribution. The probability distribution function of a random variable is defined mathematically and does not depend on a list of numbers.
#The results of a Monte Carlo simulation with a large enough number of observations will approximate the probability distribution of X.
#If a random variable is defined as draws from an urn:
  #The probability distribution function of the random variable is defined as the distribution of the list of values in the urn.
  #The expected value of the random variable is the average of values in the urn.
  #The standard error of one draw of the random variable is the standard deviation of the values of the urn.

#X (capital letters) states for a theoretical value of a random variable, while x (small letter) states for the actual observation of the random variable.
#In the notation Pr(X=x), we are asking how frequently the random variable X is equal to the value x. For example, if x=6 , this statement becomes Pr(X=6).

#Intro to Central limit theorem
#The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.

#The expected value of a random variable, E[X]=u , is the average of the values in the urn. This represents the expectation of one draw. 
#The standard error of one draw of a random variable is the standard deviation of the values in the urn.
#The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
#The standard error of the sum of independent draws of a random variable is the square root of the number of draws times the standard deviation of the urn. 


#Section 3: Central limit theorem
#Law of large numbers
#The law of large numbers states that as n increases, the standard error of the average of a random variable decreases. In other words, when n is large, the average of the draws converges to the average of the urn.
#The law of large numbers is also known as the law of averages.
#The law of averages only applies when n is very large and events are independent. It is often misused to make predictions about an event being "due" because it has happened less frequently than expected in a small sample size.

#The sample size required for the Central Limit Theorem and Law of Large Numbers to apply differs based on the probability of success.
#If the probability of success is high, then relatively few observations are needed.
#As the probability of success decreases, more observations are needed.
#If the probability of success is extremely low, such as winning a lottery, then the Central Limit Theorem may not apply even with extremely large sample sizes. The normal distribution is not a good approximation in these cases, and other distributions such as the Poisson distribution may be more appropriate.


#FINAL ASSESSMENT ##Random variables, Sampling models, and Central limit theorem
#The SAT is a standardized college admissions test used in the United States. The following two multi-part questions will ask you some questions about SAT testing.
#This is a 6-part question asking you to determine some probabilities of what happens when a student guessed for all of their answers on the SAT. Use the information below to inform your answers for the following questions.
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

#Q1AWhat is the probability of guessing correctly for one question?
p_correct<-1/5

#q1bWhat is the expected value of points for guessing on one question?
1*1/5+(-0.25)*4/5

#q1c What is the expected score of guessing on all 44 questions?
avg<-44*(1*1/5+(-0.25)*4/5)

#q1d What is the standard error of guessing on all 44 questions?
sd<-sqrt(44)*abs((-0.25)-1)*sqrt(p_correct*(1-p_correct))

#q1e Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8,avg,sd)

#q1f Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding") instead of set.seed(x)
set.seed(21, sample.kind = "Rounding")
# What is the probability that a guessing student scores 8 points or higher?
p_not_correct<-1-p_correct
S<-replicate(10000,{
  X<-sample(c(1,-0.25),44,replace=TRUE,prob=c(p_correct,p_not_correct))
  sum(X)
})
  
sum(S>=8)/10000

#The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.
#In this two-part question, you'll explore how that affected the expected values for the test.

#Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
#q2a What is the expected value of the score when guessing on this new test?
avg<-44*(1*1/4+(0)*3/4)
avg

#q2b Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.

# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])


#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
#The following 7-part question asks you to do some calculations related to this scenario.

#q3a What is the expected value of the payout for one bet?
(6*5/38 + -1*(1 - 5/38))

#q3b What is the standard error of the payout for one bet?
abs((-1)-6)*sqrt(5/38*(33/38))

#q3c What is the expected value of the average payout over 500 bets? Remember there
#  is a difference between expected value of the average and expected value of
#  the sum. Same as one bet.
(6*5/38 + -1*(33/38))


#q3d What is the standard error of the average payout over 500 bets? Remember there
# is a difference between the standard error of the average and standard error of
# the sum.
(abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))/sqrt(500)


#q3e What is the expected value of the sum of 500 bets?
avg<-(6*5/38 + -1*(33/38))*500


#q3f What is the standard error of the sum of 500 bets?
se<-sqrt(500)*abs(-1-6)*sqrt(5/38*33/38)

#q3g Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets, Pr(X<=0) .
pnorm(0,avg,se)



#Interest rates - The Big Short
#Code: Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Code: Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#Code: Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

#Code: Expected value and standard error of the sum of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

#Code: Calculating interest rates for expected value of 0
x = - loss_per_foreclosure*p/(1-p)
x

x/180000 #interest rate

#Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))\x
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#Code: Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Code: Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#Code: Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#Code: Monte Carlo simulation with unknown default probability

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million