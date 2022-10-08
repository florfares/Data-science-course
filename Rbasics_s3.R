#R basics
#Section 3: Indexing, Data Wrangling, Plots

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