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
