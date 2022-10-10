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
