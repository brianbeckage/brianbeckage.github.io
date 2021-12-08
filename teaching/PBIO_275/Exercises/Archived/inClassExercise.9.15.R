# 1. Lavine question 2.

#  Simulating Dice Rolls
# (a) simulate 6000 dice rolls. Count the number of 1’s, 2’s, . . . , 6’s.

rolls<-sample(6, 6000, replace=T)
for(i in 1:6) print (sum(rolls==i)) 
#### My solution
# [1] 991
# [1] 1002
# [1] 1008
# [1] 1038
# [1] 979
# [1] 982
# OR
simDiceTot<-apply(rmultinom(n=6000, size=1, prob=c(rep(1,6))),1,sum)
[1] 964 993 1049 993 1001 1000


# (b) You expect about 1000 of each number. How close was your result to what you expected?

# The result was fairly close. The largest deviation from 1000 was 1038.
(simDiceTot-(rep(1000,6)))/1000
[1] -0.036 -0.007 0.049 -0.007 0.001 0.000


# (c) About how often would you expect to get more than 1030 1’s? Run an R
# simulation to estimate the answer.

rollvec=rep(0,100000)
for(i in 1:100000){ 
  roll=sample(6,6000,replace=T) 
  if(sum(roll==1)>1030) rollvec[i]=1 
}
sum(rollvec)/100000
# 0.14518

simDice<-matrix(NA,nrow=6,ncol=1000)
for(i in 1:1000){
  simDice[,i]<-apply(rmultinom(n=6000, size=1, prob=c(rep(1,6))),1,sum)
} 
length(simDice[1,simDice[1,]>1030])/dim(simDice)[2] # 0.141 or 14% of time


#### Another solution:
# We can also calculate the probability using the binomial distribution. This is appropriate, 
# because the 1's coming up is a binary condition (either it lands on 1, or it does not).
# The parameters of the binomial distribution are N (the number of trials, known) and p (the probability, 
# in this case 1/6). We can use the pbinom function to find the probabilities that our observations will be above 1030:
pbinom(1030, 6000, 1/6, lower.tail=FALSE)
[1] 0.1454537
#There is a probaility of 0.145 that a given number will be rolled greater than 1030 times. 





# 2. Lavine question 44.

# Enter the following R commands:
# u <- matrix ( runif(250000), 1000, 250 )
# y <- apply ( u, 2, mean )
# These create a 1000x250 (a thousand rows and two hundred fifty columns) matrix of random draws, 
# called u and a 250-dimensional vector y which contains the means of each column of U.
# Now enter the command hist(u[,1]). This command takes the first column of u (a column vector 
# with 1000 entries) and makes a histogram. Print out this histogram and describe what it looks like. 
# What distribution is the runif command drawing from?
# Now enter the command hist(y). This command makes a histogram from the vector y. 
# Print out this histogram. Describe what it looks like and how it differs from the one above. 
# Based on the histogram, what distribution do you think y follows?
# You generated y and u with the same random draws, so how can they have different distributions? What’s going on here?

u <- matrix ( runif(250000), 1000, 2500 )
y <- apply ( u, 2, mean )

hist(u[,1])
hist(y)




