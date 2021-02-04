# Homework1
library(stats)
library(gendata)

#----Question 1----

pvs1 <- numeric(1000) 

set.seed(123)
for (i in 1:1000){
  y1 <- rnorm(n = 100, mean = 100, sd = 15) 
  y2 <- rnorm(n = 100, mean = 100, sd = 15)
  group <- rep(c("Group 1", "Group 2"), each = 100) 
  combined <- c(y1, y2)
  data <- data.frame(group, combined)
  result1 <- t.test(combined ~ group, data = data)
  pvs1[i] <- result1$p.value
}

sum1 <- sum(pvs1 <= .05) 

sum1/1000

#we would expect to see significant findings 4.1% of the time

#----Question 2A----

pvs2 <- numeric(1000) 
corr <- numeric(1000)

set.seed(2468)

for (i in 1:1000){
  d1 <- genmvnorm(cor = .3, k = 2, n = 50) 
  result2 <- cor.test(d1$X1, d1$X2)
  pvs2[i] <- result2$p.value
  corr[i] <- result2$estimate
}

sum2 <- sum(pvs2 <= .05) 

sum2/1000

# 57% of the time the correlation between these two variables is significant 

#----Question 2B----


#histogram of the correlation coefficient values

hist(corr, breaks = 20)

#----Question 2C----


mean(corr)
#mean of the 1,000 correlation coefficients is .30

#----Question 2D----


pvs2d <- numeric(1000) 
corr2d <- numeric(1000) 

set.seed(2468)

for (i in 1:1000){
  d2d <- genmvnorm(cor = .15, k = 2, n = 100) 
  result2d <- cor.test(d2d$X1, d2d$X2)
  pvs2d[i] <- result2d$p.value
  corr2d[i] <- result2d$estimate
}

sum2d<- sum(pvs2d <= .05) 

sum2d/1000

#If we had a correlation of .15 and n = 100, we would have power of .297


#----Question 2E----


library(pwr)
set.seed(123)
pwr.r.test(n = NULL, r = .15, sig.level = .05, power = .80)

#We need ~346 participants to have power of .80 with r = .15
