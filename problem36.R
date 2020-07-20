library(ggplot2)
library(tidyverse)
dataset <- read_excel("BloodCol.xls", col_names = TRUE)
### Read in the data
View(dataset)
dataset <- as.data.frame(lapply(dataset, as.numeric))
Conc = str(dataset) ### Check the data structure. This gives us information about the variables and obervations in the data.
### Check the number of rows and columns
summary(dataset)
before <- dataset[[1]]
after <- dataset[[2]]
##Comput Column means
catyield<-data.frame(before, after)
caty<-gather(catyield, key ="before", value = "after" )
caty
#computer point estimates
###compute colomn means
xbars = apply(catyield, MARGIN = 2, FUN = mean)
xbars
###compute standard deviations
sds = apply(catyield, MARGIN = 2, FUN = sd)
sds
###Function to compute pooled variance
pooled.var = function(n1, n2, s1, s2){
  ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1 + n2 - 2)
}
s1 = as.numeric(round(sds[1], 2))

s1

s2 = as.numeric(round(sds[2], 2))

s2
n1 = length(before)
n2 = length(after)
### Pooled standard deviation

psd <- sqrt(pooled.var(n1, n2, s1, s2))

psd
###test statistic
t = as.numeric((xbars[1] - xbars[2])/(psd*sqrt((1/n1) + (1/n2))))

t

### Compute the Pvalue

pvalue = 2*pt(abs(t), df = n1+n2-2, lower.tail = FALSE)

pvalue
### Use the t.test function 


###B)Calculate a one-sided confidence limit Here we assume that the sample mean is 5, the standard deviation is 2, and the sample size is 20. 
a <- 260.9 #mean
s <- sd(before) #standard dev
n <- 14 #sample size
a2 <- 235.4 #mean
s2 <- sd(after) #standard dev
n2 <- 14 #sample size
error <- qt(0.975,df=n-1)*s/sqrt(n)
error2 <- qt(0.975,df=n2-1)*s2/sqrt(n2)
left <- a-error
right <- a+error
left
right
left2 <- a2-error
right2 <- a2+error
left2
right2

