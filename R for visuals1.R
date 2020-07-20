library(ggplot2)
library(readxl)
dataset <- read_excel("Table6_2REAL.xls", col_names = TRUE)
### Read in the data
View(dataset)
dataset <- as.data.frame(lapply(dataset, as.numeric))
Conc = str(dataset) ### Check the data structure. This gives us information about the variables and obervations in the data.
### Check the number of rows and columns
summary(dataset)
#mean1 <- mean(dataset[1])
#mean2 <- mean(dataset[2])
lapply(dataset[1], as.numeric)
typeof(dataset[1])
print(dataset[[1]])
temp = dataset[[1]]
usage = dataset[[2]]
Data <- data.frame(temp, usage)
graphics.off()
par("mar")
par(mar = c(1,1,1,1))
plot(temp,usage, pch = 16, ylab = "usage", xlab="temp")
abline(coef(lm(temp ~ usage, data = Data)))
#scatterplot
ggplot(Data, mapping = aes(x = temp, y = usage)) + geom_point()+geom_smooth(method = "lm", se = FALSE)
##Produce the regression model
slrl <-lm(temp ~ usage, data = Data)
summary(slrl)
names(slrl)
slrl$coefficients
#Correlation Coefficient
cor(temp, usage)
### 

