# Subhash Karthik
# Prediction using Supervised ML
# Predict the percentage of an student based on the no. of study hours.
file <- "http://bit.ly/w-data"
data <- read.csv(file, header=TRUE)
data
# or we can get data manually.
# We have two variables Hours and Scores
Hours <- c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8)
Scores <- c(21,47,27,75,30,20,88,60,81,25,85,62,41,42,17,95,30,24,67,69,30,54,35,76,86)
# Plotting the graph
plot(Hours, Scores, pch = 16, cex = 1.3, col = "blue", main = "Hours vs Percentage", xlab = "Hours", ylab = "Percentage")
# We can see that the graph is positively correlated
lm(Hours~Scores)
# Getting the true regression line
plot(Hours~Scores)+abline(lm(Hours ~ Scores))
# Finding the minimum distance between points and the regression line.
regmodel <- predict(lm(Hours ~ Scores))
regmodel
npoints = length(Hours) # To find the size of the data
npoints
plot(Hours~Scores)+abline(lm(Hours ~ Scores))+for (k in 1: npoints)  lines(c(Scores[k], Scores[k]), c(Hours[k], regmodel[k]))
# Predicting the scores from the hours.
relation <- lm(Scores~Hours)
print(relation)
print(summary(relation))
a <- data.frame(Hours=9.25)
result <-  predict(relation,a)
print(result)
# Hence we get the score for the no of hours studied.