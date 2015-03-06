setwd("D:/stat/Assignment 2");
install.packages("plotrix");
library(plotrix);
statAnalysis <- function(dataSet, name){
# Packages needed: plotrix (Description: for standard error computation)
#Scanning the the home run data for 2011
#Constructing a box plot
boxplot(dataSet, main = name);
#1. Compute the sample mean, variance, and standard deviation.
cat("Mean:",mean(dataSet));
cat("\nStandard Deviation:",sd(dataSet));
cat("\nVariance:",var(dataSet));
#2. Estimate the standard error of the sample mean
cat("\nStandard Error:",std.error(dataSet));
#3. Compute the 5-point summary and construct a box-plot
cat("\nFive point summary:\n");
print(summary(dataSet));
#4. Compute the inter-quartile range, and using the 1.5 rule identify any outliers
remove_outliers <- function(dataSet, na.rm = TRUE, ...) {
  qnt <- quantile(dataSet, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(dataSet, na.rm = na.rm)
  y <- dataSet
  y[dataSet < (qnt[1] - H)] <- NA
  y[dataSet > (qnt[2] + H)] <- NA
  y  }
cat("\nDataset with Outliers Removed:\n",remove_outliers(dataSet));
#5. Plot a histogram and draw any conclusions you can about the underlying distributions
hist(dataSet, main = name);
}
home_run_2011 <- scan(file = "", sep = ',');
home_run_2013 <- scan(file = "D:/stat/Assignment 2/HRs-AL_2013-alt.csv", sep = ',');
salary_players_2013 <- scan(file = "D:/stat/Assignment 2/Salaries-AL_2013-alt.csv", sep = ',');
cat("\nThe home run data for 2011\n");
statAnalysis(home_run_2011, "Home run 2011");
cat("\nThe home run data for 2013\n");
statAnalysis(home_run_2013, "Home run 2013");
cat("\nThe home run data for 2011\n");
statAnalysis(salary_players_2013, "Salaries of players in 2013");

