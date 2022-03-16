## 3.5.2 Data Exercises

# load data from peopleanalyticsdata package
install.packages("peopleanalyticsdata")
library(peopleanalyticsdata)
data(charity_donation)
cd <- charity_donation


# calculate the mean total_donations from the data set
(mean(cd$total_donations))

# Calculate the sample variance for total_donation and convert this to a population variance.
(sv_td <- var(cd$total_donations, na.rm = TRUE))
n <- length(na.omit(cd$total_donations))
(pv_td <- ((n-1)/n) * sv_td)

# Calculate the sample standard deviation for total_donations and verify that it is the same as the square root of the sample variance.
(sd_totald <- (sd(cd$total_donations, na.rm = TRUE)))
sd_totald == sqrt(sv_td)

# Calculate the sample correlation between total_donations and time_donating. By using an appropriate hypothesis test, determine if these two variables are independent in the overall population.
cor(cd$total_donations, cd$time_donating, use = "complete.obs")
cdNA <- cd[complete.cases(cd), ]
r <- cor(cdNA$total_donations, cdNA$time_donating)
n <- nrow(cdNA)
t_star <- (r*sqrt(n - 2))/sqrt(1-r^2)
2*pt(t_star, df = n-2, lower = FALSE)

cor.test(cdNA$total_donations, cdNA$time_donating)

# Calculate the mean and the standard error of the mean for the first 20 entries of total_donations.
(first20 <- head(cd$total_donations, 20))
mean(first20)
sd(first20)/sqrt(20)

# Calculate the mean and the standard error of the mean for the first 50 entries of total_donations. Verify that the standard error is less than in Exercise 5.
(first50 <- head(cd$total_donations, 50))
mean(first50)
sd(first50)/sqrt(50)

# By using an appropriate hypothesis test, determine if the mean age of those who made a recent donation is different from those who did not.
recent_d <- subset(cd, subset = recent_donation == 1, select = "age")
nonrecent_d <- subset(cd, subset = recent_donation == 0, select = "age")
t.test(recent_d, nonrecent_d)

# By using an appropriate hypothesis test, determine if there is a difference in whether or not a recent donation was made according to where people reside.
(contingency <- table(cd$recent_donation, cd$reside))
chisq.test(contingency)

# Extension: By using an appropriate hypothesis test, determine if the age of those who have recently donated is at least 10 years older than those who have not recently donated in the population.
t.test(recent_d, nonrecent_d, alternative = "greater", mu = 10)
## A: TRUE; mean(recent_d) = 62.69 > mean(nonrecent_d) = 42.19

# Extension: By using an appropriate hypothesis test, determine if the average donation amount is at least 10 dollars higher for those who recently donated versus those who did not. Retest for 20 dollars higher.
recent_td <- subset(cd, subset = recent_donation == 1, select = "total_donations")
nonrecent_td <- subset(cd, subset = recent_donation == 0, select = "total_donations")
t.test(recent_td, nonrecent_td, alternative = "greater", mu = 10)
## A: TRUE; mean(recent_td) = 2948.313 > mean(nonrecent_td) = 2166.937
t.test(recent_td, nonrecent_td, alternative = "greater", mu = 20)
