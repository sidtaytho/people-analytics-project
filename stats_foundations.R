## data download
url <- "http://peopleanalytics-regression-book.org/data/salespeople.csv"
salespeople <- read.csv(url)

## check it out
head(salespeople)
str(salespeople)

## descriptives
mean(salespeople$sales, na.rm = TRUE)
mean(salespeople$promoted, na.rm = TRUE)
mean(salespeople$performance, na.rm = TRUE)

# sample variance
(sample_variance_sales <- var(salespeople$sales, na.rm = TRUE))

# population variance (need length of non-NA data)
n <- length(na.omit(salespeople$sales))
(population_variance_sales <- ((n-1)/n) * sample_variance_sales)

# sample sd
(sample_sd_sales <- sd(salespeople$sales, na.rm = TRUE))

# sample sd should equal sqrt(sample var) - lets check
sample_sd_sales == sqrt(sample_variance_sales)

# calculate population sd
(population_sd_sales <- sqrt((n-1)/n) * sample_sd_sales)

## covariance & correlation
# get sample covar for sales and customer_rate
# ignoring observations w/ missing data
(sample_cov <- cov(salespeople$sales, salespeople$customer_rate,
                   use = "complete.obs"))

# convert to population covar (need number of complete obs)
cols <- subset(salespeople, select = c("sales","customer_rate"))
n <- nrow(cols[complete.cases(cols), ])
(population_cov <- ((n-1)/n) * sample_cov)

# Pearson's correlation - sample
cor(salespeople$sales,salespeople$customer_rate, use = "complete.obs")

# Pearson's correlation - sample (point-biserial)
cor(salespeople$sales, salespeople$promoted, use = "complete.obs")

# Spearman's rho correlation (ranked variables)
cor(salespeople$sales, salespeople$performance, 
    method = "spearman", use = "complete.obs")

# Kendall's tau correlation (ranked variables)
cor(salespeople$sales, salespeople$performance,
    method = "kendall", use = "complete.obs")

## standard errors, t-dist., confidence intv.
# set seed for reproducability
set.seed(123)

# generate a sample of 100 observations
custrate <- na.omit(salespeople$customer_rate)
n <- 100
sample_custrate <- sample(custrate, n)

# mean
(sample_mean <- mean(sample_custrate))

# standard error (se)
(se <- sd(sample_custrate)/sqrt(n))

# se multiple for 0.975
(t <- qt(p = 0.975, df = n-1))

# 95% CI - lower & upper bounds
lower_bound <- sample_mean - t*se
upper_bound <- sample_mean + t*se
cat(paste0('[', lower_bound, ', ', upper_bound, ']'))

## hypothesis testing

# testing for a difference in means (Welch's t-test)
# take two performance group samples 
perf1 <- subset(salespeople, subset = performance == 1)
perf4 <- subset(salespeople, subset = performance == 4)

# difference in mean sales
(diff <- mean(perf4$sales) - mean(perf1$sales))

# calculate se of the two sets
se <- sqrt(sd(perf1$sales)^2/length(perf1$sales)
           + sd(perf4$sales)^2/length(perf4$sales))

# calculate the required t-statistic
t <- qt(p = 0.975, df = 100.98)

# calculate 95% CI
(lower_bound <- diff - t*se)
(upper_bound <- diff + t*se)

# test if zero is inside the CI
(0 <= upper_bound) & (0 >= lower_bound)

# get t-stat
t_actual <- diff/se

# convert t-stat to p-value
2*pt(t_actual, df = 100.98, lower = FALSE)
t.test(perf4$sales, perf1$sales)

## testing for a non-zero correlation between 2 vars

# remove NAs
salespeople <- salespeople[complete.cases(salespeople), ]

# complete t_star
r <- cor(salespeople$sales, salespeople$customer_rate)
n <- nrow(salespeople)
t_star <- (r*sqrt(n - 2))/sqrt(1 - r^2)

# convert to p-value on t-dist. w/ n-2 df
2*pt(t_star, df = n - 2, lower = FALSE)
cor.test(salespeople$sales, salespeople$customer_rate)

## testing for dif. in freqeuncy dist. between categories (chi-square)

# create contingency table of promoted vs performance
(contingency <- table(salespeople$promoted, salespeople$performance))

# calculate expected promoted and not promoted
(expected_promoted <- (sum(contingency[2, ])/sum(contingency)) *
    colSums(contingency))

(expected_notpromoted <- (sum(contingency[1, ])/sum(contingency)) *
    colSums(contingency))

# calculate the difference metrics for promoted and not promoted
promoted <- sum((expected_promoted - contingency[2, ])^2/expected_promoted)
notpromoted <- sum((expected_notpromoted - contingency[2, ])^2/expected_notpromoted)

# calculate chi-squared stat
(chi_sq_stat <- notpromoted + promoted)

# calculate p-value from chi-squared stat
pchisq(chi_sq_stat, df = 3, lower.tail = FALSE)
chisq.test(contingency)

