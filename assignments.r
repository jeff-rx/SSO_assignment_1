
# Appendix code

### 1.2 ####

# Given data
n <- 200
x <- 140
p_hat <- x/n
z_alpha_over_2 <- qnorm(1 - 0.005)  # Z-value for 99% confidence interval
p0 <- 0.75

# 1.2 a) Point estimate for p
point_estimate <- p_hat

# 1.2 b) 99% confidence interval for p
ci_lower <- p_hat - z_alpha_over_2 * sqrt(p_hat * (1 - p_hat) / n)
ci_upper <- p_hat + z_alpha_over_2 * sqrt(p_hat * (1 - p_hat) / n)

# 1.2 c) Hypothesis test for p
z_test_statistic <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)

# Print results
cat("a) Point estimate for p:", point_estimate, "\n")
cat("b) 99% confidence interval for p: (", ci_lower, ",", ci_upper, ")\n")
cat("c) Test statistic:", z_test_statistic, "\n")

# For alpha = 0.1
alpha <- 0.1
z_critical <- qnorm(1 - alpha/2)
if (abs(z_test_statistic) > z_critical) {
  cat("For alpha =", alpha, ": Reject H0\n")
} else {
  cat("For alpha =", alpha, ": Fail to reject H0\n")
}





### 1.4 ####


# Read in the .txt file
austen_data <- read.table("austen.txt", header = TRUE, sep = "\t")

# View the first few rows of the data
print(head(austen_data))


#1.4 a) A test for Homegeniety is better

#1.4 b) Investigating Consistency in Austen's Novels
# Perform chi-squared test for Austen's novels only
test_austen <- chisq.test(austen_data[, c("Sense", "Emma", "Sand1")])
cat("Test for Austen's novels:\n")
print(test_austen)

#1.4 c) Imitating Austen’s Style

# Perform chi-squared test comparing Austen's chapters to the admirer's chapters
test_imitation <- chisq.test(austen_data[, c("Sand1", "Sand2")])
cat("\nTest comparing Austen's chapters to the admirer's chapters:\n")
print(test_imitation)

