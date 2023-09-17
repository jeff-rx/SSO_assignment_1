



# Read in the .txt file
austen_data <- read.table("austen.txt", header = TRUE, sep = "\t")

# View the first few rows of the data
print(head(austen_data))



# b) Investigating Consistency in Austen's Novels
# Perform chi-squared test for Austen's novels only
test_austen <- chisq.test(austen_data[, c("Sense", "Emma", "Sand1")])
cat("Test for Austen's novels:\n")
print(test_austen)

# c) Imitating Austen’s Style
# Perform chi-squared test comparing Austen's chapters to the admirer's chapters
test_imitation <- chisq.test(austen_data[, c("Sand1", "Sand2")])
cat("\nTest comparing Austen's chapters to the admirer's chapters:\n")
print(test_imitation)

