# Install required package(s)
if(!require(RVAideMemoire)) {
  install.packages("RVAideMemoire")
  require(RVAideMemoire)
}
if(!require(psych)) {
  install.packages("psych")
  require(psych)
}

# Read data
wide_data <- read.csv("consensus_ratings.csv")

# Descriptive statistics
summary(wide_data$committee)
summary(wide_data$study_type)
summary(wide_data$op_report)
summary(wide_data$op_materials)
summary(wide_data$op_preregistration)
summary(wide_data$op_data)

# Create unique id for each application to avoid problems from duplicate dnrs
wide_data$unique_id <- paste(wide_data$committee, wide_data$dnr1, sep = " ") 

# Reshape data into long format for Cochran's Q test
concat_practices <- c("op_data", "op_report", "op_materials", "op_preregistration")
long_data <- reshape(wide_data, varying = concat_practices, idvar = "unique_id", direction = "long", sep = "_")

xtabs(~ op + time, data = long_data)

# Create binary integer variable for Cochran's Q test
long_data$binary_outcome <- "no"
long_data$binary_outcome[long_data$op == "yes"] <- "yes"
long_data$binary_outcome <- as.numeric(as.factor(long_data$binary_outcome)) - 1 

str(long_data)

xtabs(~ binary_outcome + time, data = long_data)
tab <- xtabs(~ binary_outcome + time, data = long_data)
tab2 <- tab[, 1:2]
tab <- xtabs(~ binary_outcome + time, data = long_data)

fisher.test(tab)
fisher.test(tab[, 1:2])
fisher.test(tab[, c(1, 3)])
fisher.test(tab[, c(1, 4)])
fisher.test(tab[, c(2, 3)])
fisher.test(tab[, c(2, 4)])
fisher.test(tab[, c(3, 4)])

# Run Cochran's Q test
test1 <- cochran.qtest(binary_outcome ~ time | unique_id, data = long_data)

# Experimental
test_data <- long_data
test_data$practice = factor(test_data$time, levels = unique(test_data$time))
test_data$response = factor(test_data$binary_outcome, levels = c("no", "yes"))
columns_to_keep = c("unique_id", "response", "practice")
test_data <- test_data[ , columns_to_keep, drop = FALSE]
test_data$response.n = as.numeric(test_data$response) - 1

cochran.qtest(response.n ~ practice | unique_id, data = test_data)

#### RESHAPE 2: This Time It's Personal
require(reshape2)

wide_test_data <- wide_data

# Numberficate values
wide_test_data$op_report <- as.numeric(wide_test_data$op_report)
wide_test_data$op_preregistration <- as.numeric(wide_test_data$op_preregistration)
wide_test_data$op_data <- as.numeric(wide_test_data$op_data)
wide_test_data$op_materials <- as.numeric(wide_test_data$op_materials)

# Melt it wide
# Throws warning, more info here: https://stackoverflow.com/questions/25688897/reshape2-melt-warning-message
melted_data <- melt(wide_test_data, id = "unique_id", measured = concat_practices)


# Force the Q-test

tab <- tapply(long_data$binary_outcome, list(long_data$unique_id, long_data$time), function(x) sum(x))
k <- ncol(tab)
b <- nrow(tab)
X.j <- colSums(tab)
Xi. <- rowSums(tab)
N <- sum(X.j)
Q <- k * (k - 1) * sum((X.j - N/k)^2)/sum(Xi. * (k - Xi.))
p <- pchisq(Q, k - 1, lower.tail = FALSE)

signs <- apply(tab[, c("data", "preregistration")], 1, diff)
binom.test(length(signs[signs > 0]), length(signs[signs != 0]), 0.5)$p.value



tab <- tapply(long_data$binary_outcome[long_data$time %in% c("data", "preregistration")], list(long_data$unique_id[long_data$time %in% c("data", "preregistration")], long_data$time[long_data$time %in% c("data", "preregistration")]), function(x) sum(x))
k <- ncol(tab)
b <- nrow(tab)
X.j <- colSums(tab)
Xi. <- rowSums(tab)
N <- sum(X.j)
Q <- k * (k - 1) * sum((X.j - N/k)^2)/sum(Xi. * (k - Xi.))
p <- pchisq(Q, k - 1, lower.tail = FALSE)

