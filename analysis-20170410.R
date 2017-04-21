# Install required package(s)
if(!require(RVAideMemoire)) {
  install.packages("RVAideMemoire")
  require(RVAideMemoire)
}
if(!require(psych)) {
  install.packages("psych")
  require(psych)
}

# Define Cochran's Q test function
q_test <- function(outcome, id, variables) {
  tab <- tapply(outcome, list(id, variables), function(x) sum(x))
  k <- ncol(tab)
  b <- nrow(tab)
  X.j <- colSums(tab)
  Xi. <- rowSums(tab)
  N <- sum(X.j)
  
  Q <- k * (k - 1) * sum((X.j - N/k)^2)/sum(Xi. * (k - Xi.))
  Q
  p <- pchisq(Q, k - 1, lower.tail = FALSE)
  p
  output <- c(Q, p)
  
  return(output)
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

# Visualize trinary data in crosstab for each open practice
xtabs(~ op + time, data = long_data)

# Create binary integer variable for Cochran's Q test
long_data$binary_outcome <- "no"
long_data$binary_outcome[long_data$op == "yes"] <- "yes"
long_data$binary_outcome <- as.numeric(as.factor(long_data$binary_outcome)) - 1 

# Visualize binary data for each practice
xtabs(~ binary_outcome + time, data = long_data)

result <- q_test(long_data$binary_outcome, long_data$unique_id, long_data$time)
result[1]
result[2]

# Run follow up tests on the specified columns
# repeat with other columns if necessary
# tab <- tapply(long_data$binary_outcome[long_data$time %in% c("data", "preregistration")], list(long_data$unique_id[long_data$time %in% c("data", "preregistration")], long_data$time[long_data$time %in% c("data", "preregistration")]), function(x) sum(x))
# k <- ncol(tab)
# b <- nrow(tab)
# X.j <- colSums(tab)
# Xi. <- rowSums(tab)
# N <- sum(X.j)
# Q <- k * (k - 1) * sum((X.j - N/k)^2)/sum(Xi. * (k - Xi.))
# p <- pchisq(Q, k - 1, lower.tail = FALSE)

# p.adjust(vektor_med_pvärden, method = fdr)