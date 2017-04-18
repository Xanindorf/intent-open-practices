# Install required package(s)
if(!require(RVAideMemoire)) {
  install.packages("RVAideMemoire")
  require(RVAideMemoire)
}
require(psych)

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
# long_data$binary_outcome <- as.factor(long_data$binary_outcome) # doesn't seem to make a difference

# Run Cochran's Q test
cochran.qtest(binary_outcome ~ time | unique_id, alpha = 0.05, p.method = "fdr", data = long_data)

# Experimental
test_data <- long_data
test_data$practice = factor(test_data$time, levels = unique(test_data$time))
test_data$response = factor(test_data$binary_outcome, levels = c("no", "yes"))
columns_to_keep = c("unique_id", "response", "practice")
test_data <- test_data[ , columns_to_keep, drop = FALSE]
test_data$response.n = as.numeric(test_data$response) - 1

table <- xtabs(response.n ~ unique_id + practice, data = test_data)
table
xtabs( ~ practice + response, data = test_data)

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
