# Install required package(s)
if(!require(RVAideMemoire)) {
  install.packages("RVAideMemoire")
  require(RVAideMemoire)
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

# Create dummy variable with only two categories for Cochran's Q test
long_data$binary_outcome <- "no"
long_data$binary_outcome[long_data$op == "yes"] <- "yes"
long_data$binary_outcome <- as.factor(long_data$binary_outcome)

# Run Cochran's Q test
cochran.qtest(binary_outcome ~ time | unique_id, data = long_data)