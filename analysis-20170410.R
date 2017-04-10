# Install required package(s)
if(!require(RVAideMemoire)){install.packages("RVAideMemoire")}
require(RVAideMemoire)

# Read data
data <- read.csv("consensus_ratings.csv")

# Descriptive statistics
summary(data$committee)
summary(data$study_type)
summary(data$op_report)
summary(data$op_materials)
summary(data$op_preregistration)
summary(data$op_data)

# Create unique id to avoid problems from duplicate dnrs
data$unique_id <- paste(data$committee, data$dnr1, sep = " ") 

concatenated_practices <- c("op_data", "op_report", "op_materials", "op_preregistration")
reshaped_data <- reshape(data, varying = concatenated_practices, times = concatenated_practices, idvar = "unique_id", direction = "long", sep = "_")

xtabs(~ op + time, data = reshaped_data)

# Create dummy variable with only two categories for Cochran's Q test
reshaped_data$binary_outcome <- "no"
reshaped_data$binary_outcome[reshaped_data$op == "yes"] <- "yes"
reshaped_data$binary_outcome <- as.factor(reshaped_data$binary_outcome)

# Run Cochran's Q test
cochran.qtest(binary_outcome ~ time | unique_id, data = reshaped_data)
