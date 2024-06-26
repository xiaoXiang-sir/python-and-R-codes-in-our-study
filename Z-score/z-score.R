# Load necessary libraries
library(readxl)
library(writexl)

# Read the training dataset
file_path <- "C:/Users/12292/Desktop/SR3/train radiomics.xlsx"
train_data <- read_excel(file_path)

# Extract the status column as the response variable (y)
y <- as.numeric(train_data$status)

# Extract data from the 3rd column to the last column as predictors (x)
x <- train_data[, 3:ncol(train_data)]

# Standardize the predictors (x)
scaled_x <- scale(x)

# Extract the ID column (the 1st column) from the original data frame
id_column <- train_data[, 1, drop = FALSE]

# Create a new data frame containing the ID column, y values, and standardized predictors
df_scaled <- cbind(id_column, status = y, as.data.frame(scaled_x))

# Retain original names for the first two columns and use x's column names for the rest
colnames(df_scaled)[-c(1, 2)] <- colnames(x)

# Print the standardized data frame
print(df_scaled)

# Write the new data frame to an Excel file without row names
write_xlsx(df_scaled, "train_scale.xlsx")

# Calculate the means and standard deviations of the training predictors (x)
means <- colMeans(x)
sds <- apply(x, 2, sd)

# Save these parameters for future use
save(means, sds, file = "scaling_parameters.rda")

# Load the previously saved scaling parameters
load("scaling_parameters.rda")

# Read the testing dataset
file_test_path <- "C:/Users/12292/Desktop/test size uni.xlsx"
test_data <- read_excel(file_test_path)

# Extract the predictors to be standardized, including the status column
test_x <- test_data[, 3:ncol(test_data)]
y_test <- as.numeric(test_data$status)

# Standardize the testing predictors using the training set's means and standard deviations
test_x_scaled <- scale(test_x, center = means, scale = sds)

# If you need to retain the testing dataset's ID column, merge them as before
test_id_column <- test_data[, 1, drop = FALSE]
test_df_scaled <- cbind(test_id_column, status = y_test, as.data.frame(test_x_scaled))

# Set column names for the standardized testing data frame
colnames(test_df_scaled)[3:ncol(test_df_scaled)] <- colnames(test_x_scaled)

# Print the standardized testing data frame
names(test_df_scaled)

# Optionally, write the standardized testing data to a new Excel file
write_xlsx(test_df_scaled, "test_scale.xlsx")
