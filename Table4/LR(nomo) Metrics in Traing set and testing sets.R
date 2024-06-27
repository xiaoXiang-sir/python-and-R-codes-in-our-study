# Load necessary libraries
library(readxl)
library(ResourceSelection)
library(pROC)

# Define file paths
train_file_path <- "C:/Users/12292/Desktop/train yyhb.xlsx"
test_file_path <- "C:/Users/12292/Desktop/test yyhb.xlsx"

# Load training data
train_data <- read_excel(train_file_path)

# Convert status to a factor
train_data$status <- as.factor(train_data$status)

# Train logistic regression model using selected features
logistic_rad_mor_phases1 <- glm(status ~ BNF + Radscore + phases, data = train_data, family = binomial)

# Predict probabilities on training data
predicted_rad_mor_phases1 <- predict(logistic_rad_mor_phases1, newdata = train_data, type = "response")

# Calculate ROC curve and AUC for training data
roc_curve_rad_mor_phases1 <- roc(train_data$status, predicted_rad_mor_phases1)
auc_score_rad_mor_phases1 <- auc(roc_curve_rad_mor_phases1)
print(paste("Training AUC:", auc_score_rad_mor_phases1))

# Calculate Hosmer-Lemeshow goodness of fit test for train data
hl_1_test <- hoslem.test(as.numeric(as.character(train_data$status)), predicted_rad_mor_phases1, g=49)
print(hl_1_test)
# p-value = 0.4132

# Calculate 95% CI for AUC
ci_auc_score_rad_mor_phases1 <- ci.auc(roc_curve_rad_mor_phases1)
print(paste("Training AUC 95% CI:", ci_auc_score_rad_mor_phases1[1], "-", ci_auc_score_rad_mor_phases1[3]))

# Convert predictions to binary classification
predicted_class <- ifelse(predicted_rad_mor_phases1 > 0.5, 1, 0)

# Calculate confusion matrix for training data
conf_matrix_train <- table(train_data$status, predicted_class)

# Calculate sensitivity, specificity, and accuracy for training data
sensitivity_train <- conf_matrix_train[2, 2] / (conf_matrix_train[2, 2] + conf_matrix_train[2, 1])
specificity_train <- conf_matrix_train[1, 1] / (conf_matrix_train[1, 1] + conf_matrix_train[1, 2])
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)

# Print training metrics
print(paste("Training Sensitivity:", sensitivity_train))
print(paste("Training Specificity:", specificity_train))
print(paste("Training Accuracy:", accuracy_train))

# Load test data
test_data <- read_excel(test_file_path)
test_data$status <- factor(test_data$status)

# Predict probabilities on test data
predicted_rad_mor_phases <- predict(logistic_rad_mor_phases1, newdata = test_data, type = "response")

# Calculate ROC curve and AUC for test data
roc_curve_rad_mor_phases <- roc(test_data$status, predicted_rad_mor_phases)
auc_score_rad_mor_phases <- auc(roc_curve_rad_mor_phases)
print(paste("Test AUC:", auc_score_rad_mor_phases))

# Calculate Hosmer-Lemeshow goodness of fit test for test data
hl_test <- hoslem.test(as.numeric(as.character(test_data$status)), predicted_rad_mor_phases, g=20)
print(hl_test)
# p-value = 0.5429

# Calculate 95% CI for AUC on test data
ci_auc_score_rad_mor_phases <- ci.auc(roc_curve_rad_mor_phases)
print(paste("Test AUC 95% CI:", ci_auc_score_rad_mor_phases[1], "-", ci_auc_score_rad_mor_phases[3]))

# Calculate Brier Score for test data
test_data$status <- as.numeric(as.character(test_data$status))
brier_score_rad_mor_phases <- mean((test_data$status - predicted_rad_mor_phases)^2)
print(paste("Brier Score:", brier_score_rad_mor_phases))

# Calculate confusion matrix for test data
conf_matrix_test <- table(test_data$status, ifelse(predicted_rad_mor_phases > 0.5, 1, 0))

# Calculate sensitivity, specificity, and accuracy for test data
sensitivity_test <- conf_matrix_test[2, 2] / sum(conf_matrix_test[2, ])
specificity_test <- conf_matrix_test[1, 1] / sum(conf_matrix_test[1, ])
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)

# Print test metrics
print(paste("Test Sensitivity:", sensitivity_test))
print(paste("Test Specificity:", specificity_test))
print(paste("Test Accuracy:", accuracy_test))
