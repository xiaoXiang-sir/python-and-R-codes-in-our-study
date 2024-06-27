windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")
library(readxl)
library(ggplot2)
library(openxlsx)

# Load the Excel file
file_path <- "C:/Users/12292/Desktop/scale_train_feature315.xlsx"
data <- read_excel(file_path)
data <- as.data.frame(data)

# Extract features and convert to matrix
X <- data[,2:ncol(data)]
X.matrix <- as.matrix(X)
str(X)

# Extract response variable and convert to matrix
Y <- as.matrix(data$status)

# Load glmnet library and set seed
library(glmnet)
set.seed(42)

# Perform cross-validated Lasso regression
cv_fit <- cv.glmnet(x = X.matrix, y = Y, alpha = 1, nfolds = 10, family = "binomial", maxit = 10000000)

# Plot settings
par(mar = c(5, 5, 3, 2))
par(cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
par(family = "Times")

# Plot cross-validated errors
plot(cv_fit, xlab = "Log Lambda")
abline(v = log(cv_fit$lambda.1se), lwd = 2, lty = 1, col = "red")
abline(v = log(cv_fit$lambda.min), lwd = 2, lty = 1, col = "black")

# Plot Lasso path
plot(cv_fit$glmnet.fit, xvar = "lambda", label = FALSE)
abline(v = log(cv_fit$lambda.1se), lwd = 2, lty = 1, col = "red")
abline(v = log(cv_fit$lambda.min), lwd = 2, lty = 1, col = "black")

plot(cv_fit$glmnet.fit, xvar = "lambda", label = FALSE, cex = 2, cex.axis = 2, cex.lab = 1.6)
plot(cv_fit, cex = 1.8, cex.axis = 2, cex.lab = 1.6, cex.main = 15, label = FALSE)
plot(cv_fit)
abline(v = log(cv_fit$lambda.1se), lwd = 2, lty = 1, col = "red")
abline(v = log(cv_fit$lambda.min), lwd = 2, lty = 1, col = "black")

# Output cross-validation results
cv_fit
lambdas_min <- cv_fit$lambda.min
lambdas_min

best_lambda_min <- cv_fit$lambda.min
coefficients <- coef(cv_fit, s = best_lambda_min)
print(coefficients)

lambdas_lse <- cv_fit$lambda.1se
lambdas_lse
best_lambda_lse <- cv_fit$lambda.1se
coefficients <- coef(cv_fit, s = best_lambda_lse)
print(coefficients)

# Identify non-zero coefficients
nonzero_indices <- which(coefficients[, 1] != 0)
nonzero_variables <- rownames(coefficients)[nonzero_indices]
nonzero_coefficients <- coefficients[nonzero_indices, 1]
nonzero_variables <- nonzero_variables[-1]
nonzero_coefficients <- nonzero_coefficients[-1]
data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)

# Plot non-zero coefficients using ggplot2
library(showtext)

windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")

p <- ggplot(data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "#B73508", width = 0.5) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = "Nonzero Coefficients") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 17, family = "Times", color = "black"),
        axis.text.x = element_text(size = 17, family = "Times", color = "black"),
        axis.title = element_text(size = 17, family = "Times", color = "black"),
        plot.title = element_text(size = 17, family = "Times", color = "black"))

print(p)

selected_X <- X[, nonzero_variables]
selected_data <- cbind(status = Y,selected_X)
write.xlsx(selected_data, "selected_data315.xlsx")
