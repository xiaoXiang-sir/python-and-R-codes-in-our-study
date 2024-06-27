windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
library(readxl)
library(ggplot2)
library(openxlsx)
file_path <- "C:/Users/12292/Desktop/scale_train_feature315.xlsx"
data <- read_excel(file_path)
data <- as.data.frame(data)
X <- data[,2:ncol(data)]
X.matrix <- as.matrix(X)
str(X)
Y <- as.matrix(data$status)
library(glmnet)
set.seed(42)
cv_fit <- cv.glmnet(x=X.matrix,y=Y,alpha=1,nfolds = 10,family="binomial",maxit = 10000000)

par(mar=c(5,5,3,2))
par(cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
par(family="Times")
plot(cv_fit,xlab = "Log Lambda")
abline(v = log(cv_fit$lambda.1se), lwd=2, lty=1, col="red")
abline(v = log(cv_fit$lambda.min), lwd=2, lty=1, col="black")
plot(cv_fit$glmnet.fit, xvar = "lambda", label = FALSE)
abline(v = log(cv_fit$lambda.1se), lwd=2, lty=1, col="red")
abline(v = log(cv_fit$lambda.min), lwd=2, lty=1, col="black")
#par(mar=c(5,5,2,1))
#par(mfrow=c(2,2))
#plot(cv_fit,cex=1.5,cex.axis=1.5,cex.lab=1.5)
#plot(cv_fit$glmnet.fit,xvar = "lambda",label = TRUE,cex=1.5,cex.axis=1.5,cex.lab=1.5)
plot(cv_fit$glmnet.fit,xvar = "lambda",label = FALSE,cex=2,cex.axis=2,cex.lab=1.6)
plot(cv_fit, cex=1.8, cex.axis=2, cex.lab=1.6,cex.main=15,label=FALSE)
plot(cv_fit)
abline(v = log(cv_fit$lambda.1se), lwd=2, lty=1, col="red")
abline(v = log(cv_fit$lambda.min), lwd=2, lty=1, col="black")

#plot(cv_fit$glmnet.fit,xvar = "dev",label = TRUE)
cv_fit
lambdas_min <- cv_fit$lambda.min
lambdas_min

best_lambda_min <- cv_fit$lambda.min
coefficients <- coef(cv_fit, s = best_lambda_min)
print(coefficients)

#In this study,we choose lambda.1se.
lambdas_lse <- cv_fit$lambda.1se
lambdas_lse
best_lambda_lse <- cv_fit$lambda.1se
coefficients <- coef(cv_fit, s = best_lambda_lse)
print(coefficients)

nonzero_indices <- which(coefficients[, 1] != 0)
nonzero_variables <- rownames(coefficients)[nonzero_indices]
nonzero_coefficients <- coefficients[nonzero_indices, 1]
nonzero_variables <- nonzero_variables[-1]
nonzero_coefficients <- nonzero_coefficients[-1]
data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)

#p <- ggplot(data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
#  geom_bar(stat = "identity", fill = "#B73508", width = 0.5) +
#  coord_flip() +
#  labs(x = "Variable", y = "Coefficient", title = "Nonzero Coefficients") +
#  theme_minimal() +
#  theme(axis.text.y = element_text(size = 15))
#print(p)



#library(showtext)

#windowsFonts(Times =windowsFont("Times New Roman"))
#par(family="Times")

#p <- ggplot(data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
#  geom_bar(stat = "identity", fill = "#B73508", width = 0.5) +
#  coord_flip() +
#  labs(x = NULL, y = NULL, title = "Nonzero Coefficients") +
#  theme_minimal() +
#  theme(axis.text.y = element_text(size =17, family = "Times",color = "black"),
#        axis.text.x = element_text(size =17, family = "Times",color = "black"),
#        axis.title = element_text(size = 17, family = "Times",color = "black"),
#        plot.title = element_text(size =17, family = "Times",color = "black"))

#print(p)

# Select non-zero variables and create a new dataset
selected_X <- X[, nonzero_variables]
selected_data <- cbind(status = Y, selected_X)
write.xlsx(selected_data, "selected_data315.xlsx")

# Install and load pROC package
library(pROC)

# Predict using the Lasso model with lambda.1se
predicted <- predict(cv_fit, newx = X.matrix, s = "lambda.1se")

# Calculate AUC using the true labels and predicted probabilities from the training set
auc_train <- roc(response = factor(Y, levels = c(0, 1)), predictor = as.numeric(predicted), levels = c(0, 1), direction = "<", plot = TRUE)

# Print the AUC of the training set
print(auc_train$auc)

# Calculate Radscore
coefPara <- coef(object = cv_fit, s = "lambda.1se")
lasso_values <- as.data.frame(which(coefPara != 0, arr.ind = TRUE))
lasso_names <- rownames(lasso_values)[-1]
lasso_coef <- data.frame(Feature = rownames(lasso_values), Coef = coefPara[which(coefPara != 0, arr.ind = TRUE)])

# Create lasso_coef data frame
train_set_lasso <- X[lasso_names]
train_all <- as.matrix(train_set_lasso)
xn <- nrow(train_set_lasso)
yn <- ncol(train_set_lasso)
beta <- as.matrix(coefPara[which(coefPara != 0), ])
betai_Matrix <- as.matrix(beta[-1])
beta0_Matrix <- matrix(beta[1], xn, 1)
Radcore_matrix <- train_all %*% betai_Matrix + beta0_Matrix
Radscore_train <- as.numeric(Radcore_matrix)
Radscore <- as.data.frame(Radscore_train)

# Load writexl package
library(writexl)
X_with_Radscore <- cbind(status = Y, Radscore)
# Save the data as an Excel file
write_xlsx(X_with_Radscore, "X_with_Radscore315.xlsx")

#Test Radscore
# Assume you already have beta, train_set_lasso, and lasso_names
excel_file_path <- "C:/Users/12292/Desktop/test_rad.xlsx"
validation_data <- read_excel(excel_file_path)

validation_set_lasso <- validation_data[lasso_names]
validation_all <- as.matrix(validation_set_lasso)
xn_val <- nrow(validation_set_lasso)
yn_val <- ncol(validation_set_lasso)
beta_val <- as.matrix(coefPara[which(coefPara != 0), ])
betai_Matrix_val <- as.matrix(beta_val[-1])
beta0_Matrix_val <- matrix(beta_val[1], xn_val, 1)
Radcore_matrix_val <- validation_all %*% betai_Matrix_val + beta0_Matrix_val
Radscore_validation <- as.numeric(Radcore_matrix_val)
Radscore_validation <- as.data.frame(Radscore_validation)

# Load writexl package
library(writexl)

validation_Radscore <- cbind(validation_data, Radscore = Radscore_validation)
# Save the data as an Excel file
write_xlsx(validation_Radscore, "validation_Radscore315.xlsx")
