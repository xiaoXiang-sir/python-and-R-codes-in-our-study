library(readxl)
library(randomForest)
library(pROC)


file_path <- "C:\\Users\\12292\\Desktop\\scale_train_feature315 - fb.xlsx"
train_data <- read_excel(file_path)

file1_path <- "C:\\Users\\12292\\Desktop\\test_scale.xlsx"
test_data <- read_excel(file1_path)

# Ensure consistency in column names between test_data and train_data
names(test_data) <- names(train_data)

y <- train_data[[1]] 
y <- as.factor(y)   
x <- train_data[, -1]  

# Combine y and x into one data frame
data_rf <- cbind(y, x)

# Set random seed for reproducibility
set.seed(321)

# Initialize an empty vector to store errors
err <- numeric()

# Loop to test different mtry values
for (i in 1:(ncol(train_data) - 1)) {
  mtry_test <- randomForest(y ~ ., data = data_rf, mtry = i)
  err <- append(err, mean(mtry_test$err.rate))
}

# Print error rates
print(err)

# Find the mtry value corresponding to the minimum error rate
mtry <- which.min(err)

# Fit the random forest model with optimal mtry and 1000 trees
ntree_fit <- randomForest(y ~ ., data = data_rf, mtry = mtry, ntree = 1000)

# Plot the random forest model
plot(ntree_fit)
rf <- randomForest(y ~ ., data = data_rf, mtry = mtry, ntree = 840, importance = TRUE)
plot(rf)

print(rf)

varImpPlot(rf)
par(mar = c(5, 8, 4, 2))
varImpPlot(rf, cex = 1.2, main = "Variable Importance", pch = 19, col = "blue")
# Set larger plot dimensions
par(mar = c(5, 8, 4, 2), cex.main = 1, cex.lab = 0.8, cex.axis = 0.8)

# Plot variable importance
varImpPlot(rf, main = "Variable Importance", pch = 19, col = "black")

library(autoReg)
overall.log1 <- glm(status~HW+Nmax+BNF+AR+phases+original_shape_Elongation+original_shape_Flatness+original_firstorder_Skewness+original_shape_Sphericity+original_shape_MajorAxisLength ,data=train_data,family=binomial) 
model1<-autoReg(overall.log1,uni=TRUE,multi=TRUE,threshold=0.05)
model1


model2 <- lm(status ~ HW+Nmax+BNF+phases+original_shape_Elongation+original_shape_Sphericity+original_shape_MajorAxisLength, data = train_data)
library(car)
vif_values1 <- vif(model2)
vif_values1

model3 <- lm(status ~ HW+Nmax+BNF+phases+original_shape_Elongation+original_shape_Sphericity, data = train_data)
library(car)
vif_values2 <- vif(model3)
vif_values2
overall.log1 <- glm(status~HW+Nmax+BNF+phases+original_shape_Elongation+original_shape_Sphericity,data=train_data,family=binomial) 
model1<-autoReg(overall.log1,uni=TRUE,multi=TRUE,threshold=0.05)
model1



library(autoReg)
overall.log1 <- glm(status~HW+BNF+AR+phases+original_shape_Elongation+original_shape_Flatness+original_ngtdm_Coarseness+original_shape_Sphericity+Nmax+Hmax,data=train_data,family=binomial) 
model1<-autoReg(overall.log1,uni=TRUE,multi=TRUE,threshold=0.05)
model1
# Remove ineffective variables from univariate and multivariate analyses.
model2 <- lm(status ~ HW+BNF+phases+original_shape_Sphericity+Nmax+Hmax, data = train_data)
library(car)
vif_values1 <- vif(model2)
vif_values1
#Remove Hmax
model3 <- lm(status ~ HW+BNF+phases+original_shape_Sphericity+Nmax, data = train_data)
library(car)
vif_values2 <- vif(model3)
vif_values2

overall.log1 <- glm(status~HW+BNF+phases+original_shape_Sphericity+Nmax,data=train_data,family=binomial) 
model1<-autoReg(overall.log1,uni=TRUE,multi=TRUE,threshold=0.05)
model1

#Model A
library(pROC)

# Model 1: Full model
logit_model1 <- glm(status ~ HW + BNF + phases + original_shape_Sphericity, data = train_data, family = binomial)
predicted_logit_model1_train <- predict(logit_model1, newdata = train_data, type = "response")
roc_curve_train <- roc(train_data$status, predicted_logit_model1_train)
auc_score_train <- auc(roc_curve_train)
print(paste("Train AUC (Model 1):", auc_score_train))

predicted_logit_model1_test <- predict(logit_model1, newdata = test_data, type = "response")
roc_curve_test <- roc(test_data$status, predicted_logit_model1_test)
auc_score_test <- auc(roc_curve_test)
print(paste("Test AUC (Model 1):", auc_score_test))

# Model 2: Phases only
logisticphases <- glm(status ~ phases, data = train_data, family = binomial)
predicted_Phases_train <- predict(logisticphases, newdata = train_data, type = "response")
roc_curve_train1 <- roc(train_data$status, predicted_Phases_train)
auc_score_train1 <- auc(roc_curve_train1)
print(paste("Train AUC (Phases):", auc_score_train1))

predicted_Phases <- predict(logisticphases, newdata = test_data, type = "response")
roc_curve1 <- roc(test_data$status, predicted_Phases)
auc_score1 <- auc(roc_curve1)
print(paste("Test AUC (Phases):", auc_score1))

# Model 3: BNF, Radscore, and Phases
logistic_rad__mor_phases <- glm(status ~ BNF + Radscore + phases, data = train_data, family = binomial)
predicted_rad__mor_phases_train <- predict(logistic_rad__mor_phases, newdata = train_data, type = "response")
roc_curve_rad__mor_phases_train <- roc(train_data$status, predicted_rad__mor_phases_train)
auc_score_rad__mor_phases_train <- auc(roc_curve_rad__mor_phases_train)
print(paste("Train AUC (BNF+Radscore+Phases):", auc_score_rad__mor_phases_train))

predicted_rad__mor_phases_test <- predict(logistic_rad__mor_phases, newdata = test_data, type = "response")
roc_curve_rad__mor_phases_test <- roc(test_data$status, predicted_rad__mor_phases_test)
auc_score_rad__mor_phases_test <- auc(roc_curve_rad__mor_phases_test)
print(paste("Test AUC (BNF+Radscore+Phases):", auc_score_rad__mor_phases_test))

# ROC tests between models on train data
print(roc.test(roc_curve_train, roc_curve_train1))
print(roc.test(roc_curve_train, roc_curve_rad__mor_phases_train))

# ROC tests between models on test data
print(roc.test(roc_curve_test, roc_curve1))
print(roc.test(roc_curve_test, roc_curve_rad__mor_phases_test)



# Set the font to Times New Roman
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")


# Plot ROC curves on Training Data
plot(1 - roc_curve_train$specificities, roc_curve_train$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Training Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve_train1$specificities, roc_curve_train1$sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_train$specificities, roc_curve_rad__mor_phases_train$sensitivities, col = "#00FF00", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model A (AUC =", round(auc_score_train, 3), ")"),
                  paste("Model P (AUC =", round(auc_score_train1, 3), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_train, 3), ")")), 
       col = c("#0000FF", "#FF0000", "#00FF00"), lwd = 2, cex = 1.1)



plot(1 - roc_curve_test$specificities, roc_curve_test$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Testing Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve1$specificities, roc_curve1$sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_test$specificities, roc_curve_rad__mor_phases_test$sensitivities, col = "#00FF00", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model A (AUC =", round(auc_score_test, 3), ")"),
                  paste("Model P (AUC =", round(auc_score1, 3), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_test, 3), ")")), 
       col = c("#0000FF", "#FF0000", "#00FF00"), lwd = 2, cex = 1.1)
