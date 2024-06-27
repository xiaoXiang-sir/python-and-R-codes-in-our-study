library(readxl)
library(ResourceSelection)
library(pROC)
file_path <- "C:/Users/12292/Desktop/train yyhb.xlsx"
train_data <- read_excel(file_path)
train_data[[1]] <- factor(train_data[[1]])
file1_path <- "C:/Users/12292/Desktop/test yyhb.xlsx"
test_data <- read_excel(file1_path)
test_data[[1]] <- factor(test_data[[1]])

# Logistic regression model for phases
logisticphases <- glm(status ~ phases, data = train_data, family = binomial)
predicted_Phases_train <- predict(logisticphases, newdata = train_data, type = "response")
roc_curve_train <- roc(train_data$status, predicted_Phases_train)
auc_score_train <- auc(roc_curve_train)
print(paste("Training AUC:", auc_score_train))

# Radscore for the training set
logisticRad <- glm(status ~ Radscore, data = train_data, family = binomial)
predicted_Rad_train <- predict(logisticRad, newdata = train_data, type = "response")
roc_curve_train2 <- roc(train_data$status, predicted_Rad_train)
auc_score_train2 <- auc(roc_curve_train2)
print(paste("Training AUC:", auc_score_train2))

# Morphology
logisticMor <- glm(status ~ HW + BNF, data = train_data, family = binomial)
predicted_Mor_train <- predict(logisticMor, newdata = train_data, type = "response")
roc_curve_train3 <- roc(train_data$status, predicted_Mor_train)
auc_score_train3 <- auc(roc_curve_train3)
print(paste("Training AUC:", auc_score_train3))

# Plot ROC curves
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")
plot(1 - roc_curve_train$specificities, roc_curve_train$sensitivities, 
     type = "n", col = "#C71000FF",
     xlab = "1 - Specificity", ylab = "Sensitivity", lwd = 2,
     cex.lab = 1.4, cex.axis = 1.4)
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve_train$specificities, roc_curve_train$sensitivities, 
      col = "#EEA236FF", lwd = 2)
lines(1 - roc_curve_train2$specificities, roc_curve_train2$sensitivities, 
      col = "#1A7Fc4", lwd = 2)
lines(1 - roc_curve_train3$specificities, roc_curve_train3$sensitivities, 
      col = "#C71000FF", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright",
       legend = c(paste("AUC:", format(round(auc_score_train3, 3), nsmall = 3), "Model M"),
                  paste("AUC:", format(round(auc_score_train2, 3), nsmall = 3), "Model R"),
                  paste("AUC:", format(round(auc_score_train, 3), nsmall = 3), "Model P")),
       col = c("#C71000FF", "#1A7Fc4", "#EEA236FF"),
       lwd = 2,
       cex = 1.1)

# Combined models
logistic_rad_phases <- glm(status ~ Radscore + phases, data = train_data, family = binomial)
predicted_train_rad_phases <- predict(logistic_rad_phases, newdata = train_data, type = "response")
roc_curve_train_rad_phases <- roc(train_data$status, predicted_train_rad_phases)
auc_score_train_rad_phases <- auc(roc_curve_train_rad_phases)
print(paste("AUC on training data:", auc_score_train_rad_phases))

logistic_Mor_rad <- glm(status ~ BNF + HW + Radscore, data = train_data, family = binomial)
predicted_train_Mor_rad <- predict(logistic_Mor_rad, newdata = train_data, type = "response")
roc_curve_train_Mor_rad <- roc(train_data$status, predicted_train_Mor_rad)
auc_score_train_Mor_rad <- auc(roc_curve_train_Mor_rad)
print(paste("AUC on training data:", auc_score_train_Mor_rad))

logistic_rad__mor_phases <- glm(status ~ BNF + Radscore + phases, data = train_data, family = binomial)
predicted_train_rad__mor_phases <- predict(logistic_rad__mor_phases, newdata = train_data, type = "response")
roc_curve_train_rad__mor_phases <- roc(train_data$status, predicted_train_rad__mor_phases)
auc_score_train_rad__mor_phases <- auc(roc_curve_train_rad__mor_phases)
print(paste("AUC on training data:", auc_score_train_rad__mor_phases))

# Plot combined ROC curves
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")
plot(1 - roc_curve_train_Mor_rad$specificities, roc_curve_train_Mor_rad$sensitivities,
     type = "l", col = "#C71000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", lwd = 2, cex.lab = 1.4, cex.axis = 1.4)
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve_train_Mor_rad$specificities, roc_curve_train_Mor_rad$sensitivities, 
      col = "#C71000FF", lwd = 2)
lines(1 - roc_curve_train_rad_phases$specificities, roc_curve_train_rad_phases$sensitivities, 
      col = "#1A7Fc4", lwd = 2)
lines(1 - roc_curve_train_rad__mor_phases$specificities, roc_curve_train_rad__mor_phases$sensitivities, 
      col = "#EEA236FF", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright",
       legend = c(paste("AUC:", format(round(auc_score_train_Mor_rad, 3), nsmall = 3), "Model R+M"),
                  paste("AUC:", format(round(auc_score_train_rad_phases, 3), nsmall = 3), "Model R+P"),
                  paste("AUC:", format(round(auc_score_train_rad__mor_phases, 3), nsmall = 3), "Model R+M+P")),
       col = c("#C71000FF", "#1A7Fc4", "#EEA236FF"),
       lwd = 2,
       cex = 1.1)