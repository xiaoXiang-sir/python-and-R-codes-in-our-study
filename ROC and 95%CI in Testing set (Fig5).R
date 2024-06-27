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
predicted_Phases <- predict(logisticphases, newdata = test_data, type = "response")
roc_curve1 <- roc(test_data$status, predicted_Phases)
auc_score1 <- auc(roc_curve1)
print(paste("AUC:", auc_score1))

#95%CI
ci_auc_score1 <- ci.auc(roc_curve1)
print(paste("AUC 95% CI:", ci_auc_score1[1], "-", ci_auc_score1[3]))

# Radscore for the testing set
logisticRad <- glm(status ~ Radscore, data = train_data, family = binomial)
predicted_Rad <- predict(logisticRad , newdata = test_data, type = "response")
roc_curve2 <- roc(test_data$status, predicted_Rad )
auc_score2 <- auc(roc_curve2)
print(paste("AUC:", auc_score2))

#95%CI
ci_auc_score2 <- ci.auc(roc_curve2)
print(paste("AUC 95% CI:", ci_auc_score2[1], "-", ci_auc_score2[3]))

# Morphology
logisticMor <- glm(status ~ HW + BNF, data = train_data, family = binomial)
predicted_Mor <- predict(logisticMor, newdata = test_data, type = "response")
roc_curve3 <- roc(test_data$status,predicted_Mor )
auc_score3 <- auc(roc_curve3)
print(paste("AUC:", auc_score3))

#95%CI
ci_auc_score3 <- ci.auc(roc_curve3)
print(paste("AUC 95% CI:", ci_auc_score3[1], "-", ci_auc_score3[3]))

#ROC Training R,M,P
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
plot(1 - roc_curve1$specificities, roc_curve1$sensitivities, 
     type = "n", 
     xlab = "1 - Specificity", ylab = "Sensitivity", lwd = 2,
     cex.lab = 1.4, cex.axis = 1.4)
smoothed_curve <- smooth.spline(1 - roc_curve1$specificities, roc_curve1$sensitivities)
lines(smoothed_curve, col = "#EEA236FF", lwd = 2)
abline(0,1,col = "black",lty = 2,lwd = 2)
lines(1 - roc_curve2$specificities, roc_curve2$sensitivities, 
      col = "#1A7Fc4", lwd = 2)
# Add ROC curve for Morphology Prediction
lines(1 - roc_curve3$specificities, roc_curve3$sensitivities, 
      col = "#C71000FF", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright",
       legend = c(paste("AUC:", format(round(auc_score3, 3), nsmall = 3), "Model M"),
                  paste("AUC:", format(round(auc_score2, 3), nsmall = 3), "Model R"),
                  paste("AUC:", format(round(auc_score1, 3), nsmall = 3), "Model P")),
       col = c("#C71000FF", "#1A7Fc4", "#EEA236FF"),
       lwd = 2,
       cex = 1.1)


# Combined models
#   R+P
logistic_rad_phases <- glm(status ~ Radscore + phases, data = train_data, family = binomial)
predicted_rad_phases <- predict(logistic_rad_phases, newdata = test_data, type = "response")
roc_curve_rad_phases <- roc(test_data$status,predicted_rad_phases  )
auc_score_rad_phases <- auc(roc_curve_rad_phases)
print(paste("AUC:", auc_score_rad_phases ))
#95%CI
ci_auc_rad_phases <- ci.auc(roc_curve_rad_phases)
print(paste("AUC 95% CI:", ci_auc_rad_phases[1], "-", ci_auc_rad_phases[3]))

#   R+M
logistic_Mor_rad <- glm(status ~ BNF + HW + Radscore, data = train_data, family = binomial)
predicted_Mor_rad <- predict(logistic_Mor_rad , newdata = test_data, type = "response")
roc_curve_Mor_rad <- roc(test_data$status,predicted_Mor_rad  )
auc_score_Mor_rad <- auc(roc_curve_Mor_rad)
print(paste("AUC:", auc_score_Mor_rad))
#95%CI
ci_auc_score_Mor_rad  <- ci.auc(roc_curve_Mor_rad )
print(paste("AUC 95% CI:", ci_auc_score_Mor_rad [1], "-", ci_auc_score_Mor_rad [3]))

# rad+Mor+phases
logistic_rad__mor_phases <- glm(status ~ BNF + Radscore + phases, data = train_data, family = binomial)
predicted_rad__mor_phases <- predict(logistic_rad__mor_phases  , newdata = test_data, type = "response")
roc_curve_rad__mor_phases <- roc(test_data$status,predicted_rad__mor_phases )
auc_score_rad__mor_phases <- auc(roc_curve_rad__mor_phases)
print(paste("AUC:", auc_score_rad__mor_phases))
#95%CI
ci_auc_score_rad__mor_phases  <- ci.auc(roc_curve_rad__mor_phases )
print(paste("AUC 95% CI:", ci_auc_score_rad__mor_phases [1], "-", ci_auc_score_rad__mor_phases [3]))

# Plot combined ROC curves
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
plot(1 - roc_curve_Mor_rad$specificities, roc_curve_Mor_rad$sensitivities, 
     type = "l", col = "#C71000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", lwd = 2,cex.lab = 1.4, cex.axis = 1.4)
abline(0,1,col = "black",lty = 2,lwd = 2)
lines(1 - roc_curve_rad_phases$specificities, roc_curve_rad_phases$sensitivities, 
      col = "#1A7Fc4", lwd = 2)
lines(1 - roc_curve_rad__mor_phases$specificities, roc_curve_rad__mor_phases$sensitivities, 
      col = "#EEA236FF", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright",
       legend = c(paste("AUC:", format(round(auc_score_Mor_rad, 3), nsmall = 3), "Model R+M"),
                  paste("AUC:", format(round(auc_score_rad_phases, 3), nsmall = 3), "Model R+P"),
                  paste("AUC:", format(round(auc_score_rad__mor_phases, 3), nsmall = 3), "Model R+M+P")),
       col = c("#C71000FF", "#1A7Fc4", "#EEA236FF"),
       lwd = 2,      
       cex = 1.1)


