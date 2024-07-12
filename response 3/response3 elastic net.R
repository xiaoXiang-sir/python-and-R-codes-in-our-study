library(readxl)
library(randomForest)
library(glmnet)
library(pROC)

file_path <- "C:\\Users\\12292\\Desktop\\scale_train_feature315 - fb.xlsx"
train_data <- read_excel(file_path)

file1_path <- "C:\\Users\\12292\\Desktop\\test_scale.xlsx"
test_data <- read_excel(file1_path)

names(test_data) <- names(train_data)

y_train <- as.factor(train_data[[1]])  
x_train <- as.matrix(train_data[, -1])  #
x_train <- as.matrix(train_data[, -c(1, ncol(train_data))])


x_test <- as.matrix(test_data[, -1]) 
y_test <- as.factor(test_data[[1]]) 
alpha_values <- seq(0, 1, by = 0.1)

set.seed(321)
best_alpha <- NULL
best_mse <- Inf

for (alpha in alpha_values) {
  cv_fit <- cv.glmnet(x_train, y_train, alpha = alpha, nfolds = 10, family = "binomial")
  mse <- mean(cv_fit$cvm)
  
  if (mse < best_mse) {
    best_mse <- mse
    best_alpha <- alpha
  }
}

best_alpha

print(paste("Best alpha:", best_alpha))

cv_fit <- cv.glmnet(x=x_train,y=y_train,alpha=0.6,nfolds = 10,family="binomial",maxit = 10000)

best_lambda_lse <- cv_fit$lambda.1se

par(mar=c(5,5,3,2))
par(cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
par(family="Times")  
plot(cv_fit, xlab = "Log Lambda")
plot(cv_fit)
plot(cv_fit$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = log(cv_fit$lambda.1se), lwd=1, lty=5, col="black")
abline(v = log(cv_fit$lambda.min), lwd=1, lty=5, col="black")

lambdas_lse <- cv_fit$lambda.1se
best_lambda_lse <- cv_fit$lambda.1se
coefficients <- coef(cv_fit, s = best_lambda_lse)
print(coefficients)

library(ggplot2)
nonzero_indices <- which(coefficients[, 1] != 0)
nonzero_variables <- rownames(coefficients)[nonzero_indices]
nonzero_coefficients <- coefficients[nonzero_indices, 1]
nonzero_variables <- nonzero_variables[-1]
nonzero_coefficients <- nonzero_coefficients[-1]
data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)

data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)
p <- ggplot(data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "black", width = 0.5) +
  coord_flip() +
  labs(x = " ", y = " ", title = "Nonzero Coefficients") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))
print(p)
#HW AR BNF PHASES Elongation Sphericity IDMN
library(readxl)
library(randomForest)
library(glmnet)
library(pROC)


file_path <- "C:\\Users\\12292\\Desktop\\elastic net.xlsx"
train_data <- read_excel(file_path)

file1_path <- "C:\\Users\\12292\\Desktop\\test_scale.xlsx"
test_data <- read_excel(file1_path)
train_y <- as.factor(train_data[[1]])  
train_x <- as.matrix(train_data[, -1])
test_y <- as.factor(test_data[[1]]) 

library(autoReg)
overall.log <- glm(status~HW+BNF+AR+phases+original_shape_Elongation+original_firstorder_Skewness +original_shape_Sphericity+original_glcm_Idmn,data=train_data,family=binomial) 
model<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model


train_cols <- colnames(train_x) 


test_x <- test_data[, train_cols]


head(test_x) 

logit_model <- glm(train_y ~ train_x, data = train_data, family = binomial)
summary(logit_model)

library(autoReg)
overall.log <- glm(status~HW+AR+BNF+phases+original_shape_Sphericity+original_glcm_Idmn+original_firstorder_Skewness+original_shape_Elongation,data=train_data,family=binomial) 
model<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model

library(autoReg)
overall.log <- glm(status~HW+BNF+phases+original_shape_Sphericity+original_glcm_Idmn,data=train_data,family=binomial) 
model<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model

model1 <- lm(status ~ HW + BNF + phases + original_shape_Sphericity + original_glcm_Idmn, data = train_data)
library(car)
vif_values <- vif(model1)
vif_values

file_path <- "C:\\Users\\12292\\Desktop\\scale_train_feature315 - fb.xlsx"
train_data <- read_excel(file_path)
y_train <- as.factor(train_data[[1]])

file1_path <- "C:\\Users\\12292\\Desktop\\test_scale.xlsx"
test_data <- read_excel(file1_path)
train_y <- as.factor(train_data[[1]]) 
train_x <- as.matrix(train_data[, -1])
test_y <- as.factor(test_data[[1]]) 

logit_model1 <- glm(status ~ HW + BNF + phases + original_shape_Sphericity + original_glcm_Idmn, data = train_data, family = binomial)
summary(logit_model1)
predicted_logit_model1_train <- predict(logit_model1, newdata = train_data, type = "response")
roc_curve_trainB <- roc(train_data$status,predicted_logit_model1_train)
auc_score_trainB <- auc(roc_curve_trainB)
print(paste("AUC:", auc_score_trainB ))

predicted_logit_model1_test <- predict(logit_model1, newdata = test_data, type = "response")
roc_curve_testB <- roc(test_data$status,predicted_logit_model1_test)
auc_score_testB <- auc(roc_curve_testB)
print(paste("AUC:", auc_score_testB ))

logit_model1 <- glm(status ~ HW + BNF + phases + original_shape_Sphericity + original_glcm_Idmn, data = train_data, family = binomial)
summary(logit_model1)

logisticphases <- glm(status ~ phases, data = train_data, family = binomial)
#AUC Phases train
predicted_Phases_train <- predict(logisticphases, newdata = train_data, type = "response")
roc_curve_train <- roc(train_data$status, predicted_Phases_train)
auc_score_train <- auc(roc_curve_train)
print(paste("AUC:", auc_score_train ))
#AUC Phases test
predicted_Phases <- predict(logisticphases, newdata = test_data, type = "response")
roc_curve1 <- roc(test_data$status, predicted_Phases)
auc_score1 <- auc(roc_curve1)
print(paste("AUC:", auc_score1))

roc.test(auc_score_testB,auc_score1)

logisticphasesM <- glm(status ~ HW + BNF+phases, data = train_data, family = binomial)
predicted_Phases_train1 <- predict(logisticphasesM, newdata = train_data, type = "response")
roc_curve_train1 <- roc(train_data$status, predicted_Phases_train1)
auc_score_train1 <- auc(roc_curve_train1)
print(paste("AUC:", auc_score_train1 ))
#AUC Phases test
predicted_PhasesM <- predict(logisticphasesM, newdata = test_data, type = "response")
roc_curve11 <- roc(test_data$status, predicted_PhasesM)
auc_score11 <- auc(roc_curve11)
print(paste("AUC:", auc_score11))

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
print(roc.test(roc_curve_train, roc_curve_train1))  #P,P+M
print(roc.test(roc_curve_train, roc_curve_rad__mor_phases_train)) #P,P+M
roc.test(roc_curve_trainB , roc_curve_rad__mor_phases_train) #B R+M+P 
roc.test(roc_curve_trainB ,roc_curve_train) 
roc.test(roc_curve_trainB,roc_curve_train1) #B M+P 

windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")


# Plot ROC curves on Training Data
plot(1 - roc_curve_trainB$specificities, roc_curve_trainB$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Training Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve_train$specificities, roc_curve_train$sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve_train1$specificities, roc_curve_train1$sensitivities, col = "#00FF00", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_train$specificities, roc_curve_rad__mor_phases_train$sensitivities, col = "#800080", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model B (AUC =", round(auc_score_trainB, 3), ")"),
                  paste("Model P (AUC =", round(auc_score_train, 3), ")"),
                  paste("Model C (AUC =", round(auc_score_train1, 3), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_train, 3), ")")), 
       col = c("#0000FF", "#FF0000", "#00FF00","#800080"), lwd = 2, cex = 1.1)

#Testing set
plot(1 - roc_curve_testB$specificities, roc_curve_testB$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Testing Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve1 $specificities, roc_curve1 $sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve11 $specificities, roc_curve11 $sensitivities, col = "#00FF00", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_test$specificities, roc_curve_rad__mor_phases_test$sensitivities, col = "#800080", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model B (AUC =", round(auc_score_testB, 3), ")"),
                  paste("Model P (AUC =", round(auc_score1 , 3), ")"),
                  paste("Model C (AUC =", round(auc_score11, 3), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_test, 3), ")")), 
       col = c("#0000FF", "#FF0000", "#00FF00","#800080"), lwd = 2, cex = 1.1)


roc.test(roc_curve_testB,roc_curve_rad__mor_phases_test) #B R+M+P 
roc.test(roc_curve_testB,roc_curve1)   #B P
roc.test(roc_curve_testB,roc_curve11 ) #B C



set.seed(321)
cv_fit <- cv.glmnet(x=x_train,y=y_train,alpha=1,nfolds = 10,family="binomial",maxit = 10000)

best_lambda_lse <- cv_fit$lambda.1se

par(mar=c(5,5,3,2))
par(cex.main = 1.4, cex.lab = 1.4, cex.axis = 1.4)
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
par(family="Times") 
plot(cv_fit, xlab = "Log Lambda")
plot(cv_fit)
plot(cv_fit$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = log(cv_fit$lambda.1se), lwd=1, lty=5, col="black")
abline(v = log(cv_fit$lambda.min), lwd=1, lty=5, col="black")
lambdas_lse <- cv_fit$lambda.1se
best_lambda_lse <- cv_fit$lambda.1se
coefficients <- coef(cv_fit, s = best_lambda_lse)
print(coefficients)
library(ggplot2)
nonzero_indices <- which(coefficients[, 1] != 0)
nonzero_variables <- rownames(coefficients)[nonzero_indices]
nonzero_coefficients <- coefficients[nonzero_indices, 1]
nonzero_variables <- nonzero_variables[-1]
nonzero_coefficients <- nonzero_coefficients[-1]
data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)

data <- data.frame(
  Variable = nonzero_variables,
  Coefficient = nonzero_coefficients)
p <- ggplot(data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "black", width = 0.5) +
  coord_flip() +
  labs(x = " ", y = " ", title = "Nonzero Coefficients") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12))
print(p)

library(autoReg)
overall.log <- glm(status~HW+AR+phases+original_shape_Sphericity+original_glcm_Idmn+original_shape_Elongation,data=train_data,family=binomial) 
model<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model

library(autoReg)
overall.log <- glm(status~HW+AR+phases+original_shape_Sphericity+original_glcm_Idmn+original_shape_Elongation,data=train_data,family=binomial) 
model<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model

logit_model11 <- glm(status ~ AR + phases + original_shape_Sphericity + original_glcm_Idmn, data = train_data, family = binomial)
summary(logit_model11)
predicted_logit_model11_train <- predict(logit_model11, newdata = train_data, type = "response")
roc_curve_trainD <- roc(train_data$status,predicted_logit_model11_train)
auc_score_trainD <- auc(roc_curve_trainD)
print(paste("AUC:", auc_score_trainD ))

predicted_logit_model11_test <- predict(logit_model11, newdata = test_data, type = "response")
roc_curve_testD <- roc(test_data$status,predicted_logit_model11_test)
auc_score_testD <- auc(roc_curve_testD)
print(paste("AUC:", auc_score_testD ))


logit_model111 <- glm(status ~ AR + phases , data = train_data, family = binomial)
summary(logit_model111)
predicted_logit_model111_train <- predict(logit_model111, newdata = train_data, type = "response")
roc_curve_trainE <- roc(train_data$status,predicted_logit_model111_train)
auc_score_trainE <- auc(roc_curve_trainE)
print(paste("AUC:", auc_score_trainE ))

predicted_logit_model111_test <- predict(logit_model111, newdata = test_data, type = "response")
roc_curve_testE <- roc(test_data$status,predicted_logit_model111_test)
auc_score_testE <- auc(roc_curve_testE)
print(paste("AUC:", auc_score_testE ))
# Plot ROC curves on Training Data
plot(1 - roc_curve_trainD$specificities, roc_curve_trainD$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Training Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve_train$specificities, roc_curve_train$sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_train$specificities, roc_curve_rad__mor_phases_train$sensitivities,col = "#800080", lwd = 2)
lines(1 - roc_curve_trainE$specificities, roc_curve_trainE$sensitivities, col = "#00FF00", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model D (AUC =", round(auc_score_trainD, 3), ")"),
                  paste("Model P (AUC =", round(auc_score_train, 3), ")"),
                  paste("Model E (AUC =", round(auc_score_trainE , 4), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_train, 3), ")")), 
       col = c("#0000FF", "#FF0000","#00FF00","#800080"),cex = 1.1)



#Testing set
plot(1 - roc_curve_testD$specificities, roc_curve_testD$sensitivities, 
     type = "l", col = "#0000FF", 
     xlab = "1 - Specificity", ylab = "Sensitivity", 
     lwd = 2, cex.lab = 1.4, cex.axis = 1.4, main = "ROC Curves on Testing Data")
abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(1 - roc_curve1 $specificities, roc_curve1 $sensitivities, col = "#FF0000", lwd = 2)
lines(1 - roc_curve_testE$specificities, roc_curve_testE$sensitivities, col = "#00FF00", lwd = 2)
lines(1 - roc_curve_rad__mor_phases_test$specificities, roc_curve_rad__mor_phases_test$sensitivities, col = "#800080", lwd = 2)
grid(col = "#CFCFCF", lwd = 2.5)
legend("bottomright", 
       legend = c(paste("Model D (AUC =", round(auc_score_testD, 3), ")"),
                  paste("Model P (AUC =", round(auc_score1, 3), ")"),
                  paste("Model E (AUC =", round(auc_score_testE , 4), ")"),
                  paste("Model R+M+P (AUC =", round(auc_score_rad__mor_phases_test, 3), ")")), 
       col = c("#0000FF", "#FF0000", "#00FF00","#800080"), lwd = 2, cex = 1.1)


roc.test(auc_score_testD ,roc_curve_testB ) #B C
roc.test(auc_score_testD  ,roc_curve1)
roc.test(auc_score_testD  ,auc_score_rad__mor_phases_test )
roc.test(roc_curve_testD,roc_curve_testE)

roc.test(roc_curve_trainD , roc_curve_rad__mor_phases_train) #B R+M+P 
roc.test(roc_curve_trainD ,roc_curve_train) 
roc.test(roc_curve_trainD , roc_curve_trainB) #B D
roc.test(roc_curve_trainD , roc_curve_trainE) #B 
