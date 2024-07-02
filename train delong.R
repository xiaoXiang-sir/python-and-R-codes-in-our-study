library(readxl)
library(ResourceSelection)
library(pROC)
file_path <- "C:/Users/12292/Desktop/train yyhb.xlsx"
#file_path <- "C:/Users/12292/Desktop/train_scale 1.xlsx"
train_data <- read_excel(file_path)
train_data[[1]] <- factor(train_data[[1]])
file1_path <- "C:/Users/12292/Desktop/test yyhb.xlsx"
test_data <- read_excel(file1_path)
test_data[[1]] <- factor(test_data[[1]])
str(test_data)
#LGBM AUC
roc_LGBM<- roc(train_data$status, train_data$T_LGBM)
roc_GBM<- roc(train_data$status, train_data$T_GB)
roc_XGB<- roc(train_data$status, train_data$T_XGB)
roc_RF<- roc(train_data$status, train_data$T_RF)
#Train P
logisticphases <- glm(status ~ phases, data = train_data, family = binomial)
predicted_Phases_train <- predict(logisticphases, newdata = train_data, type = "response")
roc_curve_train <- roc(train_data$status, predicted_Phases_train)
auc_score_train <- auc(roc_curve_train)
print(paste("Training AUC:", auc_score_train))

#Train R
logisticRad <- glm(status ~ Radscore, data = train_data, family = binomial)
predicted_Rad_train <- predict(logisticRad, newdata = train_data, type = "response")
roc_curve_train2 <- roc(train_data$status, predicted_Rad_train)
auc_score_train2 <- auc(roc_curve_train2)
print(paste("Training AUC:", auc_score_train2))

#Train M
logisticMor <- glm(status ~ HW + BNF, data = train_data, family = binomial)
predicted_Mor_train <- predict(logisticMor, newdata = train_data, type = "response")
roc_curve_train3 <- roc(train_data$status, predicted_Mor_train)
auc_score_train3 <- auc(roc_curve_train3)
print(paste("Training AUC:", auc_score_train3))

roc.test(roc_LGBM,auc_score_train3) #M
roc.test(roc_LGBM,auc_score_train2) #R
roc.test(roc_LGBM,auc_score_train) #P
roc.test(roc_GBM,auc_score_train3) #M
roc.test(roc_GBM,auc_score_train2) #R
roc.test(roc_GBM,auc_score_train) #P
roc.test(roc_XGB,auc_score_train3) #M
roc.test(roc_XGB,auc_score_train2) #R
roc.test(roc_XGB,auc_score_train) #P
roc.test(roc_RF,auc_score_train3) #M
roc.test(roc_RF,auc_score_train2) #R
roc.test(roc_RF,auc_score_train) #P


#LR in training
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


#delong test
roc.test(auc_score_train_rad_phases,auc_score_train3) #M R+P
roc.test(auc_score_train_rad_phases,auc_score_train2) #R R+P
roc.test(auc_score_train_rad_phases,auc_score_train) #P
roc.test(auc_score_train_Mor_rad,auc_score_train3) #M M+R
roc.test(auc_score_train_Mor_rad,auc_score_train2) #R
roc.test(auc_score_train_Mor_rad,auc_score_train) #P
roc.test(auc_score_train_Mor_rad,auc_score_train_rad_phases) #R+M R+P
roc.test(auc_score_train_rad__mor_phases,auc_score_train3) #M R M P
roc.test(auc_score_train_rad__mor_phases,auc_score_train2) #R
roc.test(auc_score_train_rad__mor_phases,auc_score_train) #P

roc.test(auc_score_train_rad__mor_phases,auc_score_train_Mor_rad) #R M P R M 
roc.test(auc_score_train_rad__mor_phases,auc_score_train_rad_phases) #R M P R P
roc.test(auc_score_train_rad__mor_phases,auc_score_train) #P

roc.test(auc_score_train2,auc_score_train3) #R M
roc.test(auc_score_train3,auc_score_train) #M P
roc.test(auc_score_train2,auc_score_train) #R P



roc.test(auc_score_train_rad__mor_phases,roc_LGBM) 
roc.test(auc_score_train_rad__mor_phases,roc_GBM) 
roc.test(auc_score_train_rad__mor_phases,roc_XGB) 
roc.test(auc_score_train_rad__mor_phases,roc_RF) 

roc.test(roc_LGBM,roc_GBM) 
roc.test(roc_LGBM,roc_XGB)
roc.test(roc_LGBM,roc_RF) 

roc.test(roc_GBM,roc_XGB) 
roc.test(roc_GBM,roc_RF) 

roc.test(roc_XGB,roc_RF) 