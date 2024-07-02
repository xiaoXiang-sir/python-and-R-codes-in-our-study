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
roc_LGBM<- roc(test_data$status, test_data$LGBM)
roc_GBM<- roc(test_data$status, test_data$GB)
roc_XGB<- roc(test_data$status, test_data$XGB)
roc_RF<- roc(test_data$status, test_data$RF)
#Test P
logisticphases <- glm(status ~ phases, data = train_data, family = binomial)
predicted_Phases <- predict(logisticphases, newdata = test_data, type = "response")
roc_curve1 <- roc(test_data$status, predicted_Phases)
auc_score1 <- auc(roc_curve1)
print(paste("AUC:", auc_score1))

#Test R
logisticRad <- glm(status ~Radscore, data = train_data, family = binomial)
predicted_Rad <- predict(logisticRad , newdata = test_data, type = "response")
roc_curve2 <- roc(test_data$status, predicted_Rad )
auc_score2 <- auc(roc_curve2)
print(paste("AUC:", auc_score2))

#Test M
logisticMor <- glm(status ~HW+BNF, data = train_data, family = binomial)
predicted_Mor <- predict(logisticMor, newdata = test_data, type = "response")
roc_curve3 <- roc(test_data$status,predicted_Mor )
auc_score3 <- auc(roc_curve3)
print(paste("AUC:", auc_score3))

roc.test(roc_LGBM,auc_score3) #M
roc.test(roc_LGBM,auc_score2) #R
roc.test(roc_LGBM,auc_score1) #P
roc.test(roc_XGB,auc_score3) #M
roc.test(roc_XGB,auc_score2) #R
roc.test(roc_XGB,auc_score1) #P
roc.test(roc_GBM,auc_score3) #M
roc.test(roc_GBM,auc_score2) #R
roc.test(roc_GBM,auc_score1) #P
roc.test(roc_RF,auc_score3) #M
roc.test(roc_RF,auc_score2) #R
roc.test(roc_RF,auc_score1) #P
#Test R+M+P
logistic_rad__mor_phases <- glm(status ~BNF+Radscore+phases, data = train_data, family = binomial)
predicted_rad__mor_phases <- predict(logistic_rad__mor_phases  , newdata = test_data, type = "response")
roc_curve_rad__mor_phases <- roc(test_data$status,predicted_rad__mor_phases )
auc_score_rad__mor_phases <- auc(roc_curve_rad__mor_phases)

#Test R+M
logistic_Mor_rad <- glm(status ~BNF+HW+Radscore, data = train_data, family = binomial)
predicted_Mor_rad <- predict(logistic_Mor_rad , newdata = test_data, type = "response")
roc_curve_Mor_rad <- roc(test_data$status,predicted_Mor_rad  )
auc_score_Mor_rad <- auc(roc_curve_Mor_rad)

#Test R+P
logistic_rad_phases <- glm(status ~Radscore+phases, data = train_data, family = binomial)
predicted_rad_phases <- predict(logistic_rad_phases, newdata = test_data, type = "response")
roc_curve_rad_phases <- roc(test_data$status,predicted_rad_phases  )
auc_score_rad_phases <- auc(roc_curve_rad_phases)


#delong test
roc.test(auc_score_rad_phases,auc_score3) #M R+P
roc.test(auc_score_rad_phases,auc_score2) #R R+P
roc.test(auc_score_rad_phases,auc_score1) #P
roc.test(auc_score_Mor_rad,auc_score3) #M M+R
roc.test(auc_score_Mor_rad,auc_score2) #R
roc.test(auc_score_Mor_rad,auc_score1) #P
roc.test(auc_score_Mor_rad,auc_score_rad_phases) #R+M R+P
roc.test(auc_score_rad__mor_phases,auc_score3) #M R M P
roc.test(auc_score_rad__mor_phases,auc_score2) #R
roc.test(auc_score_rad__mor_phases,auc_score1) #P

roc.test(auc_score_rad__mor_phases,auc_score_Mor_rad) #R M P R M 
roc.test(auc_score_rad__mor_phases,auc_score_rad_phases) #R M P R P
roc.test(auc_score_rad__mor_phases,auc_score1) #P

roc.test(auc_score2,auc_score3) #R M
roc.test(auc_score3,auc_score1) #M P
roc.test(auc_score2,auc_score1) #R P


roc.test(auc_score_rad__mor_phases,roc_LGBM) 
roc.test(auc_score_rad__mor_phases,roc_GBM) 
roc.test(auc_score_rad__mor_phases,roc_XGB) 
roc.test(auc_score_rad__mor_phases,roc_RF) 

roc.test(roc_LGBM,roc_GBM) 
roc.test(roc_LGBM,roc_XGB)
roc.test(roc_LGBM,roc_RF) 

roc.test(roc_GBM,roc_XGB) 
roc.test(roc_GBM,roc_RF) 

roc.test(roc_XGB,roc_RF) 
