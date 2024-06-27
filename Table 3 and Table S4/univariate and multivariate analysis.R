library(readxl)
library(car)
# read path
file_path <- "C:/Users/12292/Desktop/train yyhb.xlsx"
data <- read_excel(file_path)
data
library(autoReg)
#univariate for all
overall.log <- glm(status~.,data=data,family=binomial) 
model1<-autoReg(overall.log,uni=TRUE,multi=FALSE,threshold=0.05)
model1
#Wmax p=0.881(out)

# VIF for M
#VIF<3
model <- lm(status ~ Nmax+BNF+Hmax+HW+AR, data = data)
vif_values <- vif(model)
print(vif_values)
# Remove AR (AR vif=47.150609)
model <- lm(status~ Nmax+BNF+Hmax+HW, data = data)
vif_values <- vif(model)
print(vif_values)
# Remove Hmax (Hmax vif=8.070511)
model <- lm(status ~ Nmax+HW+BNF, data = data)
vif_values <- vif(model)
print(vif_values)
# Multivariate for M
overall.log <- glm(status~Nmax+HW+BNF,data=data,family=binomial) 
model2<-autoReg(overall.log,uni=FALSE,milti=TRUE,threshold=0.05)
model2

# VIF for R+P
model <- lm(status ~ phases+Radscore, data = data)
vif_values <- vif(model)
print(vif_values)
# Multivariate for R+P
overall.log <- glm(status~ phases+Radscore,data=data,family=binomial) 
model3<-autoReg(overall.log,uni=TRUE,milti=TRUE,threshold=0.05)
model3

# VIF for R+M
model <- lm(status ~ HW+BNF+Radscore, data = data)
vif_values <- vif(model)
print(vif_values)
# Multivariate for R+M
overall.log <- glm(status~ HW+BNF+Radscore,data=data,family=binomial) 
model4<-autoReg(overall.log,uni=TRUE,milti=TRUE,threshold=0.05)
model4

# VIF for R+M+P
model <- lm(status ~ HW+BNF+Radscore+phases, data = data)
vif_values <- vif(model)
print(vif_values)
# Multivariate for R+M+P
overall.log <- glm(status~ HW+BNF+Radscore+phases,data=data,family=binomial) 
model5<-autoReg(overall.log,uni=TRUE,milti=TRUE,threshold=0.05)
model5
