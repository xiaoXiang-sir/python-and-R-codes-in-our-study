library(readxl)


file_path <- "C:/Users/12292/Desktop/train size - 副本 (2).xlsx"
train_data <- read_excel(file_path)

file1_path <- "C:/Users/12292/Desktop/test size.xlsx"
test_data <- read_excel(file1_path)

library(pROC)

library(rms)
ddist <- datadist(train_data)  
options(datadist='ddist')
logistic_model_rms <- lrm(status ~ BNF+Radscore+PHASES, data = train_data, x = TRUE, y = TRUE)
#logistic_model_rms <- lrm(status ~ BNF+Radscore+phases, data = train_data, x = TRUE, y = TRUE)

windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
# nomogram
nomogram <- nomogram(logistic_model_rms, fun = function(x)1/(1+exp(-x)),funlabel="Risk of Rupture",conf.int=F,lp=F,fun.at=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
par(family="Times")
plot(nomogram, cex.axis=0.7, cex.main=1.4, cex.sub=1.1,cex=1.2)
rect(0.175,0.225,0.515,0.245,col = "#46B8DAFF") # color
rect(0.515,0.225,0.605,0.245,col = "#FFFF00FF") # color
rect(0.605,0.225,0.96,0.245,col = "#C71000FF") # color
text(0.33, 0.273, "Low", cex = 1.1)
text(0.57, 0.273, "Medium", cex = 1.1)
text(0.80, 0.273, "High", cex = 1.1)


#NOMOSCORE
# 1. 获取 nomogram 中每个变量的系数和分数
coefficients <- coef(logistic_model_rms)  # 获取模型中的系数
# 2. 获取训练数据中每个变量的值
BNF_value <- train_data$BNF
Radscore_value <- train_data$Radscore
phases_value <- train_data$phases
coefficients["BNF"]
coefficients["Radscore"]

BNF1_value <- test_data$BNF
Radscore1_value <- test_data$Radscore
phases1_value <- test_data$phases

linear_predictor <- 2.0992 * BNF_value + 1.1467* Radscore_value + 0.4293* phases_value-3.9436
linear_test_predictor <- 2.0992 * BNF1_value + 1.1467* Radscore1_value + 0.4293* phases1_value-3.9436
# 4. 将结果通过链接函数转换为概率
train_data$nomogram<- linear_predictor 
test_data$nomogram<- linear_test_predictor
library(openxlsx)
# 指定要保存的文件路径和文件名
excel_file <- "C:/Users/12292/Documents/nomogram.xlsx"
# 将 train_data 数据框保存为 Excel 文件
write.xlsx(train_data, file = excel_file, rowNames = FALSE)
excel_test_file <- "C:/Users/12292/Documents/nomogram test.xlsx"
write.xlsx(test_data, file = excel_test_file,rowNames = FALSE)


#CUVRE
library(calibrate)
library(ggplot2)
library(pROC)
library(rms)

# Assuming 'test_data' is your training dataset

windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
#train caliberation curve
library(rms)
set.seed(123)
bc_train <- train_data
bc_test <- test_data
fit1<-lrm(status ~ BNF+PHASES+Radscore,
          x = TRUE, y = TRUE,
          data = bc_train )
cal1 <- calibrate(fit1, method = "boot", B=1000)

par(mgp=c(2.6,0.85,0),mar=c(6,6,3,3)) 
par(family="Times")
plot(cal1,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Nomogram Predicted Probability",
     ylab = "Actual Probability",
     legend = FALSE,
     subtitles = FALSE,
     cex.axis = 1.4,
     cex.lab=2, cex.sub=0.9)  
abline(0,1,col = "black",lty = 2,lwd = 2) 
lines(cal1[,c("predy","calibrated.orig")], 
      type = "l",lwd = 4.5,col="#f72f2f",pch =16)
lines(cal1[,c("predy","calibrated.corrected")],
      type = "l",lwd = 3,col="#55cbff",pch =16)
##46B8DAFF
legend(0.45,0.25,
       c("Apparent","Ideal","Bias-corrected"), 
       lty = c(2,1,1), 
       lwd = c(3,3,3), 
       col = c("black","#f72f2f","#55cbff"),
       bty = "n",
       cex = 1.1)
#test caliberation curve
phat <- predict(fit1, newdata = bc_test)
bc_test$phat <- phat
fit2 <- lrm(status ~ phat, data = bc_test,x=T,y=T)
cal2 <- calibrate(fit2, method = "boot", B = 1000)

par(mgp=c(2.6,0.85,0),mar=c(6,6,3,3)) 
plot(cal2,
     xlim = c(0,1),
     ylim = c(0,1),
     xlab = "Nomogram Predicted Probability",
     ylab = "Actual Probability",
     legend = FALSE,
     subtitles = FALSE,
     cex.axis = 1.4,
     cex.lab=2, cex.sub=0.9)  # 设置刻度标签的大小
abline(0,1,col = "black",lty = 2,lwd = 2) 
lines(cal2[,c("predy","calibrated.orig")], 
      type = "l",lwd = 4.5,col="#FF0000",pch =16)
lines(cal2[,c("predy","calibrated.corrected")],
      type = "l",lwd = 3,col="#55cbff",pch =16)
legend(0.45,0.25,  ##legend是绘制图例的函数，有兴趣的可以去深入了解这个函数
       c("Apparent","Ideal","Bias-corrected"), #表示曲线名称的集合
       lty = c(2,1,1), #lty表示线条类型，1代表实线，2至6都是虚线，虚的程度不一样
       lwd = c(3,3,3), #lwd表示宽度，以默认值的相对大小来表示宽度
       col = c("black","#FF0000","#55cbff"),  #给曲线添加颜色，对应上面c("Ax","Ix","Bx")
       bty = "n",
       cex = 1.1)



#univariable and mutivariable
library("autoReg")

overall.log <- glm(status~.,data=train_data,family=binomial) 
#overall.log <-glm(status ~ mWmBNR+CmW+c+CmBNR+mBNR, data = train_data, family = binomial)
#overall.log <-glm(status ~ mWmBNR+CmW+c+mBNR, data = train_data, family = binomial)
#overall.log <-glm(status ~ mWmBNR+CmW, data = train_data, family = binomial)
#overall.log <-glm(status ~ BNF+phases+Radscore, data = train_data, family = binomial)
model1<-autoReg(overall.log,uni=TRUE,multi=FALSE,threshold=0.05)
model1
model2<-autoReg(overall.log,uni=FALSE,milti=TRUE,threshold=0.05)
model2
model3<-autoReg(overall.log,uni=TRUE,multi=TRUE,threshold=0.05)
model3
#%>% myft() 
#pdf
result=autoReg(overall.log,uni=TRUE, multi=TRUE, threshold=0.05) %>% myft()
result

#DCA
library(readxl)
library(ResourceSelection)
library(pROC)
file_path <- "C:/Users/12292/Desktop/train 用于合并.xlsx"
#file_path <- "C:/Users/12292/Desktop/train_scale 1.xlsx"
train_data <- read_excel(file_path)
train_data[[1]] <- factor(train_data[[1]])
file1_path <- "C:/Users/12292/Desktop/test 用于合并.xlsx"
test_data <- read_excel(file1_path)
test_data[[1]] <- factor(test_data[[1]])
library(rmda)
library("autoReg")
library(ggplot2)
str(train_data$status)
train_data$status <- as.numeric(as.character(train_data$status))
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")

Nomogram<- decision_curve(status~BNF+phases+Radscore,data =train_data, 
                          family = binomial(link ='logit'),#模型类型，这里是二分类
                          thresholds= seq(0,1, by = 0.01),
                          confidence.intervals =0.95,#95可信区间
                          study.design = 'case-control')
List<-list(Nomogram)
par(mgp=c(2.6,0.85,0),mar=c(5,5,2,2)) 
par(cex.axis = 1.4,cex.lab = 1.4)
par(family="Times")
plot_decision_curve(List,
                    curve.names = c('Nomogram'),
                    cost.benefit.axis = FALSE,
                    col = c('#46B8DAFF'),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab="High Risk Threshold",
                    cex = 1.4,  # 整个绘图的默认文本大小
                    cex.axis = 1.4,  # 坐标轴刻度标签的大小
                    cex.lab = 1.4,  # 坐标轴标签的大小
                    cex.main = 1.4,  # 主标题的大小
                    cex.sub = 1.4)  # 副标题的大小


library(rmda)
library("autoReg")
library(ggplot2)
str(test_data$status)
test_data$status <- as.numeric(as.character(test_data$status))
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")

Nomogram_test<- decision_curve(status~BNF+phases+Radscore,data =test_data, 
                          family = binomial(link ='logit'),#模型类型，这里是二分类
                          thresholds= seq(0,1, by = 0.01),
                          confidence.intervals =0.95,#95可信区间
                          study.design = 'case-control')
List<-list(Nomogram_test)
par(mgp=c(2.6,0.85,0),mar=c(5,5,2,2)) 
par(cex.axis = 1.4,cex.lab = 1.4)
par(family="Times")
plot_decision_curve(List,
                    curve.names = c('Nomogram'),
                    cost.benefit.axis = FALSE,
                    col = c('#46B8DAFF'),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab="High Risk Threshold",
                    cex = 1.4,  # 整个绘图的默认文本大小
                    cex.axis = 1.4,  # 坐标轴刻度标签的大小
                    cex.lab = 1.4,  # 坐标轴标签的大小
                    cex.main = 1.4,  # 主标题的大小
                    cex.sub = 1.4)  # 副标题的大小

#train dca
str(train_data$status)
train_data$status <- as.numeric(as.character(train_data$status))
# Fit models
fit1 <- lrm(status ~ BNF + Radscore + phases, data = train_data, x = TRUE, y = TRUE)
fit2 <- lrm(status ~ Radscore, data = train_data, x = TRUE, y = TRUE)
fit3 <- lrm(status ~ phases,data = train_data,x=TRUE,y=TRUE)
fit4 <- lrm(status ~ BNF+HW,data = train_data,x=TRUE,y=TRUE)
# Generate predictions for test data
bc_test <- test_data

phat1 <- predict(fit1, newdata = bc_test)
phat2 <- predict(fit2, newdata = bc_test)
phat3 <- predict(fit3, newdata = bc_test)
phat4 <- predict(fit4, newdata = bc_test)
bc_test$phat1 <- phat1
bc_test$phat2 <- phat2
bc_test$phat3 <- phat3
bc_test$phat4 <- phat4
# Generate decision curves
Nomogram1 <- decision_curve(status ~ bc_test$phat1, data = bc_test, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')

Nomogram2 <- decision_curve(status ~ Radscore, data = train_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogram3 <- decision_curve(status ~ phases, data = train_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogram4 <- decision_curve(status ~ BNF+HW, data = train_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
# Combine the decision curves into a list
List <- list(Nomogram1, Nomogram2,Nomogram3,Nomogram4)

# Plot the decision curves
par(mgp = c(2.6, 0.85, 0), mar = c(6, 6, 2, 2)) 
par(family = "Times")
# 调用 plot_decision_curve 函数
plot_decision_curve(List,
                    curve.names = c('Nomogram', 'Radscore', 'PHASES', 'Morphology'),
                    cost.benefit.axis = FALSE,
                    col = c("#B73508", "#193E8F", "#D9AE2C", "#64894D"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab = "",  # 先不设置x轴标签
                    lwd = 3,
                    cex = 1.4,  # 整个绘图的默认文本大小
                    cex.axis = 1.4,  # 坐标轴刻度标签的大小
                    cex.lab =2,  # 坐标轴标签的大小
                    cex.main = 3,  # 主标题的大小
                    cex.sub = 3)  # 副标题的大小
# 使用 mtext 单独添加 x 轴标签，并设置字体大小
mtext("High Risk Threshold", side = 1, line = 3, cex = 2)  # 修改x轴标签的字体大小为2倍

library(rms)
#test_dca
str(test_data$status)
test_data$status <- as.numeric(as.character(test_data$status))
# Fit models
fit1 <- lrm(status ~ BNF + Radscore + phases, data = test_data, x = TRUE, y = TRUE)
fit2 <- lrm(status ~ Radscore, data = test_data, x = TRUE, y = TRUE)
fit3 <- lrm(status ~ phases,data = test_data,x=TRUE,y=TRUE)
fit4 <- lrm(status ~ BNF+HW,data = test_data,x=TRUE,y=TRUE)
# Generate predictions for test data
bc_test <- test_data
phat1 <- predict(fit1, newdata = bc_test)
phat2 <- predict(fit2, newdata = bc_test)
phat3 <- predict(fit3, newdata = bc_test)
phat4 <- predict(fit4, newdata = bc_test)
bc_test$phat1 <- phat1
bc_test$phat2 <- phat2
bc_test$phat3 <- phat3
bc_test$phat4 <- phat4
# Generate decision curves
Nomogram1 <- decision_curve(status ~ BNF + phases + Radscore, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')

Nomogram2 <- decision_curve(status ~ Radscore, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogram3 <- decision_curve(status ~ phases, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogram4 <- decision_curve(status ~ BNF+HW, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
# Combine the decision curves into a list
List <- list(Nomogram1, Nomogram2,Nomogram3,Nomogram4)

# Plot the decision curves
par(mgp = c(2.6, 0.85, 0), mar = c(6, 6, 2, 2)) 
par(family = "Times")
# 调用 plot_decision_curve 函数
plot_decision_curve(List,
                    curve.names = c('Nomogram', 'Radscore', 'PHASES', 'Morphology'),
                    cost.benefit.axis = FALSE,
                    col = c("#B73508", "#193E8F", "#D9AE2C", "#64894D"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab = "",  # 先不设置x轴标签
                    lwd = 3,
                    cex = 1.4,  # 整个绘图的默认文本大小
                    cex.axis = 1.4,  # 坐标轴刻度标签的大小
                    cex.lab =2,  # 坐标轴标签的大小
                    cex.main = 3,  # 主标题的大小
                    cex.sub = 3)  # 副标题的大小
# 使用 mtext 单独添加 x 轴标签，并设置字体大小
mtext("High Risk Threshold", side = 1, line = 3, cex = 2)  # 修改x轴标签的字体大小为2倍



Nomogram1<- decision_curve(status~phat,data =test_data, 
                      family = binomial(link ='logit'),#模型类型，这里是二分类
                      thresholds= seq(0,1, by = 0.01),
                      confidence.intervals =0.95,#95可信区间
                      study.design = 'case-control')
List<-list(Nomogram1)
plot_decision_curve(List,
                    curve.names=c('Nomogram'),
                    cost.benefit.axis =FALSE,col= c('red'),
                    confidence.intervals=FALSE,
                    standardize = FALSE)
form.bestglm <- as.formula(status~BNF+PHASES+Radscore)


install.packages("ggDCA")
library(ggDCA)