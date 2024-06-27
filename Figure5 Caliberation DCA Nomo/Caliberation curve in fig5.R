library(calibrate)
library(ggplot2)
library(pROC)
library(rms)
library(readxl)

file_path <- "C:/Users/12292/Desktop/train size - 副本 (2).xlsx"
train_data <- read_excel(file_path)

file1_path <- "C:/Users/12292/Desktop/test size.xlsx"
test_data <- read_excel(file1_path)

library(pROC)
#Fit a Logistic Regression Model using the rms package:
library(rms)
ddist <- datadist(train_data)  
options(datadist='ddist')
logistic_model_rms <- lrm(status ~ BNF + Radscore + PHASES, data = train_data, x = TRUE, y = TRUE)

#Create a Nomogram:
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

#Calibration Curve for Training Data
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
#train caliberation curve
set.seed(123)
bc_train <- train_data
bc_test <- test_data
fit1<-lrm(status ~ BNF+PHASES+Radscore,
          x = TRUE, y = TRUE,
          data = bc_train )
cal1 <- calibrate(fit1, method = "boot", B=1000)

par(mgp = c(2.6, 0.85, 0), mar = c(6, 6, 3, 3))
par(family = "Times")

plot(cal1,
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Nomogram Predicted Probability",
     ylab = "Actual Probability",
     legend = FALSE,
     subtitles = FALSE,
     cex.axis = 1.4,
     cex.lab = 2, cex.sub = 0.9)

abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(cal1[, c("predy", "calibrated.orig")], type = "l", lwd = 4.5, col = "#f72f2f", pch = 16)
lines(cal1[, c("predy", "calibrated.corrected")], type = "l", lwd = 3, col = "#55cbff", pch = 16)

legend(0.45, 0.25,
       c("Apparent", "Ideal", "Bias-corrected"), 
       lty = c(2, 1, 1), 
       lwd = c(3, 3, 3), 
       col = c("black", "#f72f2f", "#55cbff"),
       bty = "n",
       cex = 1.1)

#test caliberation curve
phat <- predict(fit1, newdata = bc_test)
bc_test$phat <- phat

fit2 <- lrm(status ~ phat, data = bc_test, x = TRUE, y = TRUE)
cal2 <- calibrate(fit2, method = "boot", B = 1000)

par(mgp = c(2.6, 0.85, 0), mar = c(6, 6, 3, 3))

plot(cal2,
     xlim = c(0, 1),
     ylim = c(0, 1),
     xlab = "Nomogram Predicted Probability",
     ylab = "Actual Probability",
     legend = FALSE,
     subtitles = FALSE,
     cex.axis = 1.4,
     cex.lab = 2, cex.sub = 0.9)

abline(0, 1, col = "black", lty = 2, lwd = 2)
lines(cal2[, c("predy", "calibrated.orig")], type = "l", lwd = 4.5, col = "#FF0000", pch = 16)
lines(cal2[, c("predy", "calibrated.corrected")], type = "l", lwd = 3, col = "#55cbff", pch = 16)

legend(0.45, 0.25,
       c("Apparent", "Ideal", "Bias-corrected"), 
       lty = c(2, 1, 1), 
       lwd = c(3, 3, 3), 
       col = c("black", "#FF0000", "#55cbff"),  
       bty = "n",
       cex = 1.1)



