library(readxl)


file_path <- "C:/Users/12292/Desktop/train size - xx (2).xlsx"
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
# Color and identification
rect(0.175,0.225,0.515,0.245,col = "#46B8DAFF") # color
rect(0.515,0.225,0.605,0.245,col = "#FFFF00FF") # color
rect(0.605,0.225,0.96,0.245,col = "#C71000FF") # color
text(0.33, 0.273, "Low", cex = 1.1)
text(0.57, 0.273, "Medium", cex = 1.1)
text(0.80, 0.273, "High", cex = 1.1)


#NOMOSCORE
# 1. Obtain the coefficients and scores of each variable in the nomogram
coefficients <- coef(logistic_model_rms)
# 2. Obtain the value of each variable in the training data
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

train_data$nomogram<- linear_predictor 
test_data$nomogram<- linear_test_predictor
library(openxlsx)
# save
excel_file <- "C:/Users/12292/Documents/nomogram.xlsx"

# train_data/test_data data frame save as Excel
write.xlsx(train_data, file = excel_file, rowNames = FALSE)
excel_test_file <- "C:/Users/12292/Documents/nomogram test.xlsx"
write.xlsx(test_data, file = excel_test_file,rowNames = FALSE)
