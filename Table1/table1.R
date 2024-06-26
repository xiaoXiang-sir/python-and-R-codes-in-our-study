# Load necessary libraries
library(MatchIt)
library(readxl)
library(tableone)
library(stats)

# Stratified by train 0=test 1=Train
# Load the dataset
file_path <- "C:\\Users\\12292\\Desktop\\simple random\\1.cy 20001015\\train size - xx.xlsx"  
data <- read_excel(file_path)  
head(data)

# Convert specific columns to factors
data$sex <- factor(data$sex)
data$status <- factor(data$status)
data$HH <- factor(data$HH)
data$shape <- factor(data$shape)
data$hypertension <- factor(data$hypertension)
data$diabetes <- factor(data$diabetes)
data$CHD <- factor(data$CHD)
data$smoking <- factor(data$smoking)
data$alcoholism <- factor(data$alcoholism)
data$NumberOfAneurysm <- factor(data$NumberOfAneurysm)
data$Location <- factor(data$Location)
data$aSAHfamilyhistory <- factor(data$aSAHfamilyhistory)
data$aSAHhistory <- factor(data$aSAHhistory)

# Perform Kolmogorov-Smirnov (KS) tests
ks_test_age <- ks.test(data$age, "pnorm", mean = mean(data$age), sd = sd(data$age))
print(ks_test_age)

ks_result_mBNR <- ks.test(data$mBNR, "pnorm", mean(data$mBNR), sd(data$mBNR))
ks_result_mW <- ks.test(data$mW, "pnorm", mean(data$mW), sd(data$mW))
ks_result_c <- ks.test(data$c, "pnorm", mean(data$c), sd(data$c))
ks_result_CmW <- ks.test(data$CmW, "pnorm", mean(data$CmW), sd(data$CmW))
ks_result_CmBNR <- ks.test(data$CmBNR, "pnorm", mean(data$CmBNR), sd(data$CmBNR))
ks_result_mWmBNR <- ks.test(data$mWmBNR, "pnorm", mean(data$mWmBNR), sd(data$mWmBNR))

# Print KS test results
print(ks_result_mBNR)
print(ks_result_mW)
print(ks_result_c)
print(ks_result_CmW)
print(ks_result_CmBNR)
print(ks_result_mWmBNR)

ks_test_phases <- ks.test(data$phases, "pnorm", mean = mean(data$phases), sd = sd(data$phases))
print(ks_test_phases)

ks_test_elapss <- ks.test(data$elapss, "pnorm", mean = mean(data$elapss), sd = sd(data$elapss))
print(ks_test_elapss)

nonvar <- c("elapss", "phases", "mBNR", "mW", "c", "CmW", "CmBNR", "mWmBNR")

# Create table one
tab_s <- CreateTableOne(data = data, 
                        vars = colnames(data)[-1], 
                        strata = "train",
                        addOverall = TRUE)

print(tab_s, nonnormal = nonvar, showAllLevels = TRUE)
Y <- print(tab_s, nonnormal = nonvar, showAllLevels = FALSE)
# write.csv(Y, file = "train and test.csv")
Y3 <- print(tab_s, nonnormal = nonvar, showAllLevels = TRUE)
# write.csv(Y3, file = "train and test 3.csv")


#Stratified by status
# Load another dataset
file_path <- "C:\\Users\\12292\\Desktop\\simple random\\1.cy 20001015\\train size -qdID.xlsx"  
data1 <- read_excel(file_path)
data1$sex <- factor(data1$sex)
data1$status <- factor(data1$status)
data1$HH <- factor(data1$HH)
data1$shape <- factor(data1$shape)
data1$hypertension <- factor(data1$hypertension)
data1$diabetes <- factor(data1$diabetes)
data1$CHD <- factor(data1$CHD)
data1$smoking <- factor(data1$smoking)
data1$alcoholism <- factor(data1$alcoholism)
data1$NumberOfAneurysm <- factor(data1$NumberOfAneurysm)
data1$Location <- factor(data1$Location)
data1$aSAHfamilyhistory <- factor(data1$aSAHfamilyhistory)
data1$aSAHhistory <- factor(data1$aSAHhistory)

# Perform KS tests on the new dataset
train_KS_age <- ks.test(data1$age, "pnorm", mean = mean(data1$age), sd = sd(data1$age))
train_KS_mBNR <- ks.test(data1$mBNR, "pnorm", mean(data1$mBNR), sd(data1$mBNR))
train_KS_mW <- ks.test(data1$mW, "pnorm", mean(data1$mW), sd(data1$mW))
train_KS_c <- ks.test(data1$c, "pnorm", mean(data1$c), sd(data1$c))
train_KS_CmW <- ks.test(data1$CmW, "pnorm", mean(data1$CmW), sd(data1$CmW))
train_KS_CmBNR <- ks.test(data1$CmBNR, "pnorm", mean(data1$CmBNR), sd(data1$CmBNR))
train_KS_mWmBNR <- ks.test(data1$mWmBNR, "pnorm", mean(data1$mWmBNR), sd(data1$mWmBNR))
train_KS_phases <- ks.test(data1$phases, "pnorm", mean = mean(data1$phases), sd = sd(data1$phases))
print(train_KS_phases)

train_KS_elapss <- ks.test(data1$elapss, "pnorm", mean = mean(data1$elapss), sd = sd(data1$elapss))
print(train_KS_elapss)

# Print KS test results for the new dataset
print(train_KS_age)
print(train_KS_mBNR)
print(train_KS_mW)
print(train_KS_c)
print(train_KS_CmW)
print(train_KS_CmBNR)
print(train_KS_mWmBNR)

nonvar <- c("elapss", "phases", "mBNR", "mW", "c", "CmW", "CmBNR", "mWmBNR")

# Create table one for the new dataset
tab_1 <- CreateTableOne(data = data1, 
                        vars = colnames(data1)[-3], # Remove ID column
                        strata = "status",
                        addOverall = TRUE)

print(tab_1, nonnormal = nonvar, showAllLevels = TRUE)
Y1 <- print(tab_1, nonnormal = nonvar, showAllLevels = TRUE)

# write.csv(Y1, file = "Table 1_groups.csv")
