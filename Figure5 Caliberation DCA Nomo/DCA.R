#DCA
library(readxl)
library(ResourceSelection)
library(pROC)
library(rmda)
library(ggplot2)
file_path <- "C:/Users/12292/Desktop/train yyhb.xlsx"
#file_path <- "C:/Users/12292/Desktop/train_scale 1.xlsx"
train_data <- read_excel(file_path)
train_data[[1]] <- factor(train_data[[1]])
file1_path <- "C:/Users/12292/Desktop/test yyhb.xlsx"
test_data <- read_excel(file1_path)
test_data[[1]] <- factor(test_data[[1]])

#train dca
str(train_data$status)
train_data$status <- as.numeric(as.character(train_data$status))
Nomogram1 <- decision_curve(status ~ BNF + Radscore + phases, data = train_data, 
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
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")

plot_decision_curve(List,
                    curve.names = c('Nomogram', 'Radscore', 'PHASES', 'Morphology'),
                    cost.benefit.axis = FALSE,
                    col = c("#B73508", "#193E8F", "#D9AE2C", "#64894D"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab = "",
                    lwd = 3,
                    cex = 1.4,
                    cex.axis = 1.4,
                    cex.lab = 2,
                    cex.main = 3,
                    cex.sub = 3)

mtext("High Risk Threshold", side = 1, line = 3, cex = 2)


modelRMP <- glm(status ~ BNF + Radscore + phases, 
                family=binomial(),
                data = train_data)
modelR <- glm(status ~ Radscore, 
              family=binomial(),
              data = train_data)
modelM <- glm(status ~ BNF+HW, 
              family=binomial(),
              data = train_data)
modelP <- glm(status ~ phases, 
              family=binomial(),
              data = train_data)
#test dca
test_data$phatrmp <- predict(modelRMP, newdata = test_data, type="response")
test_data$phatr <- predict(modelR, newdata = test_data, type="response")
test_data$phatm <- predict(modelM, newdata = test_data, type="response")
test_data$phatp <- predict(modelP, newdata = test_data, type="response")

str(test_data$status)
test_data$status <- as.numeric(as.character(test_data$status))

Nomogramt1 <- decision_curve(status ~ phatrmp, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')

Nomogramt2 <- decision_curve(status ~ phatr, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogramt3 <- decision_curve(status ~ phatp, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
Nomogramt4 <- decision_curve(status ~ phatm, data = test_data, 
                            family = binomial(link = 'logit'),
                            thresholds = seq(0, 1, by = 0.01),
                            confidence.intervals = 0.95,
                            study.design = 'case-control')
# Combine the decision curves into a list
List <- list(Nomogramt1, Nomogramt2,Nomogramt3,Nomogramt4)

# Plot the decision curves
par(mgp = c(2.6, 0.85, 0), mar = c(6, 6, 2, 2))
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")

plot_decision_curve(List,
                    curve.names = c('Nomogram', 'Radscore', 'PHASES', 'Morphology'),
                    cost.benefit.axis = FALSE,
                    col = c("#B73508", "#193E8F", "#D9AE2C", "#64894D"),
                    confidence.intervals = FALSE,
                    standardize = FALSE,
                    xlab = "",
                    lwd = 3,
                    cex = 1.4,
                    cex.axis = 1.4,
                    cex.lab = 2,
                    cex.main = 3,
                    cex.sub = 3)

mtext("High Risk Threshold", side = 1, line = 3, cex = 2)
