library(readxl)
library(car)

file_path <- "C:/Users/12292/Desktop/corr/shape - xx.xlsx"
#file_path <- "C:/Users/12292/Desktop/corr/train radiomics shape - xx.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
#data$status <- factor(data$status)


# Set Times New Roman font
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")

# Create the plot
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.4, size = 1, linetype = "longdash", col = c("#CC011F", "#68A3ED")) +  
  geom_boxplot(size = 1, fill = "white", linetype = "solid", col = c("#CC011F", "#68A3ED"), outlier.shape = NA) +   
  geom_jitter(aes(color = Group), width = 0.2, shape = 20, size = 2.5) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c("#CC011F", "#68A3ED", "#CC011F", "#68A3ED"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  ylim(0.35, 1.03) +
  theme(axis.title.x = element_blank(),  # Hide x-axis title
        axis.title = element_text(size = 17, color = "black", family = "Times"),  
        legend.title = element_text(size = 15, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 17, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "right",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 17, color = "black", family = "Times"), # Change facet title font size
        strip.background = element_rect(fill = "#EFC000")) +  # Change facet background color
  guides(fill = "none")  # Hide the legend for the "Category" variable

#elongation
file_path <- "C:/Users/12292/Desktop/corr/shape - xx.xlsx"
data <- read_excel(file_path)
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
par(mar=c(5,5,3,2))
library(ggplot2)
library(ggpubr)

ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) +
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.2,label.y=1.03, width = 0.22) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(0.35, 1.12), breaks = c(0.5, 0.7, 0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")




#sphericity
  
file_path <- "C:/Users/12292/Desktop/corr/sphericity.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
library(ggplot2)
library(ggpubr)


ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) + 
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.1,label.y=0.89) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(0.64, 0.92), breaks = c(0.7, 0.8,0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")



#Max2DDslice
file_path <- "C:/Users/12292/Desktop/corr/Max2Dslice.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) + 
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.35,label.y=11.2) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(1.4, 12.3), breaks = c(2.5, 5.0,7.5,10.0,12.5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")


#ske
file_path <- "C:/Users/12292/Desktop/corr/ske.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) +
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.3) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(-0.9, 0.9), breaks = c(-0.8,-0.4,0,0.4,0.8))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")


#idmn
file_path <- "C:/Users/12292/Desktop/corr/idmn.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) +
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.1,label.y=0.983) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(0.915, 0.989), breaks = c(0.92,0.95,0.98))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")


#ngtdm
file_path <- "C:/Users/12292/Desktop/corr/ngtdm.xlsx"
data <- read_excel(file_path)
library("ggunchained")
library(ggplot2)
library(reshape2) 
windowsFonts(Times =windowsFont("Times New Roman"))
par(family="Times")
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.3, size = 1, linetype = 1, col = c("black")) +  
  geom_boxplot(size =0.9,width = 0.62,fill = c("#CC011F","#68A3ED"), linetype = "solid", col = c("black"), outlier.shape = NA) +
  geom_jitter(color = "#5f5f5f", width = 0.22, shape = 20, size = 1.8) +
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.35,label.y=0.0165) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "black","black"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(0.0, 0.018), breaks = c(0.005,0.010,0.015))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_text(size = 28, color = "black", family = "Times"),  
        legend.title = element_text(size = 28, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 28, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 24.5,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")





#Another metods
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.4, size = 1, linetype = "longdash", col = c("#68A3ED", "#EFC000")) +  
  geom_boxplot(size = 1, fill = "white", linetype = "solid", col = c("#68A3ED", "#EFC000"), outlier.shape = NA) +   
  geom_jitter(aes(color = Group), width = 0.2, shape = 20, size = 2.5) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c("#68A3ED", "#EFC000"), labels = c("Ruptured", "Unruptured"), limits = c("Ruptured", "Unruptured")) +
  theme_bw() +
  ylim(0.35, 1.03) +
  theme(axis.title = element_text(size = 17, color = "black", family = "Times"),  
        legend.title = element_text(size = 15, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 17, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "right",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 17,color = "black",family = "Times"), 
        strip.background = element_rect(fill = "lightblue")) + 
  guides(fill = "none")  # Hide the legend for the "Category" variable


# Function to calculate median and 95% confidence interval
calculate_median_ci <- function(subset_data) {
  median_value <- median(subset_data$Value)
  ci <- quantile(subset_data$Value, c(0.025, 0.975))
  return(list(median = median_value, ci = ci))
}

# Calculate medians and CIs for various conditions
result_0_max2d <- calculate_median_ci(subset(data, status == 0 & var == "Maximum2DDiameterSlice"))
result_1_max2d <- calculate_median_ci(subset(data, status == 1 & var == "Maximum2DDiameterSlice"))
result_0_majlength <- calculate_median_ci(subset(data, status == 0 & var == "MajorAxisLength"))
result_1_majlength <- calculate_median_ci(subset(data, status == 1 & var == "MajorAxisLength"))

# Display results for specific conditions
cat("status = 0, var = Maximum2DDiameterSlice: Median =", result_0_max2d$median, ", 95% CI =", result_0_max2d$ci, "\n")
cat("status = 1, var = Maximum2DDiameterSlice: Median =", result_1_max2d$median, ", 95% CI =", result_1_max2d$ci, "\n")
cat("status = 0, var = MajorAxisLength: Median =", result_0_majlength$median, ", 95% CI =", result_0_majlength$ci, "\n")
cat("status = 1, var = MajorAxisLength: Median =", result_1_majlength$median, ", 95% CI =", result_1_majlength$ci, "\n")

# Calculate medians and CIs for additional variables
result_0_elongation <- calculate_median_ci(subset(data, status == 0 & var == "Elongation"))
result_1_elongation <- calculate_median_ci(subset(data, status == 1 & var == "Elongation"))
result_0_flatness <- calculate_median_ci(subset(data, status == 0 & var == "Flatness"))
result_1_flatness <- calculate_median_ci(subset(data, status == 1 & var == "Flatness"))
result_0_sphericity <- calculate_median_ci(subset(data, status == 0 & var == "Sphericity"))
result_1_sphericity <- calculate_median_ci(subset(data, status == 1 & var == "Sphericity"))

# Display results for additional variables
cat("status = 0 and var = Elongation: Median and 95% CI:\n")
print(result_0_elongation)

cat("\nstatus = 1 and var = Elongation: Median and 95% CI:\n")
print(result_1_elongation)

cat("\nstatus = 0 and var = Flatness: Median and 95% CI:\n")
print(result_0_flatness)

cat("\nstatus = 1 and var = Flatness: Median and 95% CI:\n")
print(result_1_flatness)

cat("\nstatus = 0 and var = Sphericity: Median and 95% CI:\n")
print(result_0_sphericity)

cat("\nstatus = 1 and var = Sphericity: Median and 95% CI:\n")
print(result_1_sphericity)

# Create a data frame containing all results with formatted numbers
results_all <- data.frame(
  status_var = c("status = 0, var = Elongation", "status = 1, var = Elongation",
                 "status = 0, var = Flatness", "status = 1, var = Flatness",
                 "status = 0, var = Sphericity", "status = 1, var = Sphericity"),
  median = format(round(c(result_0_elongation$median, result_1_elongation$median,
                          result_0_flatness$median, result_1_flatness$median,
                          result_0_sphericity$median, result_1_sphericity$median), 2), nsmall = 2),
  ci_lower = format(round(c(result_0_elongation$ci[1], result_1_elongation$ci[1],
                            result_0_flatness$ci[1], result_1_flatness$ci[1],
                            result_0_sphericity$ci[1], result_1_sphericity$ci[1]), 2), nsmall = 2),
  ci_upper = format(round(c(result_0_elongation$ci[2], result_1_elongation$ci[2],
                            result_0_flatness$ci[2], result_1_flatness$ci[2],
                            result_0_sphericity$ci[2], result_1_sphericity$ci[2]), 2), nsmall = 2)
)

# Print the data frame
print(results_all)


#Another method
ggplot(data, aes(x = Group, y = Value, fill = Category)) +
  stat_boxplot(geom = "errorbar", width = 0.425, size = 1.2, linetype = 1, col = c("#CC011F","#68A3ED")) +  
  geom_boxplot(size = 1.1, fill = "white", linetype = "solid", col = c("#CC011F","#68A3ED"), outlier.shape = NA) +   
  geom_jitter(aes(color = Group), width = 0.22, shape = 20, size = 2.2) +
  stat_compare_means(label="p.signif", method="t.test", comparisons=list(c("Unruptured", "Ruptured")),
                     method.args=list(alternative="two.sided", paired=FALSE), size=6,tip.length=0.1,label.y=1.03) +
  facet_grid(. ~ Category, scales = "free_x", space = "free") +
  scale_color_manual(values = c( "#CC011F", "#68A3ED", "#CC011F","#68A3ED"), labels = c("Ruptured", "Unruptured")) +
  theme_bw() +
  scale_y_continuous(limits = c(0.35, 1.12), breaks = c(0.5, 0.7, 0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title = element_text(size = 17, color = "black", family = "Times"),  
        legend.title = element_text(size = 15, color = "black", family = "Times"), 
        legend.text = element_text(size = 13, color = "black", family = "Times"),
        axis.text = element_text(size = 17, color = "black", family = "Times"),  
        panel.background = element_rect(colour = "black", fill = NA),  
        panel.grid.minor = element_blank(),  
        legend.position = "none",  
        legend.background = element_rect(colour = NA, fill = NA),  
        axis.ticks = element_line(colour = "black"),
        strip.text = element_text(size = 17,color = "black",family = "Times", margin = margin(t = 5, b = 5)),
        strip.background = element_rect(fill = "#EFC000")) +
  guides(fill ="none")
