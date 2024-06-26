library(readxl)
library(car)

file_path <- "C:/Users/12292/Desktop/scale_train_xgx.xlsx"
data <- read_excel(file_path)
data <- data[, -1]
data <- data[, -1]
library(corrplot)
windowsFonts(Times = windowsFont("Times New Roman"))
par(family = "Times")
tdc <- cor (data, method="pearson")
addcol <- colorRampPalette(c("#FFFF00FF", "#EEA236FF", "#C71000FF")) 
testRes <- cor.mtest(data, method = "pearson", conf.level = 0.95)
corrplot(tdc, method = "color", col = addcol(100), 
         tl.col = "black", tl.cex = 1.4, tl.srt = 45, tl.pos = "lt",
         p.mat = testRes$p, diag = TRUE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.4,
         insig = 'label_sig', pch.col = 'grey20')
corrplot(tdc, method = "number", type = "lower", col = addcol(100), 
         tl.col = "n", tl.cex = 1.2, tl.pos = "n", cl.pos = "n",
         add = TRUE)

#corrplot(tdc, method = "number", type = "lower", col = addcol(100), 
#         tl.col = "n", tl.cex = 0.6, tl.pos = "n",cl.pos = "n",
#         add = TRUE)


#scatter plots
library(ggpubr)
library(ggplot2)
library(gridExtra)

# Plot 1
plot1 <- ggplot(data, aes(x = HW, y = Elongation)) +
  geom_point(size = 1, color = "#FF7F00") +  # Change point color to orange
  geom_smooth(method = "lm", se = TRUE, aes(color = "#AD002AFF"), show.legend = FALSE) +  # Linear regression line
  labs(x = "Height/Width",
       y = "Elongation") +
  scale_size_continuous(range = c(2, 10)) +
  stat_cor(method = "pearson", label.x = 1.5, label.y = 1.0, size = 3)  # Pearson correlation

# Plot 2
plot2 <- ggplot(data, aes(x = Hmax, y = Max2DDiameterSlice)) +
  geom_point(size = 1, color = "#FF7F00") +  # Change point color to orange
  geom_smooth(method = "lm", se = TRUE, aes(color = "#AD002AFF"), show.legend = FALSE) +  # Linear regression line
  labs(x = "Hmax",
       y = "Max2DDiameterSlice") +
  scale_size_continuous(range = c(2, 10)) +
  stat_cor(method = "pearson", label.x = 1.8, label.y = 3, size = 3)  # Pearson correlation

# Plot 3
plot3 <- ggplot(data, aes(x = HW, y = Sphericity)) +
  geom_point(size = 1, color = "#FF7F00") +  # Change point color to orange
  geom_smooth(method = "lm", se = TRUE, aes(color = "#AD002AFF"), show.legend = FALSE) +  # Linear regression line
  labs(x = "Height/Width",
       y = "Sphericity") +
  scale_size_continuous(range = c(2, 10)) +
  stat_cor(method = "pearson", label.x = 1.5, label.y = 1.5, size = 3)  # Pearson correlation

# Plot 4
plot4 <- ggplot(data, aes(x = HW, y = Flatness)) +
  geom_point(size = 1, color = "#FF7F00") +  # Change point color to orange
  geom_smooth(method = "lm", se = TRUE, aes(color = "#AD002AFF"), show.legend = FALSE) +  # Linear regression line
  labs(x = "Height/Width",
       y = "Flatness") +
  scale_size_continuous(range = c(2, 10)) +
  stat_cor(method = "pearson", label.x = 1.5, label.y = 1.9, size = 3)  # Pearson correlation

# Arrange plots into a 2x2 grid
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)