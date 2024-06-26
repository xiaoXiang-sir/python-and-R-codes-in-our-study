library(readxl)
library(car)
# Load data from Excel file
file_path <- "C:/Users/12292/Desktop/corr/glcm.xlsx"
data <- read_excel(file_path)

# Function to calculate median and 95% confidence interval
calculate_median_ci <- function(subset_data) {
  median_value <- median(subset_data$Value)
  ci <- quantile(subset_data$Value, c(0.025, 0.975))
  return(list(median = median_value, ci = ci))
}
# Calculate median and 95% CI for status = 0 and var = original_firstorder_Skewness
result_0_skw <- calculate_median_ci(subset(data, status == 0 & var == "original_firstorder_Skewness"))
# Calculate median and 95% CI for status = 1 and var = original_firstorder_Skewness
result_1_skw <- calculate_median_ci(subset(data, status == 1 & var == "original_firstorder_Skewness"))

# Print results for original_firstorder_Skewness
cat("status = 0, var = original_firstorder_Skewness: Median =", result_0_skw$median, ", 95% CI =", result_0_skw$ci, "\n")
cat("status = 1, var = original_firstorder_Skewness: Median =", result_1_skw$median, ", 95% CI =", result_1_skw$ci, "\n")

# Calculate median and 95% CI for status = 0 and var = original_glcm_Idmn
result_0_idmn <- calculate_median_ci(subset(data, status == 0 & var == "original_glcm_Idmn"))

# Calculate median and 95% CI for status = 1 and var = original_glcm_Idmn
result_1_idmn <- calculate_median_ci(subset(data, status == 1 & var == "original_glcm_Idmn"))

# Print results for original_glcm_Idmn
cat("status = 0, var = original_glcm_Idmn: Median =", result_0_idmn$median, ", 95% CI =", result_0_idmn$ci, "\n")
cat("status = 1, var = original_glcm_Idmn: Median =", result_1_idmn$median, ", 95% CI =", result_1_idmn$ci, "\n")

# Calculate median and 95% CI for status = 0 and var = original_ngtdm_Coarseness
result_0_Coa <- calculate_median_ci(subset(data, status == 0 & var == "original_ngtdm_Coarseness"))

# Calculate median and 95% CI for status = 1 and var = original_ngtdm_Coarseness
result_1_Coa <- calculate_median_ci(subset(data, status == 1 & var == "original_ngtdm_Coarseness"))

# Print results for original_ngtdm_Coarseness
cat("status = 0, var = original_ngtdm_Coarseness: Median =", result_0_Coa$median, ", 95% CI =", result_0_Coa$ci, "\n")
cat("status = 1, var = original_ngtdm_Coarseness: Median =", result_1_Coa$median, ", 95% CI =", result_1_Coa$ci, "\n")
