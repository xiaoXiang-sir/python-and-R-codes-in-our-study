library(readxl)
library(irr)
library(dplyr)
library(writexl)

# This is for intra-ICC
data <- read_excel('c:/Users/12292/Desktop/321/ICC/modified_intra ICC.xlsx')
first_row_list <- colnames(data)[3:109]
selected_columns <- c()
icc_values <- list() 

for (col in first_row_list) {
  dataA <- data[, col]  # Select column dataA
  dataB <- data[, paste0('n', col)]  # Select column dataB based on col
  dataAB <- data.frame(dataA, dataB)  # Combine dataA and dataB into a dataframe
  icc_value <- icc(dataAB)$value  # Calculate ICC value
  
  if (icc_value > 0.8) {
    selected_columns <- c(selected_columns, col)  # Store the column name in selected_columns
  }
  
  # Store ICC value in the list
  icc_values[[col]] <- icc_value
}

print(icc_values)  # Print ICC values for each feature
print(selected_columns)  # Print column names that meet the condition
# Convert ICC values to a dataframe
icc_df <- data.frame(
  feature = names(icc_values),
  icc_value = unlist(icc_values)
)
write.csv(icc_df, file = "iccintra_values.csv", row.names = FALSE)


# Create a new dataframe (datar1) with selected columns
datar1 <- data[selected_columns]
datar2 <- data[paste0('n', selected_columns)]
datar = data.frame(datar1, datar2)
write_xlsx(datar, path='c:/Users/12292/Desktop/321/ICC/ICC 0.8 intra.xlsx')


# This is for inter-ICC
data <- read_excel('c:/Users/12292/Desktop/321/ICC/ICC inter.xlsx')
first_row_list <- colnames(data)[3:109]
selected_columns <- c()
icc_values <- list()  # Create an empty list to store ICC values for each feature

for (col in first_row_list) {
  dataA <- data[, col]  # Select column dataA
  dataB <- data[, paste0('a', col)]  # Select column dataB based on col
  dataAB <- data.frame(dataA, dataB)  # Combine dataA and dataB into a dataframe
  icc_value <- icc(dataAB)$value  # Calculate ICC value
  
  if (icc_value > 0.8) {
    selected_columns <- c(selected_columns, col)  # Store the column name in selected_columns
  }
  
  # Store ICC value in the list
  icc_values[[col]] <- icc_value
}

print(icc_values)  # Print ICC values for each feature
print(selected_columns)  # Print column names that meet the condition

# Convert ICC values to a dataframe
icc_df <- data.frame(
  feature = names(icc_values),
  icc_value = unlist(icc_values)
)

# Save as a CSV file
write.csv(icc_df, file = "icc_values.csv", row.names = FALSE)

