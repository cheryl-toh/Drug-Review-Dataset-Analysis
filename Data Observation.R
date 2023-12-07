# Install the required package
install.packages("readr")
library(readr)

# Load Train dataset and Test dataset (.tsv files)
file_path <- "./Dataset/drugsComTest_raw.tsv"
test_data <- read_tsv(file_path)

file_path2 <- "./Dataset/drugsComTrain_raw.tsv"
train_data <- read_tsv(file_path2)

# Convert .tsv files into .csv file or DATAFRAME
test_df <- as.data.frame(test_data)
train_df <- as.data.frame(train_data)

# Print the dimensions of the data frame
print(head(test_df))
write.csv(test_df, file = "testing.csv", row.names = FALSE)

print(head(train_df))
write.csv(train_df, file = "training.csv", row.names = FALSE)

#Merge Data
merged_data <- rbind(test_df, train_df)
print(merged_data)
write.csv(merged_data, file = "fullData.csv")

# Plot graph 1 (Observation 1)

# Plot graph 2 (Observation 2)

# Plot graph 3 (Observation 3)

# Plot graph 4 (Observation 4)
