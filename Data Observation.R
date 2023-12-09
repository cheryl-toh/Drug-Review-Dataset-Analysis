# Install the required package
install.packages("readr")
library(readr)

# Load Train dataset and Test dataset (.tsv files)
file_path <- "./Dataset/drugsComTest_raw.tsv"

test_data <- read_tsv(file_path)

# Convert .tsv files into .csv file or DATAFRAME
test_df <- as.data.frame(test_data)

# Print the dimensions of the data frame
print(head(test_df))
write.csv(test_df, file = "data.csv", row.names = FALSE)


# Plot graph 1 (Observation 1)

# Plot graph 2 (Observation 2)

# Plot graph 3 (Observation 3)

# Plot graph 4 (Observation 4)
