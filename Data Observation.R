# Install the required package
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load dataset (.tsv files)
file_path <- "./Dataset/drugsComTest_raw.tsv"

data <- read_tsv(file_path)

# Convert .tsv files into .csv file or DATAFRAME
df <- as.data.frame(data)

# Print the dimensions of the data frame
print(head(df))

# Save as csv file
write.csv(df, file = "data.csv", row.names = FALSE)



# Plot graph 1 (Observation 1)

# Create a new data frame with the average rating for each drug
average_rating_per_drug <- df %>%
  group_by(drugName) %>%
  summarise(average_rating = mean(rating, na.rm = TRUE))

# Plot histogram for average rating
histogram_plot <- ggplot(average_rating_per_drug, aes(x = average_rating)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Average Rating per Drug",
       x = "Average Rating",
       y = "Frequency") +
  theme_minimal()

# Save the plot as a PNG file
ggsave("Images/average_rating_histogram.png", histogram_plot)

# View the plot
print(histogram_plot)




# Plot graph 2 (Observation 2)
# Count words in the reviews column and create a new column with the counts (up to 200 words)
df <- df %>%
  mutate(words_count = pmin(str_count(review, "\\S+"), 200))  # Consider up to 200 words

# Plot a bar chart of word counts
word_count_plot <- ggplot(df, aes(x = cut(words_count, breaks = seq(0, 200, by = 10)), fill = cut(words_count, breaks = seq(0, 200, by = 10)))) +
  geom_bar() +
  labs(title = "Distribution of Review Word Counts (Up to 200 Words)",
       x = "Number of Words (in multiples of 10)",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")

# Print the plot
print(word_count_plot)

# Save the plot as a PNG file
ggsave("Images/Word count distribution.png", word_count_plot)

