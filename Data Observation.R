# Install packages if not already installed
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}

if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)



# Function to clean and preprocess data
cleanAndPreprocessData <- function(df) {
  
  # data cleaning
  df$review <- HTMLdecode(df$review) #1-2 mins
  
  df$condition <- ifelse(grepl("</span> users found this comment helpful", df$condition),
                                 NA, df$condition)

  # Cleaning (removing incomplete drugname in conditions) and inputing conditions based on the review and what the medication does
  # min) into Diabetes, Type 2 -> (All under Metformin, or Dapagliflozin, Empagliflozin, Glyburide and Sitagliptin which is taken for Type 2 Diabetes)
  df$drugName <- gsub("Metformin / sitagliptin", "Janumet", df$drugName)
  df$drugName <- gsub("Metformin / saxagliptin", "Onglyza", df$drugName)
  df$drugName <- gsub("Saxagliptin", "Onglyza", df$drugName)
  df$drugName <- gsub("Linagliptin", "Tradjenta", df$drugName)
  df$drugName <- gsub("Empagliflozin / metformin", "Synjardy", df$drugName)
  
  df$condition <- gsub("min)", "Diabetes, Type 2", df$condition)
  df$condition <- gsub("min / sitagliptin)", "Diabetes, Type 2", df$condition)
  df$condition <- gsub("min / saxagliptin)", "Diabetes, Type 2", df$condition)
  
  # Dulera and Asthma <- Formoterol / mometasone is sold under the brand name Dulera
  df$drugName <- gsub("Formoterol / mometasone", "Dulera", df$drugName)
  df$condition <- gsub("moterol / mometasone)", "Asthma", df$condition)
  
  # COPD
  df$drugName <- gsub("Budesonide / formoterol", "Symbicort", df$drugName)
  df$condition <- gsub("mist (", "COPD", df$condition, fixed=TRUE)
  df$condition <- gsub("moterol)", "Asthma / COPD", df$condition) # symbicort for asthma or COPD
  
  # Mycophenolic acid
  df$condition <- gsub("tic (mycophenolic acid)", "Organ Transplant, Rejection Prophylaxis", df$condition, fixed=TRUE)
  
  # Exforge
  df$drugName <- gsub("Amlodipine / valsartan", "Exforge", df$drugName)
  df$drugName <- gsub("Exforge HCT", "Exforge", df$drugName)
  df$condition <- gsub("ge (amlodipine / valsartan)", "High Blood Pressure", df$condition, fixed=TRUE)
  
  # Pramoxine / zinc oxide
  df$condition <- gsub("Hemorrhoids (pramoxine / zinc oxide)", "Hemorrhoids", df$condition, fixed=TRUE)
  
  # nasal congestion
  df$condition <- gsub("mulation) (phenylephrine)", "Nasal Congestion", df$condition, fixed=TRUE)
  
  # Oxybutynin
  df$condition <- gsub("Women (oxybutynin)", "Urinary Incontinence / Prostatitis / Overactive Bladder / Hyperhidrosis", df$condition, fixed=TRUE)
  
  
  # uncompleted words
  df$condition <- gsub("zen Shoulde", "Frozen Shoulder", df$condition)
  df$condition <- gsub("Not Listed / Othe", "Not Listed or Other", df$condition)
  df$condition <- gsub("Overactive Bladde", "Overactive Bladder", df$condition)
  df$condition <- gsub("Disorde", "Disorder", df$condition)
  df$condition <- gsub("Disorderr", "Disorder", df$condition)
  df$condition <- gsub("Ulce", "Ulcer", df$condition)
  df$condition <- gsub("Ulcerr", "Ulcer", df$condition)
  df$condition <- gsub("Uveitis, Posteri", "Uveitis, Posterior", df$condition)
  df$condition <- gsub("cal Segmental Glomerulosclerosis", "Focal Segmental Glomerulosclerosis", df$condition)
  df$condition <- gsub("Somat", "Somatic Symptom Disorder", df$condition)
  df$condition <- gsub("m Pain Disorde", "Somatoform Pain Disorder", df$condition)
  df$condition <- gsub("atigue", "Fatigue", df$condition)
  df$condition <- gsub("FFatigue", "Fatigue", df$condition)
  df$condition <- gsub("emale Infertility", "Female Infertility", df$condition)
  df$condition <- gsub("Vertig", "Vertigo", df$condition)
  df$condition <- gsub("Trem", "Tremor", df$condition)
  df$condition <- gsub("Premature Lab", "Premature Labor", df$condition)
  df$condition <- gsub("Cance", "Cancer", df$condition)
  df$condition <- gsub("Cancerr", "Cancer", df$condition)
  df$condition <- gsub("Feve", "Fever", df$condition)
  df$condition <- gsub("Feverr", "Fever", df$condition)
  df$condition <- gsub("ibromyalgia", "Fibromyalgia", df$condition)
  
  # make every first letter in every word capital
  df$drugName <- str_to_title(df$drugName)
  df$condition <- str_to_title(df$condition)
  
  # make every first letter in every word capital
  df$drugName <- str_to_title(df$drugName)
  df$condition <- str_to_title(df$condition)
  
  # listing 
  df$drugName <- strsplit(df$drugName, "(?<=[A-Za-z]) / (?=[A-Za-z])", perl = TRUE)
  df$drugName <- sapply(df$drugName, function(x) paste(x, collapse = "/"))
  df$drugName <- gsub(" / ", "/", df$drugName) # for dose
  
  df$condition <- sapply(df$condition, function(x) paste(x, collapse = "/"))
  df$condition <- gsub(" / ", "/", df$condition)
  
  # Save as csv file
  # Unnest the lists in 'drugName' and 'condition' columns before writing to CSV
  df <- unnest(df, cols = c(drugName, condition))
  
  return(df)
  
}


# Function to save cleaned and preprocessed data to CSV
saveDataToCSV <- function(df) {
  
  write.csv(df, file = "Dataset/data.csv", row.names = FALSE)
}



# Function to plot average rating histogram
plotAverageRatingHistogram <- function(df) {
  
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
  
}



# Function to plot word count distribution
plotWordCountDistribution <- function(df) {
  
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
  
}



# Load dataset (.tsv files)
file_path <- "./Dataset/drugsComTest_raw.tsv"
data <- read_tsv(file_path)
df <- as.data.frame(data)

print("Cleaning Data")
# Clean and preprocess data
df <- cleanAndPreprocessData(df)
print("Finish Cleaning Data")

# Save cleaned and preprocessed data to CSV
saveDataToCSV(df)
print("Saved data as CSV")

print("Plotting average rating histogram")
# Plot average rating histogram
plotAverageRatingHistogram(df)

print("Plotting word count distribution")
# Plot word count distribution
plotWordCountDistribution(df)

print("Finish Data Observation Execution")