# Install packages if not already installed
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}

if (!requireNamespace("textutils", quietly = TRUE)) {
  install.packages("textutils")
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

if (!requireNamespace("SentimentAnalysis", quietly = TRUE)) {
  install.packages("SentimentAnalysis")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

# Load packages
library(R6)
library(dplyr)
library(textutils)
library(stringr)
library(ggplot2)
library(SentimentAnalysis)
library(data.table)



# Sentiment Analysis Class
SentimentAnalysis <- 
  
  R6Class("SentimentAnalysis",
          
          public = list( 
            
            
            # Generic Function 1: Data Pre-processing
            preprocessData = function(data, sample_size = 10000) {
              
              print("Start preprocessing")
              
              # Take a random sample of the data
              sampled_data <- data[sample(1:nrow(data), sample_size), ]
              
              # Convert data to data.table for efficient operations
              senti_data <- as.data.table(sampled_data)
              
              # Only keep necessary columns
              senti_data <- senti_data[, .(review, rating, condition, drugName)]
              
              
              print("end preprocessing")
              
              return(senti_data)
            },
            
            
            # Generic Function 2: Sentiment Analysis
            performSentimentAnalysis = function(senti_data) {
              
              print("Start Sentiment Analysis")

              # Subset data to include only necessary columns
              subset_data <- senti_data[, c("review", "rating", "condition")]
              
              # Explicitly remove the original dataset from memory
              rm(senti_data)
              
              # plotting sentiment score against ratings for correlation
              senti_score <- subset_data$review
              senti_rate <- subset_data$rating
              
              sentiment <- analyzeSentiment(senti_score) # around 3-4 mins
              
              # Explicitly remove unnecessary variables to free up memory
              rm(senti_score)
              
              subset_data$score <- sentiment$SentimentQDAP
              subset_data$score_result <- convertToDirection(sentiment$SentimentQDAP)
              
              evaluation <- data.frame(compareToResponse(sentiment, senti_rate)) 
              
              # Convert the 'condition' column from list to character
              subset_data$condition <- sapply(subset_data$condition, function(x) paste(x, collapse = "/"))
              
              # Save sentiment scores to CSV for descriptive analysis
              write.csv(subset_data[, c("condition", "review", "rating", "score", "score_result")], "Dataset/sentiment_scores.csv", row.names = FALSE)
              
              # Explicitly free up memory
              gc()
              
              return(list(subset_data = subset_data, evaluation = evaluation))
            },
            
            
            # Generic Function 3: Visualization
            visualizeResults = function(sentiment_data) {
              
              print("Plotting Sentiment-Ratiing Correlation")
              
              # Plot sentiment - rating correlation
              senti_corr <- plotSentimentResponse(sentiment_data$score, sentiment_data$rating)
              ggsave("Images/senti_corr.png", plot = senti_corr, width = 11, height = 8)
              
              print(senti_corr)
              
              print("Plotting Sentiment Score by Medical Condition Histogram")
              
              # plotting sentiment scores distribution with conditions
              top_conditions <- sentiment_data %>% group_by(condition) %>% summarise(count = n()) %>%
                arrange(desc(count)) %>% head(10)
              
              senti_top10 <- sentiment_data %>%
                filter(condition %in% top_conditions$condition) %>%
                mutate(score_result = factor(score_result, levels = c("positive", "negative", "neutral")),
                       rounded_score = ifelse(score_result == "positive", ceiling(score),
                                              ifelse(score_result == "negative", floor(score),
                                                     ifelse(score_result == "neutral", 0, NA))))
              
              # Reorder levels of 'condition' based on the median of 'score'
              condition_order <- senti_top10 %>%
                group_by(condition) %>%
                summarise(median_score = median(score, na.rm = TRUE)) %>%
                arrange(median_score) %>%
                pull(condition)
              
              senti_top10$condition <- factor(senti_top10$condition, levels = condition_order)
              
              # Plot histogram
              senti_histo <- ggplot(senti_top10, aes(x = score, fill = condition)) +
                geom_histogram(binwidth = 0.1, position = "dodge") +
                ggtitle("Sentiment Score Distribution by Medical Condition") +
                xlab("Sentiment Score") +
                ylab("Frequency") +
                facet_wrap(~condition, scales = "free_y")
              
              print(senti_histo)
              
              print("Plotting Sentiment Score by Medical Condition Boxplot")
              
              # Plot boxplot
              senti_box <- ggplot(senti_top10, aes(x = condition, y = score)) +
                geom_boxplot() +
                ggtitle("Boxplot of Sentiment Scores by Medical Condition") +
                xlab("Condition") +
                ylab("Sentiment Score") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
              
              print(senti_box)
              
              ggsave("Images/senti_histogram.png", plot = senti_histo, width = 11, height = 8)
              ggsave("Images/senti_boxplot.png", plot = senti_box, width = 11, height = 8)
            },
            
            
            # Generic Function 4: Evaluation
            evaluateResults = function(results) {
              
              # Print the results
              print("Evaluation Results:")
              print(results)
            }
          )
  )



# Main Class for loading data from CSV file and calling functions in the SentimentAnalysis class
SentimentAnalysisMain <- function(csvFilePath) {
  
  # Load data from CSV file
  data <- read.csv(csvFilePath)
  
  # Create an instance of the SentimentAnalysis class
  sa_instance <- SentimentAnalysis$new()
  
  # Perform data pre-processing
  preprocessedData <- sa_instance$preprocessData(data)
  
  # Perform sentiment analysis
  sentimentResults <- sa_instance$performSentimentAnalysis(preprocessedData)
  
  sentiment = sentimentResults$subset_data
  evaluation = sentimentResults$evaluation
  
  # Graph plotting
  sa_instance$visualizeResults(sentiment)
  
  # Evaluate the results
  sa_instance$evaluateResults(evaluation)
}

# Usage
csvFilePath <- "Dataset/data.csv"
SentimentAnalysisMain(csvFilePath)
