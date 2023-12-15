# Install packages if not already installed
if (!requireNamespace("R6", quietly = TRUE)) {
  install.packages("R6")
}

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

if (!requireNamespace("grid", quietly = TRUE)) {
  install.packages("grid")
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Load Packages
library(R6)
library(ggplot2)
library(gridExtra)
library(grid)



# Descriptive Analysis Class
DescriptiveAnalysis <- 
  R6Class("DescriptiveAnalysis",
          
          public = list(
            
            
            # Generic Function 1: Basic Statistics
            basicStatistics = function(sentiment_data) {
              print("Calculating Basic Statistics")
              
              # Compute basic statistics on sentiment scores
              basic_stats <- summary(sentiment_data$score)
              
              # Save the summary statistics as a data frame
              basic_stats_df <- data.frame(Statistic = names(basic_stats),
                                           Value = unclass(basic_stats),
                                           row.names = NULL)

              # Print basic statistics
              print(basic_stats_df)
              
              # Create a table grob from basic statistics
              table_grob <- gridExtra::tableGrob(basic_stats_df)
              
              # Save the table as a PNG file
              png("Images/basic_statistics_table.png", width = 300, height = 300)
              grid.draw(table_grob)
              dev.off()
            },
            
            
            # Generic Function 2: Distribution Analysis
            distributionAnalysis = function(sentiment_data) {
              print("Performing Distribution Analysis")
              
              # Create a histogram of sentiment scores
              hist_plot <- ggplot(sentiment_data, aes(x = score)) +
                geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
                ggtitle("Distribution of Sentiment Scores") +
                xlab("Sentiment Score") +
                ylab("Frequency")
              
              # Save the histogram plot as a PNG file
              ggsave("Images/sentiment_distribution.png", plot = hist_plot, width = 11, height = 8)
              
              # View the histogram plot
              print(hist_plot)
            },
            
            
            # Generic Function 3: Sentiment Score by Review Length (Word Count)
            sentimentByReviewLength = function(sentiment_data, sampling_ratio = 0.2) {
              print("Analyzing Sentiment Score by Review Length")
              
              ## UNCOMMENT FOR PLOT WITHOUT RANDOM SAMPLING
              # # Create a new column for word count
              # sentiment_data$word_count <- sapply(strsplit(sentiment_data$review, "\\s+"), length)
              # 
              # # Create a scatter plot of sentiment scores by word count
              # scatter_plot <- ggplot(sentiment_data, aes(x = word_count, y = score)) +
              #   geom_point(alpha = 0.5, color = "darkgreen") +
              #   ggtitle("Sentiment Score by Review Length (Word Count)") +
              #   xlab("Review Word Count") +
              #   ylab("Sentiment Score")
              # 
              # # Print and save the scatter plot as a PNG file
              # print(scatter_plot)
              # ggsave("Images/sentiment_by_review_length.png", plot = scatter_plot, width = 11, height = 8)
              
              ## UNCOMMENT FOR PLOT WITH RANDOM SAMPLING
              # Create a new column for word count
              sentiment_data$word_count <- sapply(strsplit(sentiment_data$review, "\\s+"), length)

              # Sample a subset of data points
              sampled_data <- sentiment_data[sample(1:nrow(sentiment_data), size = round(sampling_ratio * nrow(sentiment_data))), ]

              # Create a scatter plot of sentiment scores by word count
              scatter_plot <- ggplot(sampled_data, aes(x = word_count, y = score)) +
                geom_point(alpha = 0.5, color = "darkgreen") +
                ggtitle("Sentiment Score by Review Length (Word Count)") +
                xlab("Review Word Count") +
                ylab("Sentiment Score")

              # Print and save the scatter plot as a PNG file
              print(scatter_plot)
              ggsave("Images/sentiment_by_word_count_scatter.png", plot = scatter_plot, width = 11, height = 8)
            }
            
          )
  )



# Main Class for Descriptive Analysis
DescriptiveAnalysisMain <- function(sentiment_data) {
  
  # Create an instance of the DescriptiveAnalysis class
  da_instance <- DescriptiveAnalysis$new()
  
  # Perform basic statistics analysis
  da_instance$basicStatistics(sentiment_data)
  
  # Perform distribution analysis
  da_instance$distributionAnalysis(sentiment_data)

  # Perform sentiment score by review length analysis
  da_instance$sentimentByReviewLength(sentiment_data)
  
}

# Usage
sentiment_data <- read.csv("Dataset/sentiment_scores.csv")  # Assuming you have sentiment scores data
DescriptiveAnalysisMain(sentiment_data)
