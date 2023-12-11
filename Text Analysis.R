library(tm)
library(tidytext)
library(dplyr)
library(tidyr)
library(widyr)
library(rstudioapi)
library(R6)
library(qdapTools)
library(ggplot2)
library(igraph)
library(ggraph)

# Text Analysis Class
TextAnalysis <- 
  R6Class("TextAnalysis",
          
          public = list(
            
            # Generic Function 1: Data Cleaning And Pre-processing
            cleanAndPreprocessData = function(fulldata) {
              
              # Getting the path of your current open file
              current_path = rstudioapi::getActiveDocumentContext()$path 
              setwd(dirname(current_path ))
              
              # Load data
              test_df <- fulldata

              # Create Corpus
              print("Creating Corpus")
              corpus <- Corpus(VectorSource(test_df$review))
              
              # Clean data
              print("Cleaning data")
              corpus <- tm_map(corpus, content_transformer(tolower))
              corpus <- tm_map(corpus, removeNumbers)
              corpus <- tm_map(corpus, removePunctuation)
              corpus <- tm_map(corpus, removeWords, stopwords("en"))
              corpus <- tm_map(corpus, stemDocument)

              # Create a document-term matrix
              print("Creating DTM")
              dtm <- DocumentTermMatrix(corpus)
              
              # Create a tidy text data frame
              print("Creating tidytext")
              tidy_data <- tidy(dtm)

              print("Creating data with rating")
              # Tokenize the reviews and include ratings
              review_rating <- fulldata %>%
                unnest_tokens(word, review) %>%
                anti_join(stop_words, by = "word") %>%
                select(word, rating)
              
              # Return the pre-processed data
              return(list(reviews = tidy_data, reviews_rating = review_rating))
            },
            
            
            # Generic Function 2: Word Frequency Analysis
            wordFrequencyAnalysis = function(value) {
              
              # Get word frequency
              word_freq <- value %>%
                group_by(term) %>%
                summarise(total = sum(count)) %>%
                arrange(desc(total))
              
              # Get top 20 words
              top_words <- word_freq %>%
                top_n(20, total)
              
              # Plot word frequency bar-plot
              plot <- ggplot(top_words, aes(x = reorder(term, total), y = total)) +
                geom_bar(stat = "identity", fill = "skyblue") +
                coord_flip() +
                labs(title = "Top 20 Words - Word Frequency Analysis",
                     x = "Word",
                     y = "Total Frequency")
              
              # Save the plot as a PNG file
              ggsave("word_frequency_plot.png", plot)
              
              # View plot
              print(plot)
              
              # Rename columns to 'name' and 'num'
              review_word_freq <- word_freq %>%
                rename(name = term, num = total)
              
              # Return word frequency data frame
              return(review_word_freq)
            },
            
            
            # Generic Function 3: Word Correlation Network
            wordCorrelationAnalysis = function(data) {
              
              # Count number of times words are mentioned in ratings (more than 100 times)
              words_count <- data %>%
                count(word, name = "num_mentions") %>%
                filter(num_mentions > 100)
              
              # Get correlation between words (more than 0.8 correlation)
              word_correlation <- data %>%
                semi_join(words_count, by = "word") %>%
                pairwise_cor(item = word, feature = rating) %>%
                filter(correlation >= 0.8)

              # Create a network from the word_correlation data
              word_graph <- graph_from_data_frame(word_correlation, 
                                                  vertices = words_count %>%
                                                    semi_join(word_correlation, by = c("word" = "item1"))) %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(alpha = correlation)) +
                geom_node_point() +
                geom_node_text(aes(color = num_mentions, label = name), repel = TRUE, max.overlaps = Inf, box.padding = 0.5, point.padding = 0.3, nudge_x = 0.1)
            
              # Save the graph as a PNG file
              ggsave("word_network_graph.png", word_graph)
              
              # View graph
              print(word_graph)

            },
            
            
            # Generic Function 4: Word Cloud
            wordClouds = function(results) {

              # Word cloud

            },
            
            
            # Generic Function 4: Preparation for Sentiment Analysis
            prepareForSentimentAnalysis = function(data) {
              # Implement preparation logic for sentiment analysis here
              # find words that represents positive or negative review for sentiment analysis
            }
          )
  )

# Main Class for loading data from CSV file and calling functions in the TextAnalysis class
TextAnalysisMain <- function(csvFilePath) {
  # Load data from CSV file
  data <- read.csv(csvFilePath)
  
  # Create an instance of the TextAnalysis class
  ta_instance <- TextAnalysis$new()
  
  # Clean and preprocess the data
  print("Start Preprocessing Data")
  cleanedData <- ta_instance$cleanAndPreprocessData(data)
  print("Finish data preprocessing")
  
  # Save cleaned data
  reviews = cleanedData$reviews
  reviews_ratings = cleanedData$reviews_rating

  # Perform word frequency analysis
  print("Start Word Frequency Analysis")
  review_word_freq <- ta_instance$wordFrequencyAnalysis(reviews)
  print("Finish Word Frequency Analysis")
  
  # Perform word correlation analysis
  print("Start Word Correlation Analysis")
  ta_instance$wordCorrelationAnalysis(reviews_ratings)
  print("Finish Word Correlation Analysis")
  
  
  # Create Word Clouds
  ta_instance$wordClouds(review_word_freq)
  
  # Prepare data for sentiment analysis
  sentimentData <- ta_instance$prepareForSentimentAnalysis(cleanedData)
}

# Example usage
csvFilePath <- "./data.csv"
TextAnalysisMain(csvFilePath)
