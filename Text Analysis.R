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
library(wordcloud)


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
              ggsave("Images/word_frequency_plot.png", plot)
              
              # View plot
              print(plot)
              
              # Rename columns to 'term' and 'total'
              review_word_freq <- word_freq %>%
                rename(term = term, num = total)
              
              
              # Return word frequency data frame
              return(review_word_freq)
            },
            
            
            # Generic Function 3: Word Correlation Network
            wordCorrelationAnalysis = function(data) {
              
              # Count number of times words are mentioned in ratings (more than 100 times)
              words_count <- data %>%
                count(word, name = "num_mentions") %>%
                filter(num_mentions >= 200)
              
              # Get correlation between words (more than 0.8 correlation)
              word_correlation <- data %>%
                semi_join(words_count, by = "word") %>%
                pairwise_cor(item = word, feature = rating) %>%
                filter(correlation >= 0.5)

              # Create a network from the word_correlation data
              word_graph <- graph_from_data_frame(word_correlation, 
                                                  vertices = words_count %>%
                                                    semi_join(word_correlation, by = c("word" = "item1"))) %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(alpha = correlation)) +
                geom_node_point() +
                geom_node_text(aes(color = num_mentions, label = name), repel = TRUE)
            
              # Save the graph as a PNG file
              ggsave("Images/word_network_graph.png", word_graph)
              
              # View graph
              print(word_graph)

            },
            
            
            #  Generic Function 4: Word Cloud
            wordClouds = function(review_word_freq) {
              
              print("Creating word cloud")
              # Open a PNG device for saving the plot
              png("Images/word_cloud.png", width = 800, height = 600)
              
              # Word cloud for reviews
              review_cloud <- wordcloud(review_word_freq$term, review_word_freq$num,
                                        max.words = 200, random.order=FALSE, 
                                        colors = brewer.pal(8, "Dark2"))
              
              # View word cloud
              print(review_cloud)
              
              # Close the PNG device
              dev.off()
            },
            
            # Generic Function 5: Preparation for Sentiment Analysis
            prepareForSentimentAnalysis = function(data) {
              
              print("Plotting positive words network")
              # Filter data for positive ratings (rating > 5) and negative ratings (rating <= 5)
              positive_data <- data %>%
                filter(rating > 5)
              
              negative_data <- data %>%
                filter(rating <= 5)
              
              # Count number of times words are mentioned in positive ratings
              positive_counts <- positive_data %>%
                count(word, name = "positive_mentions") %>%
                filter(positive_mentions >= 100)  # Adjust the threshold as needed
              
              # Get correlation between words in positive ratings (more than 0.5 correlation)
              positive_correlation <- positive_data %>%
                semi_join(positive_counts, by = "word") %>%
                pairwise_cor(item = word, feature = rating) %>%
                filter(correlation >= 0.8)
              
              # Create a network from the positive_correlation data
              positive_word_graph <- graph_from_data_frame(positive_correlation, 
                                                           vertices = positive_counts %>%
                                                             semi_join(positive_correlation, by = c("word" = "item1"))) %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(alpha = correlation)) +
                geom_node_point() +
                geom_node_text(aes(color = positive_mentions, label = name), repel = TRUE)
              
              # Save the positive word correlation graph as a PNG file
              ggsave("Images/positive_word_network_graph.png", positive_word_graph)
              
              # View positive word correlation graph
              print(positive_word_graph)
              
              
              print("Plotting negative word network")
              # Count number of times words are mentioned in negative ratings
              negative_counts <- negative_data %>%
                count(word, name = "negative_mentions") %>%
                filter(negative_mentions >= 50)  # Adjust the threshold as needed
              
              # Get correlation between words in negative ratings (more than 0.5 correlation)
              negative_correlation <- negative_data %>%
                semi_join(negative_counts, by = "word") %>%
                pairwise_cor(item = word, feature = rating) %>%
                filter(correlation >= 0.8)
              
              # Create a network from the negative_correlation data
              negative_word_graph <- graph_from_data_frame(negative_correlation, 
                                                           vertices = negative_counts %>%
                                                             semi_join(negative_correlation, by = c("word" = "item1"))) %>%
                ggraph(layout = "fr") +
                geom_edge_link(aes(alpha = correlation)) +
                geom_node_point() +
                geom_node_text(aes(color = negative_mentions, label = name), repel = TRUE)
              
              # Save the negative word correlation graph as a PNG file
              ggsave("Images/negative_word_network_graph.png", negative_word_graph)
              
              # View negative word correlation graph
              print(negative_word_graph)
              
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
# 
#   # Prepare data for sentiment analysis
#   ta_instance$prepareForSentimentAnalysis(reviews_ratings)
}

# Example usage
csvFilePath <- "./data.csv"
TextAnalysisMain(csvFilePath)
