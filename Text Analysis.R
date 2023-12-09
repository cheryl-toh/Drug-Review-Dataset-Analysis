library(tm)
library(tidytext)
library(rstudioapi)
library(R6)

# Text Analysis Class
TextAnalysis <- 
  R6Class("TextAnalysis",
          
          public = list(
            # Generic Function 1: Data Cleaning And Pre-processing
            cleanAndPreprocessData = function(fulldata) {
              # Getting the path of your current open file
              current_path = rstudioapi::getActiveDocumentContext()$path 
              setwd(dirname(current_path ))
              print( getwd() )
              
              # Reading fullData csv file
              print("Creating corpus")
              review_corpus <- fulldata$review
              review_corpus <- Corpus(VectorSource(review_corpus))
              
              print("Cleaning data")
              # Convert all the text into lower case
              review_corpus <- tm_map(review_corpus, tolower)
              
              # Removing special characters from the text
              review_corpus <- tm_map(review_corpus, removePunctuation)
              
              # Remove numbers from the text data.
              review_corpus <- tm_map(review_corpus, removeNumbers)
              
              # Remove stop words
              review_corpus <- tm_map(review_corpus, removeWords, stopwords('english'))
              
              # Remove URLs
              removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
              review_corpus <- tm_map(review_corpus, content_transformer(removeURL))
              
              # Text stemming – which reduces words to their root form
              review_corpus <- tm_map(review_corpus, stemDocument)
              review_corpus <- tm_map(review_corpus, stripWhitespace)
              
              print("Creating TDM")
              # Convert to Term Document Matrix
              tdm <- TermDocumentMatrix(review_corpus)
              
              # Remove sparse terms from tdm to reduce matrix size
              tdm_sparsed <- removeSparseTerms(tdm, sparse = 0.2)
              tdm_matrix <- as.matrix(tdm_sparsed)
              
              print("Tokenizing data")
              # Word tokenization
              library(tokenizers)
              tokenized_words <- unlist(tokenize_words(as.character(review_corpus)))
              
              tdm_df <- as.data.frame(tdm_matrix)
              
              # Return the preprocessed data
              return(list(review_corpus = review_corpus, tdm_matrix = tdm_matrix))
            },
            
            # Generic Function 2: Exploratory Analysis
            performExploratoryAnalysis = function(data) {
              # Implement exploratory analysis logic here
            },
            
            # Generic Function 3: Visualization
            visualizeResults = function(results) {
              # Implement visualization logic here
            },
            
            # Generic Function 4: Preparation for Sentiment Analysis
            prepareForSentimentAnalysis = function(data) {
              # Implement preparation logic for sentiment analysis here
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
  cleanedData <- ta_instance$cleanAndPreprocessData(data)
  
  # Perform exploratory analysis
  exploratoryResults <- ta_instance$performExploratoryAnalysis(cleanedData)
  
  # Visualize the results
  ta_instance$visualizeResults(exploratoryResults)
  
  # Prepare data for sentiment analysis
  sentimentData <- ta_instance$prepareForSentimentAnalysis(cleanedData)
}

# Example usage
csvFilePath <- "./data.csv"
TextAnalysisMain(csvFilePath)
