# Create Text Analysis Class


# Generic Function 1: Data Cleaning And Pre-processing

  # Case sensitivity
  # punctuation
  # handle null value appropriately
  # tokenization (Breakdown text into individual words)
  # stop word removal (remove words that don't contribute much)

current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

# Reading fullData csv file
fulldata <- read.csv("fullData.csv", header = T)

# Build corpus using tm package
library(tm)
review_corpus <- fulldata$review
review_corpus <- Corpus(VectorSource(review_corpus))
inspect(review_corpus[1:5])

# convert all the text into lower case
review_corpus <- tm_map(review_corpus, tolower)

# removing special characters from the text
review_corpus <- tm_map(review_corpus, removePunctuation)

# remove numbers from the text data.
review_corpus <- tm_map(review_corpus, removeNumbers)

# remove stop words
review_corpus <- tm_map(review_corpus, removeWords, stopwords('english'))

# remove URLs
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
review_corpus <- tm_map(review_corpus, content_transformer(removeURL))

# Text stemming – which reduces words to their root form
review_corpus <- tm_map(review_corpus, stemDocument)
review_corpus <- tm_map(review_corpus, stripWhitespace)
inspect(review_corpus[1:5])

# convert to Term Document Matrix
tdm <- TermDocumentMatrix(review_corpus)

# remove sparse terms from tdm to reduce matrix size
tdm_sparsed <- removeSparseTerms(tdm, sparse = 0.2)
tdm_matrix <- as.matrix(tdm_sparsed) 
print(tdm[1:10, 1:20])

# word tokenization (too large, doesn't work) 
library(tokenizers)
tokenize_words(review_corpus)



# Generic Function 2: Exploratory Analysis 

  # Word Frequency Analysis (Identify the most frequent words in the dataset)
  # N-gram Analysis (Look at word pairs or triplets to capture context)


# Generic Function 3: Visualization

  # Word cloud
  # Word network plot (plot a network of word to view relationship between words)


# Generic Function 4: Preparation for sentiment analysis 

  # find words that represents positive or negative review for sentiment analysis



# Main Class for loading data from csv file and calling functions in class
