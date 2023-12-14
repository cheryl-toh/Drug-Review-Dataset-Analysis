library(R6)
library(dplyr)
library(textutils)
library(stringr)
library(ggplot2)
library(SentimentAnalysis)
library(ggplot2)
library(usethis) 
usethis::edit_r_environ() # must set R_MAX_VSIZE=100Gb

# Sentiment Analysis Class
SentimentAnalysis <- 
  
  R6Class("SentimentAnalysis",
          
          public = list( 
            
            # Generic Function 1: Data Pre-processing
            preprocessData = function(data) {
              
              # creating new dataset for sentiment
              senti_data <- data
              
              # data cleaning
              senti_data$review <- HTMLdecode(senti_data$review) #1-2 mins
              
              senti_data$condition <- ifelse(grepl("</span> users found this comment helpful", senti_data$condition),
                                             NA, senti_data$condition)
              
              # Cleaning (removing incomplete drugname in conditions) and inputing conditions based on the review and what the medication does
              # min) into Diabetes, Type 2 -> (All under Metformin, or Dapagliflozin, Empagliflozin, Glyburide and Sitagliptin which is taken for Type 2 Diabetes)
              senti_data$drugName <- gsub("Metformin / sitagliptin", "Janumet", senti_data$drugName)
              senti_data$drugName <- gsub("Metformin / saxagliptin", "Onglyza", senti_data$drugName)
              senti_data$drugName <- gsub("Saxagliptin", "Onglyza", senti_data$drugName)
              senti_data$drugName <- gsub("Linagliptin", "Tradjenta", senti_data$drugName)
              senti_data$drugName <- gsub("Empagliflozin / metformin", "Synjardy", senti_data$drugName)
              
              senti_data$condition <- gsub("min)", "Diabetes, Type 2", senti_data$condition)
              senti_data$condition <- gsub("min / sitagliptin)", "Diabetes, Type 2", senti_data$condition)
              senti_data$condition <- gsub("min / saxagliptin)", "Diabetes, Type 2", senti_data$condition)
              
              # Dulera and Asthma <- Formoterol / mometasone is sold under the brand name Dulera
              senti_data$drugName <- gsub("Formoterol / mometasone", "Dulera", senti_data$drugName)
              senti_data$condition <- gsub("moterol / mometasone)", "Asthma", senti_data$condition)
              
              # COPD
              senti_data$drugName <- gsub("Budesonide / formoterol", "Symbicort", senti_data$drugName)
              senti_data$condition <- gsub("mist (", "COPD", senti_data$condition, fixed=TRUE)
              senti_data$condition <- gsub("moterol)", "Asthma / COPD", senti_data$condition) # symbicort for asthma or COPD
              
              # Mycophenolic acid
              senti_data$condition <- gsub("tic (mycophenolic acid)", "Organ Transplant, Rejection Prophylaxis", senti_data$condition, fixed=TRUE)
              
              # Exforge
              senti_data$drugName <- gsub("Amlodipine / valsartan", "Exforge", senti_data$drugName)
              senti_data$drugName <- gsub("Exforge HCT", "Exforge", senti_data$drugName)
              senti_data$condition <- gsub("ge (amlodipine / valsartan)", "High Blood Pressure", senti_data$condition, fixed=TRUE)
              
              # Pramoxine / zinc oxide
              senti_data$condition <- gsub("Hemorrhoids (pramoxine / zinc oxide)", "Hemorrhoids", senti_data$condition, fixed=TRUE)
              
              # nasal congestion
              senti_data$condition <- gsub("mulation) (phenylephrine)", "Nasal Congestion", senti_data$condition, fixed=TRUE)
              
              # Oxybutynin
              senti_data$condition <- gsub("Women (oxybutynin)", "Urinary Incontinence / Prostatitis / Overactive Bladder / Hyperhidrosis", senti_data$condition, fixed=TRUE)
              
              
              # uncompleted words
              senti_data$condition <- gsub("zen Shoulde", "Frozen Shoulder", senti_data$condition)
              senti_data$condition <- gsub("Not Listed / Othe", "Not Listed or Other", senti_data$condition)
              senti_data$condition <- gsub("Overactive Bladde", "Overactive Bladder", senti_data$condition)
              senti_data$condition <- gsub("Disorde", "Disorder", senti_data$condition)
              senti_data$condition <- gsub("Disorderr", "Disorder", senti_data$condition)
              senti_data$condition <- gsub("Ulce", "Ulcer", senti_data$condition)
              senti_data$condition <- gsub("Ulcerr", "Ulcer", senti_data$condition)
              senti_data$condition <- gsub("Uveitis, Posteri", "Uveitis, Posterior", senti_data$condition)
              senti_data$condition <- gsub("cal Segmental Glomerulosclerosis", "Focal Segmental Glomerulosclerosis", senti_data$condition)
              senti_data$condition <- gsub("Somat", "Somatic Symptom Disorder", senti_data$condition)
              senti_data$condition <- gsub("m Pain Disorde", "Somatoform Pain Disorder", senti_data$condition)
              senti_data$condition <- gsub("atigue", "Fatigue", senti_data$condition)
              senti_data$condition <- gsub("FFatigue", "Fatigue", senti_data$condition)
              senti_data$condition <- gsub("emale Infertility", "Female Infertility", senti_data$condition)
              senti_data$condition <- gsub("Vertig", "Vertigo", senti_data$condition)
              senti_data$condition <- gsub("Trem", "Tremor", senti_data$condition)
              senti_data$condition <- gsub("Premature Lab", "Premature Labor", senti_data$condition)
              senti_data$condition <- gsub("Cance", "Cancer", senti_data$condition)
              senti_data$condition <- gsub("Cancerr", "Cancer", senti_data$condition)
              senti_data$condition <- gsub("Feve", "Fever", senti_data$condition)
              senti_data$condition <- gsub("Feverr", "Fever", senti_data$condition)
              senti_data$condition <- gsub("ibromyalgia", "Fibromyalgia", senti_data$condition)
              
              # make every first letter in every word capital
              senti_data$drugName <- str_to_title(senti_data$drugName)
              senti_data$condition <- str_to_title(senti_data$condition)
              
              
              # listing 
              senti_data$drugName <- strsplit(senti_data$drugName, "(?<=[A-Za-z]) / (?=[A-Za-z])", perl = TRUE)
              senti_data$drugName <- gsub(" / ", "/", senti_data$drugName) # for dose
              
              senti_data$condition <- gsub(" / ", "/", senti_data$condition)
              senti_data$condition <- strsplit(senti_data$condition, "/")
              
              # setting date variable
              senti_data$date <- as.Date(senti_data$date, format = "%B %d, %Y")
              
              return(senti_data)
            },
            
            
            # Generic Function 2: Sentiment Analysis
            performSentimentAnalysis = function(data) {
              
              # plotting sentiment score against ratings for correlation
                senti_score <- senti_data$review
                senti_rate <- senti_data$rating
                
                sentiment <- analyzeSentiment(senti_score) # around 3-4 mins
                senti_data$score <- sentiment$SentimentQDAP
                senti_data$score_result <- convertToDirection(sentiment$SentimentQDAP)
                
                evaluation <- data.frame(compareToResponse(sentiment, senti_rate)) 
                senti_corr <- plotSentimentResponse(sentiment$SentimentQDAP, senti_rate)
                ggsave("senti_corr.png", plot = senti_corr, width = 11, height = 8)
              
              # plotting sentiment scores distribution with conditions
                top_conditions <- senti_data %>% group_by(condition) %>% summarise(count = n()) %>%
                  arrange(desc(count)) %>% head(10)
                
                senti_top10 <- senti_data %>% filter(condition %in% top_conditions$condition)
                senti_top10 <- senti_top10 %>% mutate(rounded_score = ifelse(score_result == "positive", ceiling(score),
                                                                      ifelse(score_result == "negative", floor(score),
                                                                      ifelse(score_result == "neutral", 0, NA))))
                
                senti_histo <-  ggplot(senti_top10, aes(x = score, fill = condition)) +
                  geom_histogram(binwidth = 0.1, position = "dodge") +
                  ggtitle("Sentiment Score Distribution by Medical Condition") +
                  xlab("Sentiment Score") +
                  ylab("Frequency") +
                  facet_wrap(~condition, scales = "free_y")
                
                senti_box <-  ggplot(senti_top10, aes(x = condition, y = score)) +
                  geom_boxplot() +
                  ggtitle("Boxplot of Sentiment Scores by Medical Condition") +
                  xlab("Condition") +
                  ylab("Sentiment Score") +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1))
                
                ggsave("senti_histogram.png", plot = senti_histo, width = 11, height = 8)
                ggsave("senti_boxplot.png", plot = senti_box, width = 11, height = 8)
              
            },
            
            # Generic Function 3: Visualization
            visualizeResults = function(results) {
              # Implement visualization logic here
            },
            
            # Generic Function 4: Evaluation
            evaluateResults = function(results) {
              # Implement evaluation logic here
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
  
  print(head(preprocessedData))
  
  # Perform sentiment analysis
  sentimentResults <- sa_instance$performSentimentAnalysis(preprocessedData)
  
  # Visualize the results
  sa_instance$visualizeResults(sentimentResults)
  
  # Evaluate the results
  sa_instance$evaluateResults(sentimentResults)
}

# Example usage
csvFilePath <- "./data.csv"
SentimentAnalysisMain(csvFilePath)
