library(R6)
library(dplyr)
library(textutils)
library(stringr)

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
              # Implement sentiment analysis logic here
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
