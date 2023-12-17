# Drug-Review-Dataset-Analysis

This project aims to analyze a dataset containing reviews and ratings of various medications. The analysis is divided into three main parts: Text Analysis, Sentiment Analysis, and Descriptive Analysis. Each part is implemented as a separate script to ensure modularity and clarity in the codebase.


## Code Organization

The code follows an object-oriented programming (OOP) paradigm for better code organization and scalability. Each analysis is implemented within a class, and there's a main driver class to orchestrate the entire process.


## Project Structure

The project is structured into the following scripts:

1. **Data_Observation.R**

   - *Purpose:* Observes the raw dataset, performs data cleaning procedures, and saves the cleaned data as a CSV file.
   
   - *How to Run:* Execute this script first.

2. **Text_Analysis.R**

   - *Purpose:* Analyzes the cleaned dataset using natural language processing techniques. It includes word frequency analysis, word correlation network creation, and word cloud generation.
   
   - *How to Run:* Execute after the Data_Observation script.

3. **Sentiment_Analysis.R**

   - *Purpose:* Performs sentiment analysis on the dataset, creating sentiment score histograms and word correlation networks for positive and negative sentiments.
   
   - *How to Run:* Execute after the Text_Analysis script.

4. **Descriptive_Analysis.R**

   - *Purpose:* Conducts descriptive analysis, including basic statistics, distribution analysis, and sentiment score analysis by review length.
   
   - *How to Run:* Execute after the Sentiment_Analysis script.

5. **Images Folder**
   - *Purpose:* Encapsulate all images (plots) generated

6. **Dataset Folder**
   - *Purpose:* Encapsulate all TSV and CSV Files