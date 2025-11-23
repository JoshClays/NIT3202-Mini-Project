### Install and Load Libraries

# Install packages
install.packages(c("caret", "e1071", "randomForest", "dplyr"))

# Load libraries
library(reshape2)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(dplyr)

### Load and Inspect Data
data <- read.csv("malicious_and_benign_websites1.csv", stringsAsFactors = TRUE)
str(data)
summary(data)

### Preprocess Data
# Check missing values
colSums(is.na(data))

# Remove rows with NAs
data <- na.omit(data)

###Cleaning High Cardinality Columns

# Convert to lowercase character
data$SERVER <- tolower(as.character(data$SERVER))
# Extract basic server Types
data$SERVER_Type <- ifelse(grepl("apache", data$SERVER), "Apache",
                           ifelse(grepl("nginx", data$SERVER), "Nginx",
                                  ifelse(grepl("microsoft|iis", data$SERVER), "IIS", "Other")))
data$SERVER_Type <- as.factor(data$SERVER_Type)

# Group top 5 countries, rest as "Other"
# Convert factor to character before processing
data$WHOIS_COUNTRY <- as.character(data$WHOIS_COUNTRY)
top_countries <- names(sort(table(data$WHOIS_COUNTRY), decreasing = TRUE))[1:5]
data$WHOIS_COUNTRY_CLEAN <- ifelse(data$WHOIS_COUNTRY %in% top_countries,
                                   data$WHOIS_COUNTRY, "Other")
data$WHOIS_COUNTRY_CLEAN <- as.factor(data$WHOIS_COUNTRY_CLEAN)

# Similar logic as WHOIS_COUNTRY
data$WHOIS_STATEPRO <- as.character(data$WHOIS_STATEPRO)
top_states <- names(sort(table(data$WHOIS_STATEPRO), decreasing = TRUE))[1:5]
data$WHOIS_STATE_CLEAN <- ifelse(data$WHOIS_STATEPRO %in% top_states,
                                 data$WHOIS_STATEPRO, "Other")
data$WHOIS_STATE_CLEAN <- as.factor(data$WHOIS_STATE_CLEAN)

# Convert to date (adjust format)
data$WHOIS_REGDATE <- as.Date(data$WHOIS_REGDATE, format="%d/%m/%Y")
# Calculate domain age in years
data$DOMAIN_AGE <- as.numeric(difftime(Sys.Date(), data$WHOIS_REGDATE, units = "days")) / 365
# Handle missing or invalid dates (optional)
data$DOMAIN_AGE[is.na(data$DOMAIN_AGE)] <- median(data$DOMAIN_AGE, na.rm = TRUE)

# Convert to date
data$WHOIS_UPDATED_DATE <- as.Date(data$WHOIS_UPDATED_DATE, format="%d/%m/%Y")
# Calculate how many years ago the domain was updated
data$LAST_UPDATED_YEARS_AGO <- as.numeric(difftime(Sys.Date(), data$WHOIS_UPDATED_DATE, units = "days")) / 365
data$LAST_UPDATED_YEARS_AGO[is.na(data$LAST_UPDATED_YEARS_AGO)] <- median(data$LAST_UPDATED_YEARS_AGO, na.rm = TRUE)

# Convert target label into categorical features
data$Type <- as.factor(data$Type)

#Drop Raw High-Cardinality Columns
data <- data[, !(names(data) %in% c("URL","SERVER", "WHOIS_COUNTRY", "WHOIS_STATEPRO", 
                                    "WHOIS_REGDATE", "WHOIS_UPDATED_DATE"))]

# #Splitting the dataset (70% training and 30% testing)
set.seed(123)
splitIndex <- createDataPartition(data$Type, p = 0.7, list = FALSE)
trainData <- data[splitIndex, ]
testData <- data[-splitIndex, ]

### Training Machine Learning Model Using 3 Algorithms 
#Train Random Forest
rf_model <- randomForest(Type ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
# Evaluate
confusionMatrix(rf_pred, as.factor(testData$Type))

#Train Logistic Regression
log_model <- glm(Type ~ ., data = trainData, family = binomial)
log_pred <- ifelse(predict(log_model, testData, Type = "response") > 0.5, 1, 0)
confusionMatrix(as.factor(log_pred), as.factor(testData$Type))

#Train SVM (Support Vector Machine)
svm_model <- svm(Type ~ ., data = trainData)
svm_pred <- predict(svm_model, testData)
confusionMatrix(svm_pred, as.factor(testData$Type))

###Performance evaluation and comparison
  # Function to extract evaluation metrics
  get_metrics <- function(pred, actual) {
    cm <- confusionMatrix(pred, actual)
    precision <- cm$byClass["Precision"]
    recall <- cm$byClass["Recall"]
    f1 <- 2 * (precision * recall) / (precision + recall)
    accuracy <- cm$overall["Accuracy"]
    return(c(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1))
  }
  # Ensure predictions are factors
  rf_pred <- as.factor(rf_pred)
  log_pred <- as.factor(log_pred)
  svm_pred <- as.factor(svm_pred)
  
  # Get performance metrics
  rf_metrics <- get_metrics(rf_pred, testData$Type)
  log_metrics <- get_metrics(as.factor(log_pred), testData$Type)
  svm_metrics <- get_metrics(svm_pred, testData$Type)
  
  # Combine into data frame
  performance_df <- rbind(
    "Random Forest" = rf_metrics,
    "Logistic Regression" = log_metrics,
    "SVM" = svm_metrics
  )
  
  # Convert to data frame and add classifier column
  performance_df <- as.data.frame(performance_df)
  colnames(performance_df) <- c("Accuracy", "Precision", "Recall", "F1_Score")
  performance_df$Classifier <- rownames(performance_df)
  rownames(performance_df) <- NULL
  
  # View the result
  print(performance_df)
  
  # creating the plot
  ggplot(long_perf, aes(x = Classifier, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
    geom_text(aes(label = round(value, 3)),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3, color = "black") +
    labs(title = "Classifier Performance Comparison",
         y = "Score", x = "Classifier", fill = "Metric") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      legend.position = "top"
    )
  
  
  






