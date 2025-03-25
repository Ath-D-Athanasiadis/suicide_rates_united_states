---
  title: "Suicide Rates Data Cleaning and Processing"
author: "Thanos
date: "March 25, 2025"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

### Load Required Libraries

{r load-libraries, message=FALSE}

library(tidyverse)  # For data manipulation and visualization
library(janitor)    # For cleaning data
library(skimr)      # For summary statistics

### Load Data

# Load the dataset

data <- read_csv("suicide_rates.csv")

# Display the first few rows
head(data)

### Initial Data Exploration

# Summary of the dataset
skim(data)

# Check the structure of the dataset
str(data)

### heck for Duplicates

# Check for duplicate rows
duplicates <- data[duplicated(data), ]

cat("Number of duplicate rows:", nrow(duplicates), "\n")

# Remove duplicates if they exist
if (nrow(duplicates) > 0) {
  data <- data[!duplicated(data), ]
  cat("Duplicates removed. New dataset size:", nrow(data), "\n")
} else {
  cat("No duplicates found.\n")
}

### Check for Blanks and Missing Data

# Check for NA values in each column
colSums(is.na(data))

# Check for empty strings (blanks) in character columns
char_cols <- sapply(data, is.character)
data_char <- data[, char_cols]
blanks <- sapply(data_char, function(x) sum(x == "", na.rm = TRUE))
print("Blanks in character columns:")
print(blanks)

# Check for blanks in the ESTIMATE column (numeric but may have been read as character due to blanks)
# Convert ESTIMATE to numeric, coercing blanks to NA
data$ESTIMATE <- as.numeric(data$ESTIMATE)
cat("Number of NAs in ESTIMATE after converting blanks to NA:", sum(is.na(data$ESTIMATE)), "\n")

### Fix Errors

# Identify rows with the same STUB_LABEL, YEAR, and other identifiers but different ESTIMATE values
data <- data %>%
  group_by(STUB_LABEL, YEAR, SEX = if_else(is.na(STUB_NAME), "Total", STUB_NAME)) %>%
  mutate(
    ESTIMATE = if_else(
      n() > 1 & !is.na(ESTIMATE),
      mean(ESTIMATE, na.rm = TRUE),
      ESTIMATE
    )
  ) %>%
  distinct(STUB_LABEL, YEAR, SEX, .keep_all = TRUE) %>%
  ungroup()

# Check the dataset after fixing duplicates
cat("Dataset size after fixing inconsistent estimates:", nrow(data), "\n")

### Handle Missing Data

# Calculate mean ESTIMATE for each STUB_LABEL group
group_means <- data %>%
  group_by(STUB_LABEL) %>%
  summarise(mean_estimate = mean(ESTIMATE, na.rm = TRUE)) %>%
  ungroup()

# Overall mean for cases where group mean is NA
overall_mean <- mean(data$ESTIMATE, na.rm = TRUE)

# Impute missing ESTIMATE values
data <- data %>%
  left_join(group_means, by = "STUB_LABEL") %>%
  mutate(
    ESTIMATE = if_else(
      is.na(ESTIMATE),
      if_else(
        is.na(mean_estimate),
        overall_mean,
        mean_estimate
      ),
      ESTIMATE
    )
  ) %>%
  select(-mean_estimate)

# Check for remaining NAs in ESTIMATE
cat("Remaining NAs in ESTIMATE after imputation:", sum(is.na(data$ESTIMATE)), "\n")

### Final Data Check

# Summary of the cleaned dataset
skim(data)

# Check for any remaining NAs
colSums(is.na(data))

### Create a CSV File

# Save the cleaned dataset to a CSV file
write_csv(data, "cleaned_suicide_rates.csv")
cat("Cleaned dataset saved as 'cleaned_suicide_rates.csv'.\n")

