library(bibliometrix)
library(bibliometrixData)
library(tidyverse)

#indicate file source
file <- "SDG_TI_TI_2024_350.csv"

#convert scopus file to data.frame
M <- convert2df(file = file, dbsource = "scopus", format = "csv")
M$CR

# Load necessary library
library(stringr)

# Load the data frame M from the provided file (assuming M is available in the workspace)
# Example: load("M.RData")

# Load necessary library
library(stringr)
library(dplyr)

# Sample data frame M (this should be loaded or defined as per your actual data)
# Example: load("M.RData")

# Step 1: Split each CR field on the semicolon and create a new data frame
split_data <- data.frame(Row_Number = integer(), Split_String = character(), stringsAsFactors = FALSE)

for (i in 1:nrow(M)) {
  # Get the CR string from the current row
  cr_string <- M$CR[i]
  # Split the CR string by semicolon
  split_semicolon <- str_split(cr_string, ";")[[1]]
  # Create a data frame with the split strings and the original row number
  temp_df <- data.frame(Row_Number = rep(i, length(split_semicolon)), Split_String = split_semicolon, stringsAsFactors = FALSE)
  # Append to the split_data data frame
  split_data <- rbind(split_data, temp_df)
}

# Step 2: Extract the longest string between commas for each split segment
find_longest_string <- function(string) {
  # Split the string by comma
  split_comma <- str_split(string, ",")[[1]]
  # Find the longest string among the comma-split segments
  longest_string <- split_comma[which.max(nchar(split_comma))]
  return(longest_string)
}

# Apply the function to each row of the split_data data frame
split_data$Longest_String <- sapply(split_data$Split_String, find_longest_string)


# Load necessary libraries
library(tm)
library(proxy)

# Sample data frame M (this should be loaded or defined as per your actual data)
# Example: load("M.RData")

# Define split_data as per previous steps
# Step 1: Split each CR field on the semicolon and create a new data frame
split_data <- data.frame(Row_Number = integer(), Split_String = character(), stringsAsFactors = FALSE)

for (i in 1:nrow(M)) {
  # Get the CR string from the current row
  cr_string <- M$CR[i]
  # Split the CR string by semicolon
  split_semicolon <- str_split(cr_string, ";")[[1]]
  # Create a data frame with the split strings and the original row number
  temp_df <- data.frame(Row_Number = rep(i, length(split_semicolon)), Split_String = split_semicolon, stringsAsFactors = FALSE)
  # Append to the split_data data frame
  split_data <- rbind(split_data, temp_df)
}

# Step 2: Extract the longest string between commas for each split segment
find_longest_string <- function(string) {
  # Split the string by comma
  split_comma <- str_split(string, ",")[[1]]
  # Find the longest string among the comma-split segments
  longest_string <- split_comma[which.max(nchar(split_comma))]
  return(longest_string)
}

# Apply the function to each row of the split_data data frame
split_data$Longest_String <- sapply(split_data$Split_String, find_longest_string)

#TITLE MATCHING

# Load necessary libraries
library(tm)
library(proxy)

# Sample data frame M (this should be loaded or defined as per your actual data)
# Example: load("M.RData")

# Define split_data as per previous steps
# Step 1: Split each CR field on the semicolon and create a new data frame
split_data <- data.frame(Row_Number = integer(), Split_String = character(), stringsAsFactors = FALSE)

for (i in 1:nrow(M)) {
  # Get the CR string from the current row
  cr_string <- M$CR[i]
  # Split the CR string by semicolon
  split_semicolon <- str_split(cr_string, ";")[[1]]
  # Create a data frame with the split strings and the original row number
  temp_df <- data.frame(Row_Number = rep(i, length(split_semicolon)), Split_String = split_semicolon, stringsAsFactors = FALSE)
  # Append to the split_data data frame
  split_data <- rbind(split_data, temp_df)
}

# Step 2: Extract the longest string between commas for each split segment
find_longest_string <- function(string) {
  # Split the string by comma
  split_comma <- str_split(string, ",")[[1]]
  # Find the longest string among the comma-split segments
  longest_string <- split_comma[which.max(nchar(split_comma))]
  return(longest_string)
}

# Apply the function to each row of the split_data data frame
split_data$Longest_String <- sapply(split_data$Split_String, find_longest_string)


library(stringdist)
library(dplyr)

# Assuming split_datat$Longest_string and M$TI are your input vectors
longest_strings <- split_data$Longest_String
ti_strings <- M$TI

# Initialize an empty data frame to store the results
result <- data.frame(
  Longest_string = character(length(longest_strings)),
  Longest_string_row_number = integer(length(longest_strings)),
  Best_matching_TI = character(length(longest_strings)),
  Best_matching_TI_row_number = integer(length(longest_strings)),
  stringsAsFactors = FALSE
)

# Open a file to log the debug output
log_file <- "debug_log.txt"
sink(log_file)


# Calculate string distances and find the best match for each Longest_string
for (i in seq_along(longest_strings)) {
  distances <- stringdist::stringdist(longest_strings[i], ti_strings, method = "jw") # Jaro-Winkler distance
  best_match_index <- which.min(distances)
  
  # Calculate similarity (1 - distance)
  similarity <- 1 - distances[best_match_index]
  
  # Debug output
  cat("Longest_string:", longest_strings[i], "\n")
  cat("Best_matching_TI:", ti_strings[best_match_index], "\n")
  cat("Similarity:", similarity, "\n")
  
  # Store the results in the appropriate row
  result$Longest_string[i] <- longest_strings[i]
  result$Longest_string_row_number[i] <- i
  
  if (similarity >= 0.84) {
    result$Best_matching_TI[i] <- ti_strings[best_match_index]
    result$Best_matching_TI_row_number[i] <- best_match_index
  } else {
    result$Best_matching_TI[i] <- NA
    result$Best_matching_TI_row_number[i] <- NA
  }
}

# Close the log file
sink()

# Display the result data frame
print(result)
