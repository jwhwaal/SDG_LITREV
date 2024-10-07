# Load necessary libraries
library(bibliometrix)
library(bibliometrixData)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(stringr)
library(tm)
library(proxy)

# Specify file source
file <- "SDG_TI_TI_2024_06_18.csv"

# Convert Scopus file to data.frame
M <- convert2df(file = file, dbsource = "scopus", format = "csv")

# Function to split CR fields by semicolon and create a data frame
split_cr_field <- function(data) {
  split_data <- data.frame(Row_Number = integer(), Split_String = character(), stringsAsFactors = FALSE)
  
  for (i in seq_len(nrow(data))) {
    cr_string <- data$CR[i]
    split_semicolon <- str_split(cr_string, ";")[[1]]
    temp_df <- data.frame(Row_Number = rep(i, length(split_semicolon)), Split_String = split_semicolon, stringsAsFactors = FALSE)
    split_data <- bind_rows(split_data, temp_df)
  }
  
  return(split_data)
}

# Function to find the longest string between commas after removing extra spaces
find_longest_string <- function(string) {
  split_comma <- str_split(string, ",")[[1]]
  squished <- str_squish(split_comma)
  return(squished[which.max(nchar(squished))])
}

# Apply the split function and find the longest string
split_data <- split_cr_field(M)
split_data$Longest_String <- sapply(split_data$Split_String, find_longest_string)

# Title Matching: Prepare Truncated Titles for Corpus Analysis
prepare_corpus <- function(data, column_name, docvar_name) {
  data[[docvar_name]] <- str_trunc(data[[column_name]], 100)
  corpus_obj <- corpus(data[[docvar_name]], docvars = data$Row_Number)
  return(corpus_obj)
}

# Prepare corpora for both CR split strings and titles
corp_ls <- prepare_corpus(split_data, "Longest_String", "trunctitle")
corp_ti <- prepare_corpus(M, "TI", "TT")

# Save corpora as data frames
write_csv2(convert(corp_ls, to = "data.frame"), file = "corp_ls_df.csv")
write_csv2(convert(corp_ti, to = "data.frame"), file = "corp_ti_df.csv")

# Tokenizer function for text cleaning and tokenization
tokenizer <- function(text) {
  tokens(text) %>%
    tokens_tolower() %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>%
    tokens_remove(stopwords("english"))
}

# Apply tokenizer to both corpora
tokens_ls <- tokenizer(corp_ls)
tokens_ti <- tokenizer(corp_ti)

# Create Document-Feature Matrices (DFM) for similarity analysis
dfm_ls <- dfm(tokens_ls)
dfm_ti <- dfm(tokens_ti)

# Compute similarity between the two DFMs
dfm_similarity <- textstat_simil(dfm_ls, dfm_ti, margin = "documents", method = "correlation") %>% as_tibble()
write_csv2(dfm_similarity, file = "sim_matrix.csv")

# Filter results based on correlation threshold and summarize
result <- dfm_similarity %>%
  filter(correlation >= 0.85) %>%
  group_by(document2) %>%
  summarize(LCS = n()) %>%
  arrange(desc(LCS))

# Join result with original references
references <- data.frame(ref = rownames(M), index = seq_len(nrow(M)))
result_final <- result %>%
  mutate(index = as.integer(str_extract(document2, "\\d+"))) %>%
  left_join(references, by = c("index" = "index"))
