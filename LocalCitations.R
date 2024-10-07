LocalCitations <- function(M) {
  library(stringr)
  library(tm)
  library(quanteda)
  library(dplyr)
  
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
  
  # Function to find the longest string between commas after squishing spaces
  find_longest_string <- function(string) {
    # Split the string by comma
    split_comma <- str_split(string, ",")[[1]]
    # Squish spaces for each segment
    split_comma_squished <- str_squish(split_comma)
    # Find the longest string among the squished comma-split segments
    longest_string <- split_comma_squished[which.max(nchar(split_comma_squished))]
    return(longest_string)
  }
  
  # Apply the function to each row of the split_data data frame
  split_data$Longest_String <- sapply(split_data$Split_String, find_longest_string)
  
  # Truncate the titles
  split_data$trunctitle <- substr(split_data$Longest_String, 1, 100)
  longest_strings <- substr(split_data$Longest_String, 1, 100)
  ti_strings <- str_trunc(M$TI, 100)
  
  # Create corpora for title matching
  corp_ls <- corpus(split_data$trunctitle, docvars = data.frame(Row_Number = split_data$Row_Number))
  corp_ti <- corpus(ti_strings, docvars = data.frame(DI = M$DI, title = ti_strings))
  
  # Tokenize
  tokenizer <- function(string){
    tokens(string) %>%
      tokens_tolower() %>%
      tokens(remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE, remove_url = TRUE) %>%
      tokens_remove(stopwords("english"))
  }
  
  tokens_ls <- tokenizer(corp_ls)
  tokens_ti <- tokenizer(corp_ti)
  
  dfm_ls <- dfm(tokens_ls)
  dfm_ti <- dfm(tokens_ti)
  
  # Compute similarity matrix
  dfmsim <- textstat_simil(dfm_ls, dfm_ti, margin = "documents", method = "correlation") %>%
    as_tibble()
  
  # Filter and group results
  result <- dfmsim %>% filter(correlation >= 0.85) %>%
    group_by(document2) %>%
    summarize(LCS = n()) %>%
    arrange(desc(LCS))
  
  references <- data.frame(ref = rownames(M), index = seq(1, nrow(M)))
  
  result2 <- result %>% mutate(index = as.integer(str_extract(document2, "\\d+")))
  result3 <- result2 %>% left_join(references, by = c("index" = "index"))
  
  return(result3)
}

# Example usage
# Load necessary data frame M
# M <- load("path_to_M.RData")

TITO_LCS <- LocalCitations(M)
