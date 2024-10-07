library(bibliometrix)
library(bibliometrixData)
library(tidyverse)
library(quanteda)
library(quanteda.textstats)

#indicate file source
file <- "SDG_TI_TI_2024_06_18.csv"

#convert scopus file to data.frame
M <- convert2df(file = file, dbsource = "scopus", format = "csv")

# Load necessary library
library(stringr)

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


#TITLE MATCHING

# Load necessary libraries
library(tm)
library(proxy)

# Assuming split_datat$Longest_string and M$TI are your input vectors
split_data$trunctitle <- substr(split_data$Longest_String, 1, 100)
longest_strings <- substr(split_data$Longest_String, 1, 100)
corp_ls <- corpus(split_data$trunctitle, docvars = split_data$Row_Number)
docvars(corp_ls, "TT") <- split_data$trunctitle
docvars(corp_ls)
corp_ls_df <- corp_ls %>% convert(.,to="data.frame")
write_csv2(corp_ls_df, file ="corp_ls_df.csv")


corp_ls_df[18818,]dim(M)
ti_strings <- str_trunc(M$TI,100) 
corp_ti <- corpus(ti_strings, docvars = seq(1:length(ti_strings)))
M$TT <-  str_trunc(M$TI,100)
corp_ti_df <- corp_ti %>% convert(.,to="data.frame")
write_csv2(corp_ti_df, file ="corp_ti_df.csv")
docvars(corp_ti, "DOI") <- M$DI
docvars(corp_ti, "title") <- M$TT
docvars(corp_ti)


#tokenizer
tokenizer <- function(string){
  tokens(string) %>%
    tokens_tolower() %>%
    tokens(remove_punct = TRUE, remove_symbols = TRUE, 
           remove_numbers = TRUE, remove_url = TRUE) %>%
        tokens_remove(stopwords("english"))
}

tokens_ls <- tokenizer(corp_ls)
tokens_ti <- tokenizer(corp_ti)

dfm_ls <- dfm(tokens_ls)
dfm_ti <- dfm(tokens_ti)

dim(dfm_ls)
dim(dfm_ti)

dfmsim <- textstat_simil(dfm_ls, dfm_ti, margin = "documents", method = "correlation") %>% as_tibble()
write_csv2(dfmsim, file = "sim_matrix.csv")


result <- dfmsim %>% filter(correlation >=0.85) %>% 
  group_by(document2) %>%
  reframe(LCS = n()) %>%
  arrange(desc(LCS))

references <- data.frame(ref = rownames(M), index = seq(1,nrow(M)))

result2 <- result %>% mutate(index = as.integer(str_extract(document2, "\\d+")))
result3 <- result2 %>% left_join(., references, by = c("index"="index"))

