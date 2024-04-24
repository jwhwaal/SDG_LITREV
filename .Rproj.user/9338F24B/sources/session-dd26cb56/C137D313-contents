
library(readtext)
library(readxl)
library(readr)
library(stringr)
library(stringi)
library(tidyverse)
library(stm)
library(quanteda)
library(broom)
library(quanteda.textstats)
library(data.table)

text <- read_excel("SDG_TI_TO_2024.xls")


texts <- text %>% select(Abstract, `Publication Year`, `Source Title`) %>%
  mutate(text = stri_trans_general(Abstract, id = "Latin-ASCII"),
         text = stringi::stri_replace_all_regex(Abstract, "[\\d]", "")) %>%
  rename(year = `Publication Year`,
         journal = `Source Title`,
         abstract = Abstract) 


#create corpus, hash and convert to dataframe
corp <- corpus(texts, text_field = "abstract")
head(docvars(corp))

#d some lemmatization
find <- c("report", "model", "school", "sme")
replace <- c("reports", "models", "schools", "smes")
remove <- c("study", "aim", "aims", "paper", "results", "result", "can")
replace_frame <- data.frame(find, replace)


#tokenize
toks <- corp %>%
  tokens(.) %>%
  tokens_select(max_nchar = 20) %>% #this takes out a lot of glued junk
  tokens_remove(stopwords("english")) %>%
  tokens_remove(pattern = remove) %>%
  tokens_replace(replace_frame$find, replace_frame$replace, valuetype = "fixed") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(pattern = "\\p{Z}", valuetype = "regex") %>% #take out non-ASCII
  tokens_remove(pattern = "'s", valuetype = "regex") %>% 
  tokens_remove(pattern = "-+", valuetype = "regex") %>% 
  tokens_remove(pattern = "#.+", valuetype = "regex") %>%
  tokens_remove(pattern = "www.+", valuetype = "regex") %>%
  tokens_select(min_nchar = 3) # takes out abbreviations 

kwic(toks, pattern = "model")
toks_ngrams <- tokens_ngrams(toks, n= 2:3)
dfm_ngrams <- dfm(toks_ngrams)
ngrams100 <- textstat_frequency(dfm_ngrams) %>% filter(frequency > 15) %>%
  mutate(keep = "yes")
ngrams <- ngrams100 %>% mutate(feature = str_replace_all(feature, "_", " "))
write_csv2(ngrams, file = "ngrams.csv")
#now sort ngrams separately and save as Excel

#read Ecel sorted ngrams
library(readxl)
ngrams <- read_excel("ngrams.xlsx") %>% filter(keep == "yes")

dict_n <- dictionary(list(compound = ngrams$feature))
dict_n
#tokenize again with compounds

toks <- corp %>%
  tokens(.) %>%
  tokens_tolower() %>%
  tokens_compound(.,dict_n) %>%
  tokens_remove(stopwords("english")) %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(pattern = "\\p{Z}", valuetype = "regex") %>% #take out non-ASCII
  #tokens_replace(replace_frame$find, replace_frame$replaceby, valuetype = "fixed") %>% # do some lemmatization
  tokens_replace("\\b(\\w+)'s\\b", "\\1", valuetype = "regex") %>%
  tokens_remove(pattern = "-+", valuetype = "regex") %>% 
  tokens_remove(pattern = "#.+", valuetype = "regex") %>%
  tokens_remove(pattern = "www.+", valuetype = "regex") %>%
  tokens_select(min_nchar = 3) 

dfm <- dfm(toks)
kwic(toks, pattern = "results_show")



