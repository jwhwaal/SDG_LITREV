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
         journal = `Source Title`) %>%
  select(-Abstract)
#create corpus, hash and convert to dataframe
corp <- corpus(texts, text_field = "text")
head(docvars(corp))

#d some lemmatization
find <- c("report", "model", "school", "sme", "target", "policy", "university",
          "indicator", "organisations", "behaviour", "labour", "method",
          "partnership", "cooperative", "ppp", "businesses", "company", "impacts",
          "disclosures", "firm", "risk", "project","industry",  "enterprise",
          "chain")

replace <- c("reports", "models", "schools", "smes", "targets",
             "policies", "universities", "indicators", "organizations", "behavior", "labor",
             "methods", "partnerships", "cooperatives", "ppps", "business", "companies", "impact",
             "disclosure", "firms", "risks", "projects", 
             "industries", "enterprises", "chains")


remove <- c("study", "aim", "aims", "paper", "results", "result", "can", "research",
            "used", "data", "literature", "findings", "methodology","analysis",
            "using", "also", "approach", "published", "publications",
            "set", "sector", "ltd", "author", "article", "articles",
            "mediating", "obtained", "authors", "limitations",
            "panel", "new", "methods", "use", "part", "respondents",
            "group", "propose", "based", "outcomes", "argue", "argues", "among",
            "sustainable development goals", "sdg", "sdgs", "however",
            "implicationsthe", "upon", "studied", "ante", "medians",
            "others", "reviews", "papers", "elsevier", "robust",
            "robustness", "originality",  "practical_implications",
            "best_knowledge", "academic", "implications", "first", "two",
            "already", "well", "determinants", "mediating", "moderating",
            "rights_reserved", "towards", "structural_equation", "interviews",
            "development", "goals", "sample", "method", "methods", "tool", "show", "one", "significant", "relationship",
            "within", "thus", "including", "suggest", "across", "review", "test",
            "theory", "effect", "contribute", "whether", "furthermore", "finally",
            "firstly", "secondly", "particularly", "considered", "shows", "analyzed",
            "identified", "made", "empirical", "relationships", "indicate", "despite",
            "moreover", "understanding", "therefore", "assess", "explores", "conducted",
            "effects", "terms", "examine", "find", "applied", "statistically", "found",
            "proposed", "dimensions", "criteria", "twitter", "iii", "besides", "per",
            "nevertheless", "findingsthe", "trillion", "methodological", "hypotheses",
            "million", "dataset", "addition", "present", "models", "agenda", "help",
            "adoption", "purpose", "design", "results_show", "understand", "additionally",
            "especially", "presented", "towards", "toward", "beyond", "achievement", 
            "across", "new", "added", "present", "presented", "perspective", "context", "achieving",
            "evidence", "impications", "exploring", "related", "lens", "selected", "case", "early",
            "particular", "different", "affect", "many", "since", "significantly")
#remove <- NULL
replace_frame <- data.frame(find, replace)


#tokenize
toks <- corp %>%
  tokens(.) %>%
  tokens_remove(stopwords("english")) %>%
  #tokens_remove(pattern = remove) %>%
  #tokens_replace(replace_frame$find, replace_frame$replace, valuetype = "fixed") %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(pattern = "\\p{Z}", valuetype = "regex") %>% #take out non-ASCII
  tokens_remove(pattern = "'s", valuetype = "regex") %>% 
  tokens_remove(pattern = "-+", valuetype = "regex") %>% 
  tokens_remove(pattern = "#.+", valuetype = "regex") %>%
  tokens_remove(pattern = "www.+", valuetype = "regex") %>%
  tokens_select(min_nchar = 3)# takes out abbreviations 

kwic(toks, pattern = "sustainable development goals")
toks_ngrams <- tokens_ngrams(toks, n= 2:3)
dfm_ngrams <- dfm(toks_ngrams)
ngrams100 <- textstat_frequency(dfm_ngrams) %>%
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

words <- c("sustainable_development_goals", "sdgs", "sdg", "targets", "indicators",
           "achieving", "achievement", "sustainable", "development", "goals", "sustainability")

toks <- corp %>%
  tokens(.) %>%
  tokens_tolower() %>%
  tokens_compound(.,dict_n, join = TRUE) %>%
  tokens_replace(replace_frame$find, replace_frame$replace, valuetype = "fixed") %>%
  tokens_remove(stopwords("english")) %>%
  #tokens_remove(pattern = remove) %>%
  tokens_remove(pattern = words) %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>% 
  tokens_remove(pattern = "\\p{Z}", valuetype = "regex") %>% #take out non-ASCII
  tokens_replace("\\b(\\w+)'s\\b", "\\1", valuetype = "regex") %>%
  tokens_remove(pattern = "-+", valuetype = "regex") %>% 
  tokens_remove(pattern = "#.+", valuetype = "regex") %>%
  tokens_remove(pattern = "www.+", valuetype = "regex") %>%
  tokens_select(min_nchar = 3)  


dfm <- dfm(toks) %>% 
  dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile") 
head(dfm)
dim(dfm)

kwic(toks, pattern = "global_reporting_initiative", window = 3)

# Calculate TF-IDF scores
my_tfidf <- dfm_tfidf(dfm)
# Extract the terms with their TF-IDF scores
terms_tfidf <- as.data.frame(sort(colSums(my_tfidf), decreasing = TRUE)) 
terms_tfidf

