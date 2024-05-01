library(stm)
library(tidyverse)
library(quanteda.textstats)
library(stats)

texten <- M$AB 
texten
abstracts <- corpus(M$AB)

remove_words <- c("sustainable_development_goals", "agenda", "sustainable development goals", "goals")

tokens <- tokens(abstracts) %>%
  tokens_tolower() %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_replace(replace_frame$find, replace_frame$replace, valuetype = "fixed") %>%
  tokens_remove(remove, padding = TRUE) %>%
  tokens_remove(remove_words, padding = TRUE) %>%
  tokens_remove(pattern = ";") %>%
  tokens_select(min_nchar = 3)# takes out abbreviations 

kwic(tokens, pattern = "sustainable")
tokens_ngrams <- tokens_ngrams(tokens, n= 2:3)
dfm_ngrams <- dfm(tokens_ngrams)
ngrams <- textstat_frequency(dfm_ngrams) %>% mutate(feature = str_replace_all(feature, "_", " ")) 

threshold <- quantile(ngrams$frequency, probs = 0.95)
threshold <- 10
ngrams <- ngrams %>% filter(frequency > threshold) 

dict_n <- dictionary(list(compound = ngrams$feature))
dict_n

tokens <- tokens(abstracts) %>%
  tokens_tolower() %>%
  tokens(remove_punct = TRUE, remove_symbols = TRUE, 
         remove_numbers = TRUE, remove_url = TRUE) %>%
  tokens_replace(replace_frame$find, replace_frame$replace, valuetype = "fixed") %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(remove, padding = TRUE) %>%
  tokens_compound(.,dict_n, join = TRUE) %>%
  tokens_remove(remove_words, padding = TRUE) %>%
  tokens_remove(pattern = c(";", ".")) %>%
  tokens_remove(pattern = "'s", valuetype = "regex") %>% 
  tokens_select(min_nchar = 3) 
dfm <- dfm(tokens) %>% dfm_trim(min_termfreq = 0.9, termfreq_type = "quantile" )

dim(dfm)


dfm_stm <- dfm %>% convert(.,to = "stm")
dfm_stm
#save(dfm_stm, file = "dfm_stm.Rdata")

set.seed(100)
range <- seq(10,26, by = 2)

many_models <- tibble(K = range) %>%
  mutate(topic_model = map(K, ~stm(dfm_stm$documents,
                                   dfm_stm$vocab,
                                   data = dfm_stm$meta, 
                                   K = .,
                                   verbose = TRUE,
                                   emtol = 1e-05)))

save(many_models, file = "sdg_litrev_10_26.Rdata")
#load("sdg_litrev_5_50.Rdata")
heldout <- make.heldout(dfm_stm$documents, dfm_stm$vocab)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm_stm$documents),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, dfm_stm$documents),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))
save(k_result, file = "k_result.Rdata")

k_result %>%
  transmute(K,
            #`Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout"),
            `Exclusivity` = map_dbl(exclusivity, mean)) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(linewidth = 1.5, alpha = 0.7, show.legend = FALSE) +
  geom_point(size = 3) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
  )
ggsave("kplot_frags.png", width = 12, dpi = 300, units = "cm")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  filter(K %in% range) %>%
  unnest(cols = c(semantic_coherence, exclusivity)) %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 3, alpha = 1) +
  geom_rect(aes(xmin = -Inf, xmax = -200, ymin = -Inf, ymax = Inf), fill = "grey90", alpha = 0.01, col = NA) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 8.75), fill = "grey90", alpha = 0.01, col = NA) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "",
       subtitle = "")
ggsave("semex_frags5-50.png", width = 12, dpi = 300, units = "cm", bg = "white")


#extract top themes
#geef aantal
aantal <- 18
topic_model <- k_result %>% 
  filter(K == aantal) %>% 
  pull(topic_model) %>% 
  .[[1]]
summary(topic_model)
kwic(tokens, pattern = "impossible")

td_beta <- tidytext::tidy(topic_model, matrix = "beta")
td_gamma <- tidytext::tidy(topic_model, matrix = "gamma",
                           document_names = rownames(dfm_stm))
td_frex <- tidytext::tidy(topic_model, matrix = "frex",
                          document_names = rownames(dfm_stm)) %>%
  rename(frex = term)

td_lift <- tidytext::tidy(topic_model, matrix = "lift",
                          document_names = rownames(dfm_stm)) %>%
  rename(lift = term)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols = c(terms))

top_frex <- td_frex %>%
  group_by(topic) %>%
  summarise(terms = list(frex)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) 

# Assuming 'top_frex' is your data frame
top_frex_df <- top_frex %>%
  mutate(terms = sapply(terms, function(x) paste(strsplit(x, ",")[[1]][1:10], collapse = ", ")))

top_lift <- td_lift %>%
  group_by(topic) %>%
  summarise(terms = list(lift)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) 

# Assuming 'top_frex' is your data frame
top_lift_df <- top_lift %>%
  mutate(terms = sapply(terms, function(x) paste(strsplit(x, ",")[[1]][1:10], collapse = ", ")))


#select those topics that meet semantic coherence and exclusivity criteria
topic_select_f <- k_result %>% 
  filter(K == aantal) %>% 
  select(semantic_coherence, exclusivity,K) %>%
  unnest(., cols = c(semantic_coherence, exclusivity)) %>%
  mutate(topic = 1:K) #%>%
#filter(semantic_coherence > -120 & exclusivity > 9.0) 

library(gt)
gamma_terms <- td_gamma %>%
  filter(topic %in% topic_select_f$topic) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  #top_n(10, gamma) %>%
  left_join(top_terms, by = "topic") %>%
  left_join(top_frex_df, by = "topic") %>%
  #left_join(top_lift_df, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) %>%
  rename(highest_prob = terms.x, FREX = terms.y)

table_gt <- gamma_terms %>%
  gt() %>%
  tab_spanner(
    label = "Terms",
    columns = c("highest_prob", "FREX")
  ) %>%
  fmt_number(
    columns = c("gamma"),
    decimals = 3
  ) %>%
  tab_spanner(
    label = "Topic",
    columns = c("topic")
  )

table_gt
gtsave(table_gt, file = "table_frags.html")
kwic(tokens, pattern = "sdg_reporting")

library(scales)
gamma_terms %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = highest_prob, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = percent_format()) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 13)) +
  labs(x = NULL, y = expression(gamma),
       title = "Top 20 topics by prevalence",
       subtitle = "With the top words that contribute to each topic")

docvars <- docvars(dfm)
docvars <- data.frame(document = 1:nrow(docvars), year = docvars$year)
gamma_year <- td_gamma %>% left_join(., docvars)
library(ggplot2)

# Assuming your data frame is named 'df'

# Calculate the average gamma values per topic per year
avg_gamma <- aggregate(gamma ~ topic + year, data = gamma_year, FUN = mean)

topic_select <- aggregate(gamma ~topic, data = gamma_year, FUN = mean) %>% top_n(10)
avg_gamma1 <- avg_gamma %>% left_join(., custom_labels) %>% filter(topic %in% topic_select$topic & year > 2019) 

# Create the ggplot
# Create the ggplot
ggplot(avg_gamma1, aes(x = as.integer(year), y = gamma, group = topic, fill = titles)) +
  geom_col(position = "stack") + 
  labs(x = "Year", y = "Average Gamma") +
  theme_minimal()

# EXPLORE KEYWORDS IN CONTEXT
#load("tokens_retail.Rdata")
kwic(toks, pattern = "perspective", window = 3)


#estimate effects
set.seed(100)
effect <- estimateEffect(formula= ~ year, 
                         stmobj=topic_model, 
                         metadata=dfm_stm$meta,
                         prior = 1e-5)
summary(effect)
plot(effect, "year")
save(effect, file = "effect.Rdata")



docs_to_keep <- names(dfm_stm$documents)
#texts_ret <- corp %>% convert(.,to= "data.frame") %>% 
  subset(.,doc_id %in% docs_to_keep) %>%
  select(text)

texts_ret <- abstracts %>% convert(.,to= "data.frame") %>% 
  subset(.,doc_id %in% docs_to_keep) %>%
  select(text)
texts_ti <- corpus(M$TI) %>% convert(.,to= "data.frame") %>% 
  subset(.,doc_id %in% docs_to_keep) %>%
  select(text)

#find thoughts on abstracts
findThoughts(topic_model, texts = texts_ret$text, topics = 1, n = 3, thresh = 0.05)
#find thought on titles
findThoughts(topic_model, texts = texts_ret$ti, topics = 1, n = 3, thresh = 0.05)

#CORRELATION PLOTS
library(ggplot2)
library(huge)
# Assuming mod.out.corr is your list of correlation matrices
mod.out.corr <- topicCorr(topic_model, method = "huge", cutoff = 0, verbose = TRUE)
plot(mod.out.corr)

library(igraph)

# Assuming mod.out.corr is your list of correlation matrices

# Extract positive correlation matrix
positive_corr <- mod.out.corr$poscor
mod.out.corr$cor
# Create an adjacency matrix with only positive correlations
adj_matrix <- as.matrix(mod.out.corr$cor > 0)
adj_matrix
# Create an igraph graph from the adjacency matrix
graph <- graph.adjacency(adj_matrix, mode = "undirected", weighted = TRUE)

# Remove isolated nodes and their incident edges
graph <- simplify(graph)

# Set vertex names in the graph
V(graph)$name <- seq_len(vcount(graph))

# Extract top-10 topics with highest mean gammas
# Convert topic numbers to integers
top_topics <- as.integer(gsub("Topic ", "", gamma_terms$topic))

# Set a different color for top-10 topics
vertex_colors <- ifelse(V(graph)$name %in% top_topics, "#FF7F7F", "lightblue")

# Plot the network
plot(graph, layout = layout.fruchterman.reingold, 
     vertex.color = vertex_colors, vertex.size = 10, 
     vertex.label.dist = 2, vertex.label.cex = 0.7,
     edge.color = "darkgreen", edge.width = 1.5,
     main = "")

kwic(tokens, pattern = "local")

# Custom labels for top topics (replace with your own labels)
# Create a data frame with topic (numeric) and title (character)
custom_labels <- data.frame(
  topic = 1:aantal,
  titles = c("Negative Health Externalities",
              "SDG-oriented \n PPP Governance",
              "Social Enterprises & Cooperatives",
              "Sustainable Supply Chains & Industries",
              "\n Financial Performance & SDGs",
              "Policies for Economic Growth",
              "Business Transformation & Innovation",
              "Ethics in Business and Consumption",
              "Health in \n Global \n\r Communities", 
              "Sustainability Indicators & Frameworks",
              "Tourism Nature Interactions",
              "Finance for Sustainable \n Infrastructure",
              "CSR & Stakeholder Engagement ",
              "Digital Innovation & Training",
              "Local Water \n Partnerships",
              "Green Innovations in SMEs",
              "Sustainablity in \n Business Education",
              "Corporate Sustainability Reporting")
  
)

# Set up a color scale with higher contrast
color_scale <- colorRampPalette(c("white", "darkgreen"))  # Adjust the color range

mean_thetas <- td_gamma %>%
  filter(topic %in% topic_select_f$topic) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>% pull(gamma)

# Create a color vector based on the mean thetas
vertex_colors <- color_scale(length(mean_thetas))[cut(mean_thetas, length(mean_thetas))]

set.seed(135)
# Plot the corplot with colored vertices
corplot_f <- plot(graph, layout = layout.fruchterman.reingold, 
                  vertex.color = vertex_colors, 
                  vertex.size = 16, 
                  vertex.label = custom_labels$titles,
                  vertex.label.dist = 1, vertex.label.cex = 0.9,
                  vertex.label.font = 2,  # Use font style 2 for bold
                  edge.color = "black", edge.width = 3,
                  vertex.label.family = "Arial",
                  vertex.label.dist =2, vertex.label.cex = 5,
                  main = "")

# Plot the network
set.seed(3)
plot(graph, layout = layout.fruchterman.reingold, 
     vertex.color = vertex_colors, vertex.size = 10, 
     edge.color = "darkgreen", edge.width = 1.5,
     main = "", 
     vertex.label = custom_labels$titles)  # Use custom labels as vertex labels

# use LDAvis to explore topics
library(LDAvis)
library(servr)
json <- toLDAvis(topic_model, dfm_stm$documents, R = 30, 
                 out.dir = "C:/Users/hwwaal/projects/SDG_LITREV/LDAVis",
                 open.browser = T)



