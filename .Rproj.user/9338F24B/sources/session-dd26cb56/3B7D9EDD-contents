library(stm)
library(tidyverse)

dfm_stm <- dfm %>% 
  dfm_select(., words, selection = "remove")  %>%
  dfm_trim(min_termfreq = 10, 
           min_docfreq = 3, termfreq_type = "count") %>%
  convert(.,to = "stm")

rownames <- rownames(dfm_stm)

save(dfm_stm, file = "dfm_stm.Rdata")
unique(docvars(dfm)$ownership)
set.seed(100)
range <- c(5L, 10L, 15L, 20L, 25L, 30L, 40L, 50L)

many_models <- tibble(K = range) %>%
  mutate(topic_model = map(K, ~stm(dfm_stm$documents,
                                   dfm_stm$vocab,
                                   data = dfm_stm$meta, 
                                   K = .,
                                   prevalence = ~ year + ownership,
                                   verbose = TRUE)))



save(many_models, file = "retailers_5_50.Rdata")
#load("retailers_5_50.Rdata")
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
  geom_rect(aes(xmin = -Inf, xmax = -70, ymin = -Inf, ymax = Inf), fill = "grey90", alpha = 0.01, col = NA) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 8.75), fill = "grey90", alpha = 0.01, col = NA) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "",
       subtitle = "")
ggsave("semex_frags5-100.png", width = 12, dpi = 300, units = "cm", bg = "white")


#extract top themes
#geef aantal
aantal <- 20
topic_model <- k_result %>% 
  filter(K == aantal) %>% 
  pull(topic_model) %>% 
  .[[1]]
summary(topic_model)


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




# EXPLORE KEYWORDS IN CONTEXT
load("tokens_retail.Rdata")
kwic(toks, pattern = "banana", window = 5)

k_result$exclusivity[3]
#estimate effects
set.seed(100)
effect <- estimateEffect(formula= ~ year + ownership, 
                         stmobj=topic_model, 
                         metadata=dfm_stm$meta,
                         prior = 1e-5)
summary(effect)
save(effect, file = "effect.Rdata")




docs_to_keep <- names(dfm_stm$documents)
texts_ret <- corp_en %>% convert(.,to= "data.frame") %>%
  mutate(docid = names(corp_en)) %>% 
  subset(.,docid %in% docs_to_keep) %>%
  select(text)


findThoughts(topic_model, texts = texts_ret$text, topics = 1, n = 3, thresh = 0.05)















#CORRELATION PLOTS
library(ggplot2)
library(huge)
# Assuming mod.out.corr is your list of correlation matrices
mod.out.corr <- topicCorr(topic_model, method = "huge")
plot(mod.out.corr)

library(igraph)

# Assuming mod.out.corr is your list of correlation matrices



print(topics)

# Extract positive correlation matrix
positive_corr <- mod.out.corr$poscor

# Create an adjacency matrix with only positive correlations
adj_matrix <- as.matrix(positive_corr > 0)

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

# Custom labels for top topics (replace with your own labels)
# Create a data frame with topic (numeric) and title (character)
custom_labels <- data.frame(
  topic = 1:20,
  titles = c(
    "Stores Growth and Market Expansion",
    "Corporate Governance and Board Members",
    "Product Quality and CSR Initiatives",
    "Company Performance and Sustainability",
    "Financial Reporting and Governance",
    "Risk Management and Compliance",
    "Financial Assets and Taxation",
    "Employee Training and Support",
    "Audit and Financial Statements",
    "Community Support and Engagement",
    "Financial Profits and Equity",
    "Sustainability in Work and Supply Chains",
    "Remuneration and Performance Plans",
    "Asset Valuation and Impairment",
    "Sustainable Products and Consumer Preferences",
    "Corporate Strategy and Operations",
    "Environmental Impact and Waste Management",
    "Sustainable Production and Certification",
    "GRI Reporting and Stakeholder Engagement",
    "Financial Risk and Derivatives"
  )
)


# Display the data frame
print(topics)



# Set up a color scale with higher contrast
color_scale <- colorRampPalette(c("white", "darkred"))  # Adjust the color range

mean_thetas <- td_gamma %>%
  filter(topic %in% topic_select_f$topic) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>% pull(gamma)

# Create a color vector based on the mean thetas
vertex_colors <- color_scale(length(mean_thetas))[cut(mean_thetas, length(mean_thetas))]

set.seed(105)
# Plot the corplot with colored vertices
corplot_f <- plot(graph, layout = layout.fruchterman.reingold, 
                  vertex.color = vertex_colors, vertex.size = 10, 
                  vertex.label = custom_labels$titles,
                  vertex.label.dist = 2, vertex.label.cex = 0.7,
                  edge.color = "darkgreen", edge.width = 1.5,
                  vertex.label.family = "Arial",
                  vertex.label.dist = 4, vertex.label.cex = 5,
                  main = "")

# Plot the network
set.seed(3)
plot(graph, layout = layout.fruchterman.reingold, 
     vertex.color = vertex_colors, vertex.size = 10, 
     edge.color = "darkgreen", edge.width = 1.5,
     main = "", 
     vertex.label = custom_labels$titles)  # Use custom labels as vertex labels








