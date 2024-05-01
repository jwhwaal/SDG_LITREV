library(bibliometrix)
library(bibliometrixData)
library(tidyverse)

file <- "SDG_TI_TO_2024.bib"

M <- convert2df(file = file, dbsource = "wos", format = "bibtex")
#M %>% filter(JI != "SUSTAINABILITY") %>% nrow()

unique(M$JI)

results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, k = 20, pause = FALSE)

#plot(results)
# Create a data frame
df <- data.frame(Year = unique(results$Years), Counts = table(results$Years))
df$Year <- as.numeric(levels(df$Counts.Var1))
# Load the ggplot2 library
library(ggplot2)

# Create the ggplot
ggplot(data = df, aes(x = Year, y = Counts.Freq)) +
  geom_line() + # Line plot
  geom_area(fill = "lightgreen", alpha = 0.5) + # Area below the curve
  labs(x = "year", y = "article count") + # Labels for axes
  theme_minimal() + # Minimal theme
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 1)) # Display whole numbers on x-axis

# Create the ggplot
df %>% filter(Year < 2024) %>% 
  ggplot(aes(x = Year, y = Counts.Freq)) +
  geom_histogram(stat = "identity", fill = "lightgreen", alpha = 0.5) + # Histogram
  geom_text(aes(label = Counts.Freq), vjust = -0.5, color = "black", size = 3) + # Add text labels
  labs(x = "Year", y = "Number of Articles") + # Labels for axes
  theme_minimal() + # Minimal theme
  scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 1)) # Display whole numbers on x-axis

head(results$MostCitedPapers, 10)
head(results$AuthorsFrac)
head(results$Sources, 10)
results$TotalCitation


#journals
df2 <- as.data.frame(results$Sources)
df2$SO <- as.character(df2$SO)
# Sort the dataframe by Freq in descending order
# Calculate the threshold count value for the top 25%
threshold <- quantile(df2$Freq, probs = 0.975)

# Filter the dataframe to keep only the top 25% of counts
filtered_df <- df2 %>% 
  filter(Freq >= threshold) %>%
  arrange(Freq)
# Insert line breaks after every two words
filtered_df$SO <- gsub("((\\w+\\s){3})", "\\1\n", filtered_df$SO)

filtered_df %>%
  ggplot(aes(x = factor(SO, levels = SO), y = Freq)) +
  geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.5) + # Histogram
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3) + # Add text labels
  labs(x = "Year", y = "Number of Articles") + # Labels for axes
  theme_minimal() +
  coord_flip()

#Local Citations
LC <-  localCitations(M, fast.search = FALSE, sep = ";", verbose = FALSE)
# Plotting
# Plotting the top 10 papers based on LCS score
df3 <- LC$Papers
top_10 <- df3 %>% 
  filter(Year < 2024) %>% 
  arrange(desc(LCS)) %>%  # Arrange in descending order of LCS score
  head(10)  # Select top 10

# Plotting
ggplot(top_10, aes(x = reorder(Paper, -LCS), y = LCS)) +
  geom_bar(stat = "identity", fill = "lightgreen", alpha = 0.5) + # Bar plot
  geom_text(aes(label = LCS), vjust = -0.5, color = "black", size = 3) + # Add text labels
  labs(x = "Paper", y = "Local citation frequency") + # Axes labels
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Rotate x-axis labels


CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

DF <- dominance(results, k = 20)
DF

topAU <- authorProdOverTime(M, k = 20, graph = TRUE)

A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
A <- cocMatrix(M, Field = "CR", sep = ".  ")
A <- cocMatrix(M, Field = "AU", sep = ";")
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")
A <- cocMatrix(M, Field = "DE", sep = ";")
A <- cocMatrix(M, Field = "ID", sep = ";")
A
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", 
                type = "fruchterman", size.cex=TRUE, size=10, 
                remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", n = 50, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size.cex=TRUE, size=30, 
                remove.multiple=T, edgesize = 8, labelsize=5,label.cex=TRUE,label.n=30,edges.min=2)

# Open a larger plot window
options(repr.plot.width=10, repr.plot.height=6)  # Adjust width and height as needed

# Plot the network
set.seed(1)
net <- networkPlot(NetMatrix, normalize="jaccard", 
                   n=100, Title="Keyword Co-occurrences", 
                   type="sphere", size.cex=TRUE, size=30, 
                   cluster = "leading_eigen",
                   remove.multiple=TRUE, 
                   edgesize=10, labelsize=2, label.cex=F, label.n=50, edges.min=2,
                   community.repulsion = 0.2)





# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M, field="AB", method="MCA", minDegree=2, clust= "auto", 
                          stemming=F, labelsize=10, documents=10, ngrams = 1, remove.terms=remove)



options(width=130)
histResults <- histNetwork(M, min.citations = 1, sep = ";")

# Plot a historical co-citation network
net <- histPlot(histResults, n=20, size = 5, labelsize=5)

Map=thematicMap(M, field = "ID", n = 250, minfreq = 10, 
                stemming = TRUE, size = 0.7, n.labels=5, repel = TRUE,
                ngrams = 2, remove.terms = remove )
plot(Map$map)
Map2 <- thematicEvolution(M, field = "ID", n = 250,  years=2010, minFreq = 2,
                  stemming = TRUE, size = 0.7, n.labels=5, repel = TRUE,
                  ngrams = 2, remove.terms = remove)

plot(Map2$map)

remove = c(""
           )

CSA <- conceptualStructure(M, method="MCA", field="AB", minDegree=3,ngrams = 2, 
                          clust=7, stemming=FALSE, labelsize=30,documents=20,
                          remove.terms = remove)

  CST <- conceptualStructure(M, method="MCA", field="TI", minDegree=2,ngrams = 1, 
                          clust=5, stemming=TRUE, labelsize=15,documents=5,
                          remove.terms = remove)



M=metaTagExtraction(M,"CR_SO",sep=";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "sources", sep = ";")
net=networkPlot(NetMatrix, n = 10, Title = "Co-Citation Network", type = "auto", size.cex=TRUE, 
                size=10, remove.multiple=TRUE, labelsize=1,edgesize = 10, edges.min=10)

vector_elements <- results$FirstAffiliation
length(unique(vector_elements))

# Create a frequency table
freq_table <- table(vector_elements)

# Convert the table to data frame and sort by frequency
freq_df <- data.frame(Element = names(freq_table), Frequency = as.numeric(freq_table))
freq_df <- freq_df[order(-freq_df$Frequency), ]

# Plot the bar graph using ggplot
library(ggplot2)
ggplot(freq_df, aes(x = reorder(Element, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frequency of Vector Elements", x = "Elements", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  # Rotate x-axis labels for better readability


# Select only the top 10 most frequent elements
top_10 <- freq_df %>% 
  arrange(desc(Frequency)) %>% 
  head(10)

# Plot the bar graph using ggplot
library(ggplot2)
ggplot(top_10, aes(x = reorder(Element, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Nigerian Horticultural Universities by papers in Web of Science", x = "Elements", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  coord_flip()# Rotate x-axis labels for better readability


NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", n = 30, 
                Title = "Keyword Co-occurrences", type = "fruchterman", 
                size.cex=T, 
                size=10, remove.multiple=F, edgesize = 10, 
                labelsize=5,label.cex=TRUE,label.n=30,
                edges.min=3)

suppressWarnings(
  CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=15, ngrams = 2,
                            clust=5, stemming=FALSE, labelsize=15,documents=2)
)

