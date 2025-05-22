# Load necessary libraries
library(tm)
library(stringr)
library(quanteda)
library(textmineR)
library(dplyr)

# Load your text data
corpuscop <- Corpus(VectorSource(data.thesis.anon$poems))

# Convert to lowercase and remove punctuation
corpuscop <- tm_map(corpuscop, content_transformer(tolower))

remove_all_punctuation <- function(text) {
  text <- gsub("[‘’＇’`´'ʼ’\u2018\u2019\u201B\u2032\u2035\uFF07]", "", text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("[^[:alnum:][:space:]]", "", text)
  return(text)
}

# Apply the function to your corpus
corpuscop <- tm_map(corpuscop, content_transformer(remove_all_punctuation))

# Remove numbers
corpuscop <- tm_map(corpuscop, removeNumbers)

# Strip whitespace
corpuscop <- tm_map(corpuscop, stripWhitespace)

# Create a dictionary of US to UK spellings
us_to_uk <- list(
  "color" = "colour",
  "colors" = "colours",
  "favor" = "favour",
  "favors" = "favours",
  "labor" = "labour",
  "center" = "centre",
  "theater" = "theatre",
  "theater" = "theatres",
  "realize" = "realise",
  "realized" = "realised",
  "analyze" = "analyse",
  "analyzed" = "analysed",
  "defense" = "defence",
  "defenses" = "defences",
  "offense" = "offence",
  "offenses" = "offences",
  "favorite" = "favourite",
  "honor" = "honour",
  "honors" = "honours",
  "traumatize" = "traumatise",
  "traumatized" = "traumatised",
  "realized" = "realised",
  "neighbor" = "neighbour",
  "neighbors" = "neighbours",
  "survivour" = "survivor",
  "honorable" = "honourable",
  "behaviour" = "behaviour",
  "honored" = "honoured",
  "endeavor" = "endeavour",
  "paneling" = "panelling",
  "colored" = "coloured",
  "humor" = "humour",
  "favourite" = "favourite",
  "colorful" = "colourful",
  "rumor" = "rumour",
  "rumours" = "rumours",
  "harboring" = "harbouring",
  "defense" = "defence",
  "scepter" = "sceptre",
  "behavioral" = "behavioural",
  "splendor" = "splendour",
  "watercolor" = "watercolour",
  "watercolors" = "watercolours",
  "savior" = "saviour",
  "misbehavior" = "misbehaviour",
  "misbehaviors" = "misbehaviours",
  "centimeter" = "centimetre",
  "centimeters" = "centimetres",
  "fiber" = "fibre",
  "fibers" = "fibres",
  "favors" = "favours",
  "honor" = "honour",
  "mold" = "mould",
  "molded" = "moulded",
  "behaviors" = "behaviours",
  "hazey" = "hazy",
  "catalog" = "catalogue",
  "dialog" = "dialogue",
  "plow" = "plough",
  "program" = "programme",
  "jewelry" = "jewellery",
  "draft" = "draught",
  "license" = "licence",
  "practice" = "practise",
  "gray" = "grey",
  "pajamas" = "pyjamas",
  "aluminum" = "aluminium",
  "cozy" = "cosy",
  "traveler" = "traveller",
  "canceled" = "cancelled",
  "fueling" = "fuelling",
  "enroll" = "enrol",
  "enrollment" = "enrolment",
  "aging" = "ageing",
  "smolder" = "smoulder",
  "liters" = "litres",
  "liter" = "litre",
  "meters" = "metres",
  "meters" = "metres",
  "kilometers" = "kilometres",
  "kilometer" = "kilometre",
  "maneuver" = "manoeuvre",
  "pediatric" = "paediatric",
  "pediatrics" = "paediatrics",
  "esthetic" = "aesthetic",
  "arbor" = "arbour",
  "sulfur" = "sulphur",
  "leukemia" = "leukaemia",
  "diarrhea" = "diarrhoea",
  "estrogen" = "oestrogen",
  "gynecology" = "gynaecology",
  "paleontology" = "palaeontology",
  "pedophile" = "paedophile",
  "pedophiles" = "paedophiles",
  "pedophilic" = "paedophilic"
)

# Function to normalise spellings
normalize_spelling <- function(text, dictionary) {
  for (us in names(dictionary)) {
    uk <- dictionary[[us]]
    text <- str_replace_all(text, fixed(us), uk)
  }
  return(text)
}

# Normalise spellings in the corpus
normalized_corpus <- lapply(corpuscop, normalize_spelling, dictionary = us_to_uk)

# Convert corpus to character vector
text_data <- sapply(normalized_corpus, as.character)

# Tokenise and preprocess the text data
tokens <- tokens(text_data, what = "word", remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("english"), "english", "like", "\\", "\"", "s", "m", "re", "e", "o", "t", "ive", "youve", "im", "youre", "us", "shes", "hed", "youll", "isnt", "havent", "wont", "whos", "wheres", "its", "weve", "shed", "arent", "hadnt", "wouldnt", "couldnt", "whats", "whys", "were", "theyve", "wasnt", "doesnt", "shant", "mustnt", "heres", "hows", "youre", "theyre", "id", "theyd", "werent", "dont", "shouldnt", "lets", "theres", "hes", "youd", "theyll", "hasnt", "didnt", "cant", "thats", "whens", "n", "c", "r", "d", "h", "l", "ll"))


# Tokenise and preprocess the text data; use this if the one above does not work
tokens <- tokens(text_data) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_remove(c("english", "like", "\\", "\"", "s", "m", "re", "e", "o", "t", "ive", "youve", "im", "youre", "us", "shes", "hed", "youll", "isnt", "havent", "wont", "whos", "wheres", "its", "weve", "shed", "arent", "hadnt", "wouldnt", "couldnt", "whats", "whys", "were", "theyve", "wasnt", "doesnt", "shant", "mustnt", "heres", "hows", "youre", "theyre", "id", "theyd", "werent", "dont", "shouldnt", "lets", "theres", "hes", "youd", "theyll", "hasnt", "didnt", "cant", "thats", "whens", "n", "c", "r", "d", "h", "l", "ll")) %>%
  tokens_remove_punct() %>%
  tokens_remove_numbers()

# Create DFM
dfm_matrix5 <- dfm(tokens)

# Remove empty documents
empty_docs <- which(rowSums(dfm_matrix5) == 0)
if (length(empty_docs) > 0) {
  dfm_matrix5 <- dfm_matrix5[-empty_docs, ]
  data.thesis.anon <- data.thesis.anon[-empty_docs, ]
}

# Convert DFM to matrix
dfm_matrix <- as.matrix(dfm_matrix5)

# Ensure matrix is of class dgCMatrix
dfm_matrix <- as(dfm_matrix, "dgCMatrix")

# Train LDA model using textmineR
set.seed(1234)  # You can choose any integer value
lda_model <- FitLdaModel(dtm = dfm_matrix, 
                         k = 35, 
                         iterations = 1000, 
                         burnin = 500, 
                         alpha = 0.2, 
                         beta = 0.05)

# Calculate coherence score of model
coherence <- CalcProbCoherence(phi = lda_model$phi, 
                               dtm = dfm_matrix, 
                               M = 5)
mean_coherence <- mean(coherence)
print(paste("Coherence Score:", mean_coherence))

# Extract top terms for each topic
top_terms <- GetTopTerms(phi = lda_model$phi, M = 30)
print(top_terms)

#topterms to excel file
write.csv(top_terms, file = "k35standard.csv")

# Extract document-topic probabilities
doc_topics <- as.data.frame(lda_model$theta)

# Contextualize with metadata
metadata <- data.frame(document = rownames(doc_topics), year = data.thesis.anon$year)
doc_topics <- doc_topics %>%
  mutate(document = rownames(doc_topics)) %>%
  left_join(metadata, by = "document")

#to find the most probable topic for each document
doc_topics$most_probable_topic <- apply(doc_topics[, -ncol(doc_topics)], 1, which.max)
head(doc_topics)

#CSV of doctopics
write.csv(doc_topics, file = "document topics.k35.csv")

# Function to find most relevant documents for a topic
most_relevant_docs <- function(topic_num, doc_topics, top_n = 10) {
  topic_col <- paste0("t_", topic_num)
  if (!topic_col %in% colnames(doc_topics)) {
    stop(paste("Column", topic_col, "not found in doc_topics"))
  }
  doc_topics %>%
    arrange(desc(!!sym(topic_col))) %>%
    slice(1:top_n) %>%
    select(document, !!sym(topic_col))
}

# Find top 10 documents for topic x (here it is topic 1)
top_docs_topic_1 <- most_relevant_docs(1, doc_topics, top_n = 10)
print(top_docs_topic_1)


library(dplyr)
library(tidyr)
library(readr)

#find top 10 docs for all topics
top_docs_all_topics <- bind_rows(lapply(1:35, function(topic_num) {
  most_relevant_docs(topic_num, doc_topics, top_n = 10)
}))

write.csv(top_docs_all_topics, "top_docs_all_topics.k35.csv")

# Function to find most relevant documents for a topic and include poems
# Add unique identifier to doc_topics
doc_topics$document_id <- 1:nrow(doc_topics)

# Add unique identifier to data.thesis.anon
data.thesis.anon$document_id <- 1:nrow(data.thesis.anon)

library(dplyr)
library(tidyr)
library(readr)

# Function to find most relevant documents for a topic and include poems
most_relevant_docs <- function(topic_num, doc_topics, data, top_n = 10) {
  topic_col <- paste0("t_", topic_num)
  if (!topic_col %in% colnames(doc_topics)) {
    stop(paste("Column", topic_col, "not found in doc_topics"))
  }
  
  top_docs <- doc_topics %>%
    arrange(desc(!!sym(topic_col))) %>%
    slice(1:top_n) %>%
    select(document_id, !!sym(topic_col))
  
  # Ensure the 'document_id' column exists in both data frames
  if (!"document_id" %in% colnames(data)) {
    stop("Column 'document_id' not found in data.thesis.anon")
  }
  
  top_docs_with_poems <- merge(top_docs, data, by = "document_id") %>%
    select(document_id, poems, !!sym(topic_col))
  
  return(top_docs_with_poems)
}

# Test the function with a single topic
top_poems_topic_1 <- most_relevant_docs(1, doc_topics, data.thesis.anon, top_n = 10)
print(top_poems_topic_1)

# Find top 10 poems for all topics
top_poems_all_topics <- bind_rows(lapply(1:35, function(topic_num) {
  most_relevant_docs(topic_num, doc_topics, data.thesis.anon, top_n = 10)
}))

# Inspect the results
head(top_poems_all_topics)

#CSV of doctopics
write.csv(top_poems_all_topics, file = "poems and documents topics.k35.csv")

# Assess statistical significance
perplexity(lda_model)
logLik(lda_model)

library("topicmodels")
#extract topic proportions
topic_proportions <- lda_model$theta

#trendanalysis based on year of publication 
library(dplyr)
topic_proportions_df <- as.data.frame(topic_proportions)
topic_proportions_df$year <- data.thesis.anon$year  # Assuming your data has a 'year' column

yearly_topic_proportions <- topic_proportions_df %>%
  group_by(year) %>%
  summarise(across(everything(), mean))

#visualise
library(ggplot2)
library(tidyr)
yearly_topic_proportions_long <- yearly_topic_proportions %>%
  pivot_longer(cols = -year, names_to = "topic", values_to = "proportion")

# Ensure year is numeric
yearly_topic_proportions_long$year <- as.numeric(as.character(yearly_topic_proportions_long$year))

ggplot(yearly_topic_proportions_long, aes(x = year, y = proportion, color = topic)) +
  geom_line() +
  labs(title = "Topic Trends Over Time", x = "Year", y = "Proportion")

#barchart
library(ggplot2)
yearly_topic_proportions_long$year <- as.numeric(as.character(yearly_topic_proportions_long$year))

ggplot(yearly_topic_proportions_long, aes(x = factor(year), y = proportion, fill = topic)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Topic Proportions Over Time", x = "Year", y = "Proportion") +
  theme_minimal()

#Check the Number of Rows: Verify that both data.thesis.anon and topic_proportions_df have the same number of rows.
nrow(data.thesis.anon)
nrow(topic_proportions_df)

#save trend analysis
write.csv(yearly_topic_proportions, file = "trendanalysis.lda.k35.csv", row.names = FALSE)

#trendanalysis, RELATIVE PREVALENCE 
# Check the length of topic_proportions
print(length(topic_proportions))
# Check the length of publication_date
print(length(data.thesis.anon$year))

# Assuming topic_proportions is a matrix with rows corresponding to documents
combined_data <- data.frame(publication_date = data.thesis.anon$year, topic_proportions)

# Check the structure of the combined data
str(combined_data)

# Aggregate topic proportions by publication date
topic_trends <- aggregate(. ~ publication_date, data = combined_data, FUN = mean)

# Rename the columns for clarity
colnames(topic_trends) <- c("PublicationDate", paste0("Topic", 1:ncol(topic_proportions)))

# Check the aggregated data
head(topic_trends)

# Plot the trend for each topic
for (i in 2:ncol(topic_trends)) {
  plot(topic_trends$PublicationDate, topic_trends[, i], type = "l", xlab = "Publication Date", ylab = paste("Topic", i-1, "Proportion"), main = paste("Trend of Topic", i-1))
}

# Plot multiple topics
# Assuming topic_proportions is a matrix with rows corresponding to documents
combined_data <- data.frame(publication_date = data.thesis.anon$year, topic_proportions)

# Check the structure of the combined data
str(combined_data)

# Aggregate topic proportions by publication date
topic_trends <- aggregate(. ~ publication_date, data = combined_data, FUN = mean)

# Rename the columns for clarity
colnames(topic_trends) <- c("PublicationDate", paste0("Topic", 1:ncol(topic_proportions)))

# Ensure PublicationDate is in Date format
topic_trends$PublicationDate <- as.Date(topic_trends$PublicationDate, format = "%Y")

# Reshape the data for plotting
long_data <- pivot_longer(topic_trends, cols = starts_with("Topic"), names_to = "Topic", values_to = "Proportion")

# Plot multiple topics
ggplot(long_data, aes(x = PublicationDate, y = Proportion, color = Topic)) +
  geom_line() +
  labs(title = "Trend Analysis of Topics Over Time", x = "Publication Date", y = "Topic Proportion") +
  theme_minimal()

# Save the data used for the plot to a CSV file
write.csv(long_data, "trend_analysis_plot.csv", row.names = FALSE)

#frequencies
install.packages("dplyr")
library(dplyr)

datafreq <- data.thesis.anon  # Adjust the separator if needed

year_counts <- datafreq %>%
  count(year)
print(year_counts)

write.csv(year_counts, file = "thesis.yearfreq.csv", row.names = FALSE)

