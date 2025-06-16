# SUMMARIES ANALYSIS

#--- 

# Libraries
library(tidyverse)
library(syuzhet)
library(sentimentr)
library(RSentiment)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readxl)
library(countrycode)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(kableExtra)
library(gt)
library(webshot2)

#---

# Load your data
data <- read_csv("file.csv")

#Inspect your data
str(data)
head(data)

#---

# Clean and prepare the 'summarry_en' column
data <- data %>%
  mutate(header_en = tolower(summary_en)) %>%  # Convert to lowercase
  mutate(header_en = gsub("[[:punct:]]", "", summary_en)) %>% #Remove punctuation
  mutate(header_en = gsub("[[:digit:]]", "", summary_en)) #Remove digits

# Extract month and year using string extraction directly from 'date' column
data <- data |>
  mutate(
    month = str_extract(date, "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"),
    year = str_extract(date, "\\d{4}"), # Extract 4-digit year
    month_year = paste(month, year)
  )

# Check: how many unique month_year entries are there?
length(unique(data$month_year))

# Display the first few rows to verify the new columns
head(data)

#---

# Differences between countries

# First, count observations per country
country_counts <- data %>%
  count(country_code, name = "n_articles") %>%
  arrange(desc(n_articles))

# Print the complete table
print(country_counts, n = Inf)

# --- Sentiment Analysis with Different Packages ---
# --- Sentiment Analysis using syuzhet ---
syuzhet_sentiment <- get_sentiment(data$summary_en, method = "syuzhet")
data$syuzhet_score <- syuzhet_sentiment

# --- Sentiment Analysis using sentimentr ---
data$sentimentr_score <- data$summary_en %>% 
  get_sentences() %>% 
  sentiment_by() %>% 
  pull(ave_sentiment)

# --- Sentiment Score by Country (Syuzhet) ---
country_sentiment_syuzhet <- data %>%
  group_by(country_code) %>%
  summarize(avg_sentiment = mean(syuzhet_score, na.rm = TRUE)) %>%
  arrange(avg_sentiment)  # Order by sentiment

# Convert country codes to country names
country_sentiment_syuzhet$country_name <- countrycode(
  sourcevar = country_sentiment_syuzhet$country_code,
  origin = "iso2c",  # Assuming your country codes are ISO 2-character codes
  destination = "country.name"
)

# Create the labels
labels = ifelse(is.na(country_sentiment_syuzhet$country_name), paste0(country_sentiment_syuzhet$country_code, ' - Missing Name'), country_sentiment_syuzhet$country_name)

# Create a more readable plot
ggplot(country_sentiment_syuzhet, aes(x = labels, y = avg_sentiment, fill = avg_sentiment)) +  # Use labels
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0, name = "Sentiment Score") +  # Diverging color palette
  labs(
    title = "Sentiment towards Digital Euro by Country (Syuzhet)",
    x = "Country",
    y = "Average Sentiment Score"
  ) +
  theme_minimal() +  # Choose a clean theme
  theme(
    axis.text.y = element_text(size = 8), #adjust size for labels
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    title = element_text(size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)))  # Add space to y-axis

# --- Sentiment Score by Country (Sentimentr) ---
country_sentiment_sentimentr <- data %>%
  group_by(country_code) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE)) %>%
  arrange(avg_sentiment)  # Order by sentiment

# Convert country codes to country names
country_sentiment_sentimentr$country_name <- countrycode(
  sourcevar = country_sentiment_sentimentr$country_code,
  origin = "iso2c",  # Assuming your country codes are ISO 2-character codes
  destination = "country.name"
)

# Create the labels
labels = ifelse(is.na(country_sentiment_sentimentr$country_name), paste0(country_sentiment_sentimentr$country_code, ' - Missing Name'), country_sentiment_sentimentr$country_name)

# Create a readable plot
ggplot(country_sentiment_sentimentr, aes(x = labels, y = avg_sentiment, fill = avg_sentiment)) +  # Use labels
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", midpoint = 0, name = "Sentiment Score") +  # Diverging color palette
  labs(
    title = "Sentiment towards Digital Euro by Country (Sentimentr)",
    x = "Country",
    y = "Average Sentiment Score"
  ) +
  theme_minimal() +  # Choose a clean theme
  theme(
    axis.text.y = element_text(size = 8), # Adjust size for labels
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    title = element_text(size = 14)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)))  # Add space to y-axis

# Filter out JP, RU, RS countries (because of the low number of observations)
data_filtered <- data %>%
  filter(!country_code %in% c("JP", "RU", "RS"))

# --- Sentiment Score by Country (Syuzhet) ---
country_sentiment_syuzhet <- data_filtered %>%
  group_by(country_code) %>%
  summarize(avg_sentiment = mean(syuzhet_score, na.rm = TRUE)) %>%
  arrange(avg_sentiment)

# Convert country codes to names
country_sentiment_syuzhet$country_name <- countrycode(
  country_sentiment_syuzhet$country_code,
  "iso2c", "country.name"
)

# Create labels with complete parentheses
labels <- ifelse(is.na(country_sentiment_syuzhet$country_name),
                 paste0(country_sentiment_syuzhet$country_code, ' - Missing Name'),
                 country_sentiment_syuzhet$country_name)

# Plot (final plot for the thesis)
ggplot(country_sentiment_syuzhet, 
       aes(x = reorder(labels, avg_sentiment), 
           y = avg_sentiment, 
           fill = avg_sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", 
                       midpoint = 0, name = "Sentiment Score") +
  labs(title = "a) syuzhet",
       subtitle = "Excluding JP, RU, RS",
       x = "Country",
       y = "Average Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12),
        title = element_text(size = 10),
        plot.subtitle = element_text(size = 10, color = "gray50")
  )

# --- Sentiment Score by Country (Sentimentr) ---
country_sentiment_sentimentr <- data_filtered %>%
  group_by(country_code) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE)) %>%
  arrange(avg_sentiment)

# Convert country codes to names
country_sentiment_sentimentr$country_name <- countrycode(
  country_sentiment_sentimentr$country_code,
  "iso2c", "country.name"
)

# Corrected labels creation with complete parentheses
labels <- ifelse(is.na(country_sentiment_sentimentr$country_name),
                 paste0(country_sentiment_sentimentr$country_code, ' - Missing Name'),
                 country_sentiment_sentimentr$country_name)

# Plot (final plot for the thesis)
ggplot(country_sentiment_sentimentr, 
       aes(x = reorder(labels, avg_sentiment), 
           y = avg_sentiment, 
           fill = avg_sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue", 
                       midpoint = 0, name = "Sentiment Score") +
  labs(title = "b) sentimentr",
       subtitle = "Excluding JP, RU, RS",
       x = "Country",
       y = "Average Sentiment Score") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12),
        title = element_text(size = 10),
        plot.subtitle = element_text(size = 10, color = "gray50"))

#---

# Evolution of sentiment by month

monthly_sentiment_syuzhet <- data %>%
  group_by(month_year) %>%
  summarize(avg_sentiment = mean(syuzhet_score, na.rm = TRUE)) %>%
  mutate(month_year = my(month_year))  # Convert "Jan 2023" to Date

monthly_sentiment_sentimentr <- data %>%
  group_by(month_year) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE)) %>%
  mutate(month_year = my(month_year))  # Convert "Jan 2023" to Date

monthly_sentiment_syuzhet <- monthly_sentiment_syuzhet %>% drop_na(month_year)
monthly_sentiment_sentimentr <- monthly_sentiment_sentimentr %>% drop_na(month_year)

monthly_sentiment_syuzhet <- monthly_sentiment_syuzhet %>% arrange(month_year)
monthly_sentiment_sentimentr <- monthly_sentiment_sentimentr %>% arrange(month_year)

ggplot(monthly_sentiment_syuzhet, aes(x = month_year, y = avg_sentiment, group = 1)) +
  geom_line(color = "blue", size = 1) +  # Add color for visibility
  geom_point(color = "red", size = 2) +  # Add points to check data presence
  labs(title = "Evolution of Sentiment by Month (Syuzhet)", x = "Month", y = "Average Sentiment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(monthly_sentiment_sentimentr, aes(x = month_year, y = avg_sentiment, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Evolution of Sentiment by Month (Sentimentr)", x = "Month", y = "Average Sentiment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---

# Analysis by sources

# --- Top Positive, Negative, and Neutral Sources (using sentimentr, you can adapt for others) ---
source_sentiment <- data %>%
  group_by(sitename) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE)) %>%
  arrange(avg_sentiment)

top_negative_sources <- head(source_sentiment, 5)
top_positive_sources <- tail(source_sentiment, 5)

# Identify neutral sources (close to zero)
neutral_threshold <- 0.05 # Define a threshold for "neutral"
neutral_sources <- source_sentiment %>%
  filter(abs(avg_sentiment) < neutral_threshold)

#Print the values
print("Top Negative Sources:")
print(top_negative_sources)
print("Top Positive Sources:")
print(top_positive_sources)
print("Neutral Sources:")
print(neutral_sources)

# Count articles per source
source_counts <- data %>%
  count(sitename, name = "n_articles") %>%
  arrange(desc(n_articles))

# View top sources by article count
print(head(source_counts, 20), n = 20)

final_sources <- data.frame(
  sitename = c(
    # Top financial/economics-focused (high article count)
    "Financial Times", "Bloomberg", "Reuters", "Wall Street Journal",
    "Les Echos", "Handelsblatt", "Frankfurter Allgemeine Zeitung",
    "IL SOLE 24 ORE", "Corriere della Sera", "Expansión",
    
    # Major general news with strong business sections (medium count)
    "The Guardian", "New York Times", "Le Monde", "El País",
    "Süddeutsche Zeitung", "Der Spiegel", "La Stampa",
    
    # Important EU-focused outlets
    "Politico Europe", "EUobserver", "Euronews",
    
    # Crypto/Fintech specialists
    "CoinDesk", "Finextra",
    
    # National business leaders (high count in dataset)
    "Milano Finanza", "Börsen-Zeitung", "Cinco Días",
    "La Vanguardia", "L'Agefi Quotidien", "El Economista",
    
    # High-quality regional sources
    "Kathimerini", "Gazeta Wyborcza", "Dagens Industri"
  )
)

# Get article counts for these sources
final_counts <- data %>%
  filter(sitename %in% final_sources$sitename) %>%
  count(sitename, name = "n_articles") %>%
  arrange(desc(n_articles)) %>%
  left_join(final_sources, by = "sitename")  # Preserve original order

final_analysis <- data %>%
  filter(sitename %in% final_sources$sitename) %>%
  group_by(sitename) %>%
  summarize(
    n_articles = n(),
    avg_sentiment = mean(sentimentr_score, na.rm = TRUE),
    sentiment_consistency = sd(sentimentr_score, na.rm = TRUE)
  ) %>%
  arrange(desc(n_articles))

# Visualize
ggplot(final_analysis, aes(x = reorder(sitename, n_articles), 
                           y = n_articles, 
                           fill = avg_sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "steelblue",
                       midpoint = 0, name = "Sentiment") +
  labs(title = "Top 30 Sources: Article Volume and Sentiment",
       x = "",
       y = "Number of Articles") +
  theme_minimal()

# Define left/right sources (adjust as needed)
left_wing <- c("The Guardian", "Libération", "Le Monde", "Publico", 
               "El País", "La Repubblica", "Der Spiegel", 
               "Süddeutsche Zeitung", "L'Humanité", "Mediapart",
               "Il Fatto Quotidiano", "The Irish Times")

right_wing <- c("Die Welt", "Frankfurter Allgemeine Zeitung", "Le Figaro", 
                "Il Giornale", "La Razón", "Bild", "Daily Mail", 
                "The Telegraph", "Okdiario", "Corriere della Sera",
                "Expresso", "Jornal de Negócios")

# Filter data for these sources
filtered_data <- data %>%
  filter(sitename %in% c(left_wing, right_wing)) %>%
  mutate(ideology = if_else(sitename %in% left_wing, "Left", "Right"))

# Calculate avg sentiment per outlet + ideology
sentiment_summary <- filtered_data %>%
  group_by(sitename, ideology) %>%
  summarize(
    avg_sentiment = mean(sentimentr_score, na.rm = TRUE),
    n_articles = n()
  ) %>%
  ungroup()

# Plot: sentiment distribution by ideology
ggplot(sentiment_summary, aes(x = ideology, y = avg_sentiment, fill = ideology)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(size = n_articles), width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = c("Left" = "#3498db", "Right" = "#e74c3c")) +
  labs(
    title = "Sentiment Comparison: Left vs. Right-Wing Outlets",
    x = "Ideology",
    y = "Average Sentiment Score",
    caption = "Point size reflects number of articles per outlet"
  ) +
  theme_minimal()

# Top positive/negative within each group
ideology_sentiment <- sentiment_summary %>%
  group_by(ideology) %>%
  slice_max(avg_sentiment, n = 3) %>%  # Top 3 positive
  bind_rows(
    sentiment_summary %>%
      group_by(ideology) %>%
      slice_min(avg_sentiment, n = 3)   # Top 3 negative
  )

# Print results
print(ideology_sentiment, n = Inf)

# Another plot

### 3 categories

# Define left/right/tabloid sources based on previous categorization
left_wing <- c(
  "The Guardian", "El Pais", "Le Monde", "Süddeutsche Zeitung",
  "La Repubblica", "Libération", "Die Zeit", "Publico",
  "Il Fatto Quotidiano", "L'Humanité"
)

right_wing <- c(
  "Frankfurter Allgemeine Zeitung", "Le Figaro", "ABC", "La Razón",
  "Die Welt", "Il Giornale", "The Telegraph", "l'Opinion",
  "El Mundo", "Il Foglio"
)

tabloid_sources <- c(
  "Bild", "Daily Mail", "De Telegraaf", "Kronen-Zeitung",
  "Correio da Manhã", "Okdiario", "20minutos", "Le Parisien",
  "Libero", "Het Laatste Nieuws"
)

# Filter data for these three categories of sources
filtered_data <- data %>%
  filter(sitename %in% c(left_wing, right_wing, tabloid_sources)) %>%
  # Mutate to assign ideology based on the three categories
  mutate(ideology = case_when(
    sitename %in% left_wing ~ "Left",
    sitename %in% right_wing ~ "Right",
    sitename %in% tabloid_sources ~ "Tabloid",
    TRUE ~ NA_character_ # Should not happen if filter is correctly applied
  ))

# Calculate avg sentiment per outlet + ideology
sentiment_summary <- filtered_data %>%
  group_by(sitename, ideology) %>%
  summarize(
    avg_sentiment = mean(sentimentr_score, na.rm = TRUE),
    n_articles = n()
  ) %>%
  ungroup()

# Plot: Sentiment distribution by ideology for 3 categories
ggplot(sentiment_summary, aes(x = ideology, y = avg_sentiment, fill = ideology)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(aes(size = n_articles), width = 0.2, alpha = 0.5) +
  # Adjust colors for three categories
  scale_fill_manual(values = c("Left" = "#e74c3c", "Right" = "#3498db", "Tabloid" = "#2ecc71")) + # Added a new color for Tabloid
  labs(x = "Ideology/Media Type",
       y = "Average Sentiment Score",
       caption = "Point size reflects number of articles per outlet"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#---

# --- Sentiment Score within Eurozone vs. Non-Eurozone vs. Non-EU countries ---

# Define country lists based on your specifications
eurozone_countries <- c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES")
non_eurozone_eu_countries <- c("BG", "CZ", "DK", "HR", "HU", "PL", "RO", "SE")
non_eu_countries <- c("GB", "CH", "RS", "RU", "US", "CA", "JP")

# Create the new category column
data <- data %>%
  mutate(
    economic_group = case_when(
      country_code %in% eurozone_countries ~ "Eurozone",
      country_code %in% non_eurozone_eu_countries ~ "Non-Eurozone EU",
      country_code %in% non_eu_countries ~ "Non-EU",
      TRUE ~ "Other/Unknown"  # Catch-all for any countries not in the lists
    )
  )

# Print Unique Categories
print(unique(data$economic_group))

# Check how many are assigned to other unknown
print(table(data$economic_group))

# Identify country codes in the "Other/Unknown" category
other_unknown_countries <- data %>%
  filter(economic_group == "Other/Unknown")

print(unique(other_unknown_countries$country_code))

# Print to view all countries within the data
print(data %>% count(country_code, sort = TRUE))

# Example: Sentiment by economic group (using sentimentr)
group_sentiment_sentimentr <- data %>%
  group_by(economic_group) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE))

ggplot(group_sentiment_sentimentr, aes(x = economic_group, y = avg_sentiment)) +
  geom_bar(stat = "identity", fill =  "#6699CC") +
  labs(title = "Average Sentiment by Economic Group (Sentimentr)", x = "Economic Group", y = "Average Sentiment")

#---

# Word clouds

# World cloud - part 1 (containg words 'digital' and 'euro') 

# Create a corpus from the header_en column
corpus <- Corpus(VectorSource(data$summary_en))

# Clean the corpus (remove punctuation, lowercase, remove numbers, remove stopwords)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Or your relevant language

# Create a term-document matrix
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

# Generate the word cloud
set.seed(123) # For reproducibility
wordcloud(words = df$word, freq = df$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.3,
          colors = brewer.pal(8, "Dark2"))

# Word clouds - part 2 (without words 'digital' and 'euro')

# Create a corpus from the header_en column
corpus <- Corpus(VectorSource(data$header_en))

# Define custom words to remove (stopwords + digital/euro)
custom_stopwords <- c(stopwords("english"), "digital", "euro")

# Clean the corpus
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, custom_stopwords) %>%  # Using our custom list
  tm_map(stripWhitespace)

# Create a term-document matrix
dtm <- TermDocumentMatrix(corpus)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(word = names(words), freq = words)

# Generate the word cloud with better formatting
set.seed(123)
wordcloud(words = df$word, 
          freq = df$freq, 
          min.freq = 2,           # Only show words appearing at least twice
          max.words = 150,        # Increase from 200 to 150 for better readability
          random.order = FALSE,   # Most frequent words in center
          rot.per = 0.2,          # Fewer rotated words (20%)
          scale = c(4, 0.5),      # Bigger size difference between large/small words
          colors = brewer.pal(9, "YlOrRd"))  # More vibrant color palette

# For a more polished version with word frequency legend:
library(wordcloud2)
wordcloud2(df %>% filter(freq >= 2), 
           size = 0.8,
           color = "random-dark",
           backgroundColor = "white")

#---

# Analysis of specific keywords

# Part 1

### cryptocurrencies

# Define a keyword of interest
keyword <- "cryptocurrencies"  

# Filter the data to include only headlines containing the keyword
keyword_data <- data %>%
  filter(grepl(keyword, header_en, ignore.case = TRUE))

# Calculate the average sentiment score for the keyword
keyword_sentiment <- mean(keyword_data$sentimentr_score, na.rm = TRUE)

print(paste("Average sentiment score for '", keyword, "':", keyword_sentiment))

### ECB

# Define a keyword of interest
keyword <- "ecb"  

# Filter the data to include only headlines containing the keyword
keyword_data <- data %>%
  filter(grepl(keyword, header_en, ignore.case = TRUE))

# Calculate the average sentiment score for the keyword
keyword_sentiment <- mean(keyword_data$sentimentr_score, na.rm = TRUE)

print(paste("Average sentiment score for '", keyword, "':", keyword_sentiment))

### digital euro

# Define a keyword of interest
keyword <- "digital euro"  

# Filter the data to include only headlines containing the keyword
keyword_data <- data %>%
  filter(grepl(keyword, header_en, ignore.case = TRUE))

# Calculate the average sentiment score for the keyword
keyword_sentiment <- mean(keyword_data$sentimentr_score, na.rm = TRUE)

print(paste("Average sentiment score for '", keyword, "':", keyword_sentiment))

### Lagarde

# Define a keyword of interest
keyword <- "lagarde"  # Replace with your keyword - it can be something from the word cloud maybe?

# Filter the data to include only headlines containing the keyword
keyword_data <- data %>%
  filter(grepl(keyword, header_en, ignore.case = TRUE))

# Calculate the average sentiment score for the keyword
keyword_sentiment <- mean(keyword_data$sentimentr_score, na.rm = TRUE)

print(paste("Average sentiment score for '", keyword, "':", keyword_sentiment))

### ECB lexicon 

# Create a lexicon for the ECB (European Central Bank)
ecb_terms <- c("ecb", "european central bank", "lagarde", "eurosystem")

# Filter headlines mentioning the ECB
ecb_data <- data %>%
  filter(grepl(paste(ecb_terms, collapse = "|"), header_en, ignore.case = TRUE))

# Calculate sentiment towards the ECB
ecb_sentiment <- mean(ecb_data$sentimentr_score, na.rm = TRUE)

print(paste("Average sentiment score towards the ECB:", ecb_sentiment))

# Part 2 (2 different versions of tables)

keywords <- c(
  # Policy/Institutions
  "ECB", "European Central Bank", "Lagarde", "Eurosystem", 
  "digital euro", "CBDC", "central bank digital currency",
  
  # Technology
  "blockchain", "DLT",
  
  # Financial Impact
  "banking system", "financial stability", "monetary policy", 
  "interest rates", "payment system",
  
  # Cryptocurrencies
  "cryptocurrency", "Bitcoin", "Ethereum", "stablecoin",
  
  # Public Perception
  "privacy", "adoption", "cashless"
)

# Function to analyze keyword sentiment
analyze_keyword <- function(keyword, data) {
  keyword_data <- data %>%
    filter(grepl(keyword, header_en, ignore.case = TRUE))
  
  if(nrow(keyword_data) > 0) {
    data.frame(
      Keyword = keyword,
      Articles = nrow(keyword_data),
      Avg_Sentiment = round(mean(keyword_data$sentimentr_score, na.rm = TRUE), 3),
      Positive = sum(keyword_data$sentimentr_score > 0.1, na.rm = TRUE),
      Neutral = sum(abs(keyword_data$sentimentr_score) <= 0.1, na.rm = TRUE),
      Negative = sum(keyword_data$sentimentr_score < -0.1, na.rm = TRUE)
    )
  }
}

# Analyze all keywords
results <- map_df(keywords, analyze_keyword, data = data) %>%
  arrange(desc(Articles))

results %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 2, "Sentiment Score" = 1, "Article Count by Sentiment" = 3)) %>%
  column_spec(3, color = "white", background = spec_color(results$Avg_Sentiment, end = 0.6)) %>%
  row_spec(which(results$Avg_Sentiment > 0.1), background = "#E6F3E6") %>%  
  row_spec(which(results$Avg_Sentiment < -0.1), background = "#F3E6E6")   

# Function to analyze keywords
analyze_keyword <- function(keyword) {
  keyword_data <- data %>%
    filter(grepl(keyword, header_en, ignore.case = TRUE))
  
  n_headlines <- nrow(keyword_data)
  avg_sentiment <- mean(keyword_data$sentimentr_score, na.rm = TRUE)
  
  sentiment_counts <- keyword_data %>%
    mutate(sentiment_category = case_when(
      sentimentr_score > 0.05 ~ "Positive",
      sentimentr_score < -0.05 ~ "Negative",
      TRUE ~ "Neutral"
    )) %>%
    count(sentiment_category) %>%
    pivot_wider(names_from = sentiment_category, values_from = n, values_fill = 0)
  
  return(data.frame(
    Keyword = keyword,
    Num_Headlines = n_headlines,
    Avg_Sentiment = round(avg_sentiment, 3),
    Positive = ifelse("Positive" %in% names(sentiment_counts), sentiment_counts$Positive[1], 0),
    Neutral = ifelse("Neutral" %in% names(sentiment_counts), sentiment_counts$Neutral[1], 0),
    Negative = ifelse("Negative" %in% names(sentiment_counts), sentiment_counts$Negative[1], 0)
  ))
}

# Apply function to all keywords and create table
keyword_analysis_table <- do.call(rbind, lapply(keywords, analyze_keyword))

# Convert to gt table
keyword_table <- keyword_analysis_table %>%
  gt() %>%
  tab_header(title = "Sentiment Analysis of Keywords") %>%
  fmt_number(columns = c(Avg_Sentiment), decimals = 3) %>%
  cols_label(
    Keyword = "Keyword",
    Num_Headlines = "Headlines",
    Avg_Sentiment = "Avg Sentiment",
    Positive = "Positive",
    Neutral = "Neutral",
    Negative = "Negative"
  ) %>%
  tab_options(
    table.font.size = px(14),
    column_labels.font.weight = "bold",
    heading.align = "center"
  )

# Save as an image
gtsave(keyword_table, "keyword_sentiment_analysis_summaries.png")

#---

# Comparison between media types

media_counts <- data %>%
  count(media_type, name = "n_observations") %>%
  arrange(desc(n_observations))

# Using column named 'media_type'
media_sentiment <- data %>%
  filter(media_type != "SUMMARY") %>%
  group_by(media_type) %>%
  summarize(avg_sentiment = mean(sentimentr_score, na.rm = TRUE))

# Perform a t-test to compare sentiment between two media types
# For example, comparing "PRINT" and "WEB"
print_web_data <- data %>%
  filter(media_type %in% c("PRINT", "WEB"))

t.test(sentimentr_score ~ media_type, data = print_web_data)

# Visualize the sentiment across different media types
ggplot(media_sentiment, aes(x = media_type, y = avg_sentiment)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Sentiment by Media Type", x = "Media Type", y = "Average Sentiment")

#---

# Emotions analysis

# Get sentiment scores using the "nrc" method
emotion_scores <- get_nrc_sentiment(data$summary_en)

# Combine the emotion scores with your original data
data <- cbind(data, emotion_scores)

# Calculate the total count for each emotion
emotion_totals <- colSums(emotion_scores)

# Create a data frame for plotting
emotion_df <- data.frame(emotion = names(emotion_totals), count = emotion_totals)

# Plot the emotion counts
ggplot(emotion_df, aes(x = emotion, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Emotion Distribution in News Headlines", x = "Emotion", y = "Count") +
  theme_minimal()
