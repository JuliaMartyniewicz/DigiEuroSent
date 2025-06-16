# DATA CLEANING

#---

# Libraries
library(dplyr)
library(writexl)

#---

# Data loading
data_raw <- read.csv("file.csv", sep = ",")

#---

# Check the categories in these variables
unique(data_raw$media_type)
unique(data_raw$country_code)
unique(data_raw$language)

# "SUMMARY" category from 'media_type', empty strings in 'country_code', 'language' must be excluded

# CHECK FOR EMPTY STRINGS
# Check for empty strings in all columns
empty_counts <- sapply(data_raw, function(x) sum(x == ""))

# Print the count of empty strings for each column
print(empty_counts)

# Checking for how many SUMMARY in 'media_type'

# Count the frequency of each category in 'media_type'
media_type_counts <- data_raw %>%
  count(media_type)

# Print the counts
print(media_type_counts)

#---
  
# Removing empty headers - rows where 'header_en' is an empty string
data_cleaned1 <- data_raw %>%
  filter(header_en != "")

empty_counts <- sapply(data_cleaned1, function(x) sum(x == ""))

# Print the count of empty strings for each column
print(empty_counts)

#---

# Checking if it is possible to put there the country_code

# Filter rows where 'country_code' is an empty string
empty_country_code <- data_raw %>%
  filter(country_code == "")

# View the observations with empty 'country_code'
print(empty_country_code)

# These links cannot be accessed so they will be deleted
data_cleaned2 <- data_cleaned1 |>
  filter(country_code != "")

empty_counts <- sapply(data_cleaned2, function(x) sum(x == ""))

# Print the count of empty strings for each column
print(empty_counts)

#--- 

# Deleting summaries (n=8)
unique(data_cleaned2$media_type)

summary_media_type <- data_cleaned2 |>
  filter(media_type == "SUMMARY")

print(summary_media_type)

data_cleaned3 <- data_cleaned2 |>
  filter(media_type != "SUMMARY")

#--- 

empty_counts <- sapply(data_cleaned3, function(x) sum(x == ""))

# Print the count of empty strings for each column (to make sure that the data cleaning process is correct)
print(empty_counts)

nrow(data_cleaned3)

#---

# SHORT HEADERS

# Remove empty headers
data_cleaned3 <- data_cleaned3 %>%
  filter(header_en != "" & header_en != "-")

# Count words in headers 
data_cleaned3 <- data_cleaned3 %>%
  mutate(
    header_word_count = sapply(strsplit(trimws(gsub("[[:punct:]]", "", header_en)), "\\s+"), 
                               function(x) length(x[x != ""]))
  )

# Create frequency table
word_count_table <- data_cleaned3 %>%
  count(header_word_count) %>%
  arrange(header_word_count) %>%
  rename(`Words in Header` = header_word_count,
         `Number of Articles` = n)

#--- 

# Removing observations with 1 word and 98 and 104 words in 'header_en'

# Remove observations with 1, 98, or 104 word headers
data_cleaned_filtered <- data_cleaned3 %>%
  filter(!header_word_count %in% c(1, 98, 104))

# Verify the removal
removed_counts <- data_cleaned3 %>%
  filter(header_word_count %in% c(1, 98, 104)) %>%
  count(header_word_count)

cat("Removed the following observations:\n")
print(removed_counts)

# Check the new distribution
new_word_count_table <- data_cleaned_filtered %>%
  count(header_word_count) %>%
  arrange(header_word_count) %>%
  rename(`Words in Header` = header_word_count,
         `Number of Articles` = n)

#---

# Cleaning of 'summary_en'

# Count the number of rows with empty 'summary_en'
num_empty <- data_raw %>%
  summarise(empty_count = sum(summary_en == ""))

# Print the count
print(num_empty)

data_summaries <- data_raw |>
  filter(summary_en != "")

num_empty <- data_summaries %>%
  summarise(empty_count = sum(summary_en == ""))
print(num_empty)

nrow(data_summaries)

empty_counts <- sapply(data_summaries, function(x) sum(x == ""))

# Print the count of empty strings for each column (to make sure that the data cleaning process is correct)
print(empty_counts)

data_summaries <- data_summaries |>
  filter(country_code != "")

nrow(data_summaries)

# Some "-" in 'summary_en'
data_summaries <- data_summaries |>
  filter(summary_en != "-")

nrow(data_summaries)

# Verify empty counts
empty_counts <- sapply(data_summaries, function(x) sum(x == ""))
print("Empty counts after initial cleaning:")
print(empty_counts)

# Add word count column
data_summaries <- data_summaries %>%
  mutate(
    summary_word_count = sapply(strsplit(trimws(gsub("[[:punct:]]", "", summary_en)), "\\s+"), 
                                function(x) length(x[x != ""]))
  )

# Filter for minimum 30 words in 'summary_en' - keep only the summaries with at least 30 words
data_summaries <- data_summaries %>%
  filter(summary_word_count >= 30)

# Final report
cat("\nFinal Summary Statistics:\n")
cat("Original summaries:", nrow(data_raw), "\n")
cat("After basic cleaning:", nrow(data_summaries) + num_empty$empty_count, "\n")
cat("After 30-word filter:", nrow(data_summaries), "\n")
cat("Percentage kept:", round(nrow(data_summaries)/nrow(data_raw)*100, 1), "%\n")

# Word count distribution
summary_stats <- summary(data_summaries$summary_word_count)
cat("\nWord count distribution in remaining summaries:\n")
print(summary_stats)

# Visual confirmation
library(ggplot2)
ggplot(data_summaries, aes(x = summary_word_count)) +
  geom_histogram(binwidth = 5, fill = "steelblue") +
  geom_vline(xintercept = 30, color = "red", linetype = "dashed") +
  labs(title = "Summary Length Distribution (Minimum 30 Words)",
       x = "Word Count", y = "Number of Summaries") +
  theme_minimal()

#---

# Deleting columns that are not necessary (were created for cata cleaning process)
data_headers_cleaned <- data_cleaned_filtered |>
  select(-header_word_count)

data_summaries_cleaned <- data_summaries %>% 
  select(-summary_word_count)

#---           

# Saving data in .csv and .xlsx format

write.csv(data_headers_cleaned, file = "cleaned_data_headers.csv", row.names = FALSE)
write.csv(data_summaries_cleaned, file = "cleaned_data_summaries.csv", row.names = FALSE)

write_xlsx(data_headers_cleaned, path = "cleaned_data_headers.xlsx")
write_xlsx(data_summaries_cleaned, path = "cleaned_data_summaries.xlsx")

#---
