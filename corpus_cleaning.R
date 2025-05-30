library(gutenbergr)
library(tidyverse)
library(tm)
library(tidytext)

#download books
df <- gutenberg_download(
  c(7142, 1946, 13549, 10657, 132, 46976, 13529),
  mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg",
  meta_fields = c("author","title")
)

author_map <- c(
  "Sunzi, active 6th century B.C." = "Sun Tzu",
  "Clausewitz, Carl von" = "Clausewitz",
  "Thucydides" = "Thucydides",
  "Caesar, Julius" = "Caesar",
  "Mahan, A. T. (Alfred Thayer)" = "Mahan",
  "Jomini, Antoine Henri, baron de" = "Jomini",
  "Arrian" = "Arrian" 
)

year_map <- c(
  "Sun Tzu" = -500,
  "Thucydides" = -400,
  "Caesar" = -50,
  "Clausewitz" = 1832,
  "Jomini" = 1838,
  "Mahan" = 1890,
  "Arrian" = 130
)

century_map <- c(
  "Sun Tzu" = "6th c. BCE",
  "Thucydides" = "5th c. BCE",
  "Caesar" = "1st c. BCE",
  "Clausewitz" = "19th c. CE",
  "Jomini" = "19th c. CE",
  "Mahan" = "19th c. CE",
  "Arrian" = "2nd c. CE"
)


#clean text
df_clean <- df |>
  mutate(
    text = trimws(text),
    text = str_to_lower(text), 
    text = str_replace_all(text, "[[:punct:]]", ""),
    text = str_replace_all(text, "[[:digit:]]", ""),
    author = recode(author, !!!author_map),
    year = year_map[author],
    century = century_map[author]
  ) 

#tokenize into words
tokens_words <- df_clean |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

write.csv(tokens_words, "data/tokens_words_clean.csv")

#create dataframe with text chunks for topic modelling
#each chunk is five lines of text
df_chunks <- df_clean |>
  group_by(author, title) |>
  mutate(line = row_number()) |>
  group_by(author, title, chunk = (line - 1) %/% 5) |>
  summarise(text = paste(text, collapse = " "), .groups= "drop") |>
  mutate(
    year = year_map[author],
    century = century_map[author]
  )
  
write.csv(df_chunks, "data/text_chunks.csv")
