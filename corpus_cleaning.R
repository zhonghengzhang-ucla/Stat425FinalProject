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
  "Arrian" = "Arrian"  # Not part of your project
)

#clean text
df_clean <- df |>
  mutate(
    text = trimws(text),
    text = str_to_lower(text), 
    text = str_replace_all(text, "[[:punct:]]", ""),
    text = str_replace_all(text, "[[:digit:]]", ""),
    author = recode(author, !!!author_map)
  ) 
  

#tokenize into words
tokens_words <- df_clean |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

write.csv(tokens_words, "data/tokens_words_clean.csv")

