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

#clean text
df_clean <- df |>
  mutate(
    text = trimws(text),
    text = str_to_lower(text), 
    text = str_replace_all(text, "[[:punct:]]", ""),
    text = str_replace_all(text, "[[:digit:]]", "")
  )

#tokenize into words
tokens_words <- df_clean |>
  unnest_tokens(word, text) |>
  anti_join(stop_words, by = "word")

write.csv(tokens_words, "data/tokens_words_clean.csv")

