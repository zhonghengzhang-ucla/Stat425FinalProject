library(gutenbergr)
library(tidyverse)
library(tm)
library(tidytext)

tokens_words <- read.csv("data/tokens_words_clean.csv")

#word counts by grouped author
book_words <- tokens_words |>
  count(author, word)

#total words per author
total_words <- book_words |>
  group_by(author) |>
  summarise(total_words = sum(n))

#join together and add term frequency
book_words <- book_words |>
  left_join(total_words, by="author")

#add tf-idf
books_tf_idf <- book_words |>
  bind_tf_idf(word, author, n)

#top 10 words by term frequency per author
#initially top words for each author were numbers
top_words <- books_tf_idf|>
  group_by(author) |>
  top_n(10, wt = tf) |>
  ungroup()

#plot top 10 words by term frequency per author
top_words_plot <- top_words |>
  ggplot(aes(x = reorder(word, tf), y = tf, fill = author)) +
           geom_col(show.legend = FALSE) + 
           facet_wrap(~ author, scales = "free_y") +
           coord_flip() +
           labs(
             title = "Top 10 Most Frequent Words by Author",
             y = "Term Frequency", 
             x = NULL)

print(top_words_plot)           

#top 10 words by idf per author
#initially top words for each author were numbers
top_words_idf <- books_tf_idf |>
  group_by(author) |>
  top_n(10, wt = tf_idf) |>
  ungroup()

#plot top 10 words by term frequency per author
top_words_idf_plot <- top_words_idf |>
  ggplot(aes(x = reorder(word, idf), y = idf, fill = author)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ author, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top 10 Words by TF-IDF by Author",
    y = "TF-IDF", 
    x = NULL)

print(top_words_idf_plot)

