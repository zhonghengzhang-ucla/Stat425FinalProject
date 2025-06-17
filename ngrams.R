library(tidyverse)
library(tm)
library(tidytext)
library(igraph)
library(ggraph)

books <- read_csv("data/book_by_chapter.csv")

jomini <- books %>% filter(author == "Jomini")
jomini$text[7] <- strsplit(jomini$text[7], "")[[1]][
  -c(unlist(gregexpr("_Different Formations", jomini$text[7])):
       (unlist(gregexpr("Note.--In all these", jomini$text[7])) - 1))
] %>% paste(collapse = "")
jomini <- jomini %>% 
  mutate(text = str_remove_all(text, "FOOTNOTES:|\\[Illustration[^\\]]*\\]"))

books <- rbind(book %>% filter(author == "Clausewitz"), jomini)

book_bigrams <- books %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigram_count <- book_bigrams %>% group_by(author) %>% count(bigram)
bigram_count <- bigram_count %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_count <- bigram_count %>% 
  anti_join(stop_words, by = join_by(word1 == word)) %>% 
  anti_join(stop_words, by = join_by(word2 == word)) %>% 
  mutate(bigram = paste(word1, word2))

bigram_tf_idf <- bigram_count %>% bind_tf_idf(bigram, author, n)

bigram_tf_idf %>% group_by(author) %>% slice_max(tf, n = 10)  %>% 
  ggplot(aes(reorder_within(bigram, tf, author), tf, fill = author)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ author, scales = "free_y") +
  coord_flip() + scale_x_reordered()

bigram_tf_idf %>% group_by(author) %>% slice_max(tf_idf, n = 10)  %>% 
  ggplot(aes(reorder_within(bigram, tf_idf, author), tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ author, scales = "free_y") +
  coord_flip() + scale_x_reordered()

most_frequent_bigrams <- bigram_count %>% group_by(author) %>% 
  slice_max(n, n = 30) %>% ungroup()

set.seed(425)
most_frequent_bigrams %>% filter(author == "Clausewitz") %>% 
  select(word1, word2) %>% graph_from_data_frame() %>% 
  ggraph(layout = "fr") + geom_edge_link() + geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1)

set.seed(425)
most_frequent_bigrams %>% filter(author == "Jomini") %>% 
  select(word1, word2) %>% graph_from_data_frame() %>% 
  ggraph(layout = "fr") + geom_edge_link() + geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1)
