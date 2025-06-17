library(tidyverse)
library(tm)
library(tidytext)
library(wordcloud)

tokens_words <- read_csv("data/tokens_words_clean.csv") %>% 
  filter(!(word %in% c("footnotes", "fig", "footnote"))) %>% 
  mutate(word = stemDocument(word)) %>% 
  mutate(word = case_when(
    word == "citi" ~ "city",
    word == "armi" ~ "army",
    word == "alli" ~ "ally",
    word == "enemy’" ~ "enemy",
    word == "enemi" ~ "enemy",
    word == "cavalri" ~ "cavalry",
    word == "forc" ~ "force",
    word == "object" ~ "objective",
    word == "battl" ~ "battle",
    word == "posit" ~ "position",
    word == "natur" ~ "nature",
    word == "franc" ~ "france",
    word == "victori" ~ "victory",
    word == "spi" ~ "spy",
    word == "advantag" ~ "advantage",
    word == "athen" ~ "athens",
    word == "oper" ~ "operation",
    word == "alexand" ~ "alexander",
    word == "alexander’" ~ "alexander",
    word == "buonapart" ~ "buonaparte",
    word == "theatr" ~ "theatre",
    word == "theori" ~ "theory",
    word == "strategi" ~ "strategy",
    word == "organis" ~ "organisation",
    word == "maneuv" ~ "maneuver",
    word == "strateg" ~ "strategic",
    word == "artilleri" ~ "artillery",
    word == "buonapart" ~ "buonaparte",
    word == "manœuver" ~ "manœuver",
    word == "hemmedin" ~ "hemmed-in",
    word == "sunni" ~ "sunny",
    word == "ounc" ~ "ounce",
    word == "contenti" ~ "contentious",
    word == "shuaijan" ~ "shuai-jan",
    word == "argiv" ~ "argive",
    word == "hellen" ~ "hellenes",
    word == "alcibiad" ~ "alcibiades",
    TRUE ~ word
  ))

#word counts by grouped author
book_words <- tokens_words %>%
  count(author, word)

#total words per author
total_words <- book_words %>%
  group_by(author) %>%
  summarise(total_words = sum(n))

#join together
book_words <- book_words %>%
  left_join(total_words, by="author")

#token-type ratio per author
ttr_by_author <- book_words %>%
  group_by(author) %>%
  summarise(
    total_tokens = sum(n), 
    unique_types = n_distinct(word), #types = words
    ttr = unique_types/total_tokens
  )

#fairly similar lexical variety, Sun Tzu is an outlier
#plot ttr by author
ttr_plot <- ttr_by_author %>%
  ggplot(aes(x = reorder(author, ttr), y = ttr, fill = author)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  labs(
    title = "Type-Token Ratio by Author",
    x = "Author", 
    y = "Type-Token Ratio"
  ) 

plot(ttr_plot)
# ggsave("images/type_token_ratio.png", ttr_plot)


#add tf-idf
books_tf_idf <- book_words %>%
  bind_tf_idf(word, author, n)

#top 10 words by term frequency per author
#initially top words for each author were numbers
top_words <- books_tf_idf %>%
  group_by(author) %>%
  slice_max(n = 10, order_by = tf) %>%
  ungroup()

#plot top 10 words by term frequency per author
top_words_plot <- top_words %>%
  ggplot(aes(x = reorder_within(word, tf, author), y = tf, fill = author)) +
           geom_col(show.legend = FALSE) + 
           facet_wrap(~ author, scales = "free_y") +
           coord_flip() + scale_x_reordered() +
           labs(
             title = "Top 10 Most Frequent Words by Author",
             y = "Term Frequency", 
             x = NULL)

print(top_words_plot)
# ggsave("images/tf_plot.png", top_words_plot)

#top 10 words by tf-idf per author
#initially top words for each author were numbers
top_words_tf_idf <- books_tf_idf %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup()

#plot top 10 words by term frequency per author
top_words_tf_idf_plot <- top_words_tf_idf %>%
  ggplot(aes(
    x = reorder_within(word, tf_idf, author), 
    y = tf_idf, 
    fill = author)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ author, scales = "free_y") +
  coord_flip() + scale_x_reordered() +
  labs(
    title = "Top 10 Words by TF-IDF by Author",
    y = "TF-IDF", 
    x = NULL)

print(top_words_tf_idf_plot)

# can already see differences between authors, especially those focused
# on history versus those focused on tactics

create_wordcloud <- function(token_df, writer, max_words, seed) {
  set.seed(seed)
  wordcloud((token_df %>% filter(author %in% writer))$word, 
            max.words = max_words)
}

create_wordcloud(tokens_words,
                 c("Sun Tzu", "Clausewitz", "Jomini"), 70, 425)
create_wordcloud(tokens_words, 
                 c("Arrian", "Caesar", "Thucydides", "Mahan"), 70, 425)

create_wordcloud(tokens_words, "Sun Tzu", 50, 425)
create_wordcloud(tokens_words, "Clausewitz", 50, 425)
create_wordcloud(tokens_words, "Arrian", 50, 425)
create_wordcloud(tokens_words, "Jomini", 50, 425)
create_wordcloud(tokens_words, "Caesar", 50, 425)
create_wordcloud(tokens_words, "Mahan", 50, 425)
create_wordcloud(tokens_words, "Thucydides", 50, 425)
