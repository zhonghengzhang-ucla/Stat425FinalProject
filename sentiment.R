library(tidyverse)
library(tm)
library(tidytext)

book <- read_csv("data/book_by_chapter.csv")

tokens_words <- book %>%
  mutate(
    text = text %>% stripWhitespace() %>% str_to_lower() %>%
      removePunctuation() %>% removeNumbers(),
    chapter = as.factor(chapter)
  ) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

bing <- tokens_words %>%
  inner_join(get_sentiments("bing"), by = join_by(word)) %>% 
  group_by(author, chapter)

bing_mean <- bing %>% count(sentiment) %>% 
  mutate(prop = n / sum(n)) %>% filter(sentiment == "negative") %>% 
  group_by(author) %>% 
  summarise(mean = mean(prop), sd = sd(prop))

bing %>% group_by(author) %>% count(sentiment) %>% 
  mutate(prop = n / sum(n)) %>% filter(sentiment == "negative") %>% 
  ggplot(aes(author, prop)) +
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept = mean(prop))) +
  labs(title = "Proportion of Negative Sentiment by Author (Bing)",
       x = "Author", y = "Negative Proportion")

bing %>% count(sentiment) %>% 
  mutate(prop = n / sum(n)) %>% filter(sentiment == "negative") %>% 
  ggplot(aes(chapter, prop)) +
  geom_col(show.legend = FALSE) + facet_wrap(~ author, scales = "free_y") + 
  scale_x_reordered() + coord_flip() + 
  geom_hline(aes(yintercept = mean, col = "Mean"), data = bing_mean) +
  geom_hline(aes(yintercept = mean - 2 * sd, col = "-2SD"),
             data = bing_mean) +
  labs(title = "Proportion of Negative Sentiment by Chapter (Bing)",
       x = "Chapter", y = "Negative Proportion",
       col = "")

afinn <- tokens_words %>%
  inner_join(get_sentiments("afinn"), by = join_by(word))

afinn %>% 
  ggplot(aes(author, value)) + 
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Sentiment Level by Author (AFINN)",
       x = "Author", y = "Sentiment Level",
       col = "")

afinn %>% 
  ggplot(aes(chapter, value)) + 
  geom_boxplot(show.legend = FALSE) +
  facet_wrap(~ author, scales = "free_y") + 
  scale_x_reordered() +
  coord_flip() + geom_hline(yintercept = 0) +
  labs(title = "Sentiment Level by Chapter (AFINN)",
       x = "Chapter", y = "Sentiment Level",
       col = "")

nrc <- tokens_words %>%
  inner_join(get_sentiments("nrc"), by = join_by(word))

nrc %>% group_by(author) %>% count(sentiment) %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(prop = n / sum(n)) %>% 
  filter(sentiment == "negative") %>% 
  ggplot(aes(author, prop)) + 
  geom_col(show.legend = FALSE) +
  geom_hline(aes(yintercept = 0.5)) + 
  labs(title = "Proportion of Negative Sentiment by Author (NRC)",
       x = "Author", y = "Negative Proportion")

nrc %>% group_by(author) %>% count(sentiment) %>% 
  filter(!(sentiment %in% c("positive", "negative"))) %>% 
  mutate(prop = n / sum(n)) %>% 
  ggplot(aes(reorder_within(sentiment, n, author), prop, fill = sentiment)) + 
  coord_flip() +scale_x_reordered() +
  geom_col(show.legend = FALSE) + facet_wrap(~ author, scales = "free_y") +
  geom_hline(aes(yintercept = 1/8)) +
  labs(title = "Proportion of Emotions by Author (NRC)",
       x = "Emotion", y = "Proportion")
