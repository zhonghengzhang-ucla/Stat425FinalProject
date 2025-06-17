library(tidyverse)
library(tm)
library(tidytext)
library(topicmodels)

books <- read_csv("data/book_by_chapter.csv")

book_dtm <- books %>% group_by(author, chapter) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = join_by(word)) %>%
  group_by(author, chapter, word)%>% count() %>%
  mutate(book_chapter = paste(author, chapter, sep="_")) %>%
  cast_dtm(book_chapter, word, n)

trials <- 20
mismatch <-
  tibble(chapter = character(),
         topic = numeric(),
         author = character())
duplicate <- c()
for (seed in c(1:trials)) {
  book_lda <- book_dtm %>% LDA(k = 7, control = list(seed = seed))
  
  book_lda_tidy <- book_lda %>% tidy() %>% arrange(desc(beta))
  
  book_lda_tidy_gamma <- book_lda %>% tidy(matrix = "gamma") %>% 
    separate(document, c("author", "chapter"), sep = "_")
  
  consensus <- book_lda_tidy_gamma %>% group_by(author, chapter) %>% 
    arrange(desc(gamma)) %>% slice_max(gamma, n = 1) %>% group_by(author) %>%
    count(topic) %>% slice_max(n, n = 1) %>% select(-n)
  
  chapter_mismatch <- book_lda_tidy_gamma %>% group_by(author, chapter) %>% 
    arrange(desc(gamma)) %>% slice_max(gamma, n = 1) %>% 
    anti_join(consensus, by = join_by(author, topic))
  
  chapter_topic <- book_lda_tidy_gamma %>% group_by(author, chapter) %>% 
    slice_max(gamma, n = 1)
  
  pairs <- consensus %>% ungroup() %>% group_by(topic) %>% filter(n() > 1) %>% 
    summarise(authors = paste(author, collapse = " & "))
  
  for (i in pairs$authors) {
    if (!(i %in% names(duplicate))) {
      duplicate[i] <- 1
    }
    else {
      duplicate[i] <- duplicate[i] + 1
    }
  }
  
  mismatch <-
    mismatch %>% bind_rows(
      chapter_mismatch %>% mutate(chapter = paste(author, chapter)) %>%
        ungroup() %>% select(chapter, topic) %>%
        left_join(consensus, by = join_by(topic))
    )
}

same_topic_plot <- tibble(authors = names(duplicate), n = duplicate) %>% 
  ggplot(aes(fct_reorder(authors, n), n)) + geom_col() + coord_flip() +
  labs(
    title = "Number of Times Authors Classified Into Same Topic",
    y = "", 
    x = "Pairs of Authors")
plot(same_topic_plot)

misclassification_plot <- 
  mismatch %>% replace_na(list(author = "none")) %>%
  group_by(chapter, author) %>% count() %>% 
  ggplot(aes(reorder_within(chapter, n, author), n, fill = author)) +
  geom_col(show.legend = FALSE) + coord_flip() + 
  scale_x_reordered() + facet_wrap(~ author, scales = "free_y") +
  labs(title = "Misclassified Chapters by Author Topic",
       x = "Chapter", y = "Count")
plot(misclassification_plot)  
