library(gutenbergr)
library(tidyverse)
library(tm)
library(tidytext)

#download books
df <- gutenberg_download(
  c(7142, 13549, 10657, 132, 13529, 46976),
  mirror = "http://mirror.csclub.uwaterloo.ca/gutenberg",
  meta_fields = c("author","title")
)

df <- df %>% mutate(
  author = case_match(
    author,
    "Sunzi, active 6th century B.C." ~ "Sun Tzu",
    "Thucydides" ~ "Thucydides",
    "Caesar, Julius" ~ "Caesar",
    "Mahan, A. T. (Alfred Thayer)" ~ "Mahan",
    "Jomini, Antoine Henri, baron de" ~ "Jomini",
    "Arrian" ~ "Arrian"
    )
)

# remove unrelated chapters and introductions by book, and group by chapter
sun_tzu <- df %>% filter(author == "Sun Tzu")
sun_tzu <- sun_tzu[
  -c(1:(which(sun_tzu$text == "Chapter I. LAYING PLANS") - 1)),
]
sun_tzu <- sun_tzu %>%
  mutate(chapter = str_detect(text, "^Chapter") %>% cumsum()) %>%
  mutate(paragraph = cumsum(text == "")) %>%
  group_by(author, title, chapter, paragraph) %>%
  summarise(text = paste(text, collapse = " ") %>% trimws()) %>%
  filter(text != "") %>% 
  mutate(annotation = str_detect(text, "[\\[\\]]")) %>%
  filter(!annotation) %>% select(-annotation)

clausewitz <- read_lines("On_War.txt")
clausewitz <- tibble(
  gutenberg_id = 1946,
  text = clausewitz,
  author = "Clausewitz",
  title = "On War"
)
clausewitz <- clausewitz[-c(1:(which(clausewitz$text == "NOTICE")[2] - 1)),]
clausewitz <- clausewitz[
  -c(which(clausewitz$text == "BRIEF MEMOIR OF GENERAL CLAUSEWITZ"):
       (which(clausewitz$text == "BOOK I. ON THE NATURE OF WAR") - 1)),
  ]
clausewitz <- clausewitz %>% mutate(
  chapter = str_detect(text, "^BOOK|SKETCHES") %>% cumsum()
)

jomini <- df %>% filter(author == "Jomini")
jomini <- jomini[
  c(which(jomini$text == "DEFINITION OF THE ART OF WAR."):
      (which(jomini$text == "INDEX") - 1)),
]
jomini <- jomini %>% 
  mutate(chapter = str_detect(text, "^CHAPTER") %>% cumsum())

thucydides <- df %>% filter(author == "Thucydides")
thucydides <- thucydides[-c(1:(which(thucydides$text == "BOOK I") - 1)),]
thucydides <- thucydides %>% mutate(
  chapter = str_detect(text, "^BOOK") %>% cumsum()
)

arrian <- df %>% filter(author == "Arrian")
arrian <- arrian[
  -c(1:which(arrian$text == "THE ANABASIS OF ALEXANDER.")),]
arrian <- arrian[
  c(1:(which(str_detect(arrian$text, "^FOOTNOTES")) - 1)),
]
arrian <- arrian %>% mutate(
  chapter = str_detect(text, "^BOOK") %>% cumsum()
)

mahan <- df %>% filter(author == "Mahan")
mahan <- mahan[-c(1:(which(mahan$text == "PREFACE.") - 1)),]
mahan <- mahan[-c(which(mahan$text == "CONTENTS."):
                    (which(mahan$text == "INTRODUCTORY.")[2] - 1)),]
mahan <- mahan %>% mutate(
  chapter = str_detect(text, "^CHAPTER") %>% cumsum()
)

caesar <- df %>% filter(author == "Caesar")
caesar <- caesar[
  c(which(caesar$text == "THE WAR IN GAUL")[2]:
      (which(caesar$text == "THE CIVIL WAR")[2] - 1)),]
caesar <- caesar %>% mutate(
  chapter = str_detect(text, "^BOOK") %>% cumsum()
)


# combine books and merge lines into paragraphs
books_by_line <-
  rbind(clausewitz,
        jomini,
        arrian,
        mahan,
        thucydides,
        caesar) %>% 
  group_by(author) %>% 
  mutate(paragraph = cumsum(text == "")) %>% 
  filter(text != "") %>% 
  rbind(sun_tzu)

write_csv(books_by_line, "data/book_by_line.csv")

books_by_paragraph <- books_by_line %>% 
  group_by(author, title, chapter, paragraph) %>%
  summarise(text = paste(text, collapse = " ") %>% trimws()) %>%
  select(-paragraph)

write_csv(books_by_paragraph, "data/book_by_paragraph.csv")

book_by_chapter <- books_by_paragraph %>% 
  group_by(author, title, chapter) %>% 
  summarise(text = paste(text, collapse = " ") %>% trimws()) %>% 
  filter(chapter > 0)

write_csv(book_by_chapter, "data/book_by_chapter.csv")

# clean text
df_clean <- books_by_line %>%
  mutate(
    text = text %>% stripWhitespace() %>% str_to_lower() %>%
      removePunctuation() %>% removeNumbers()
  ) 

#tokenize into words
tokens_words <- df_clean %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word")

write_csv(tokens_words, "data/tokens_words_clean.csv")
