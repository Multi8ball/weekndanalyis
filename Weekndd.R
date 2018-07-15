library(dplyr)
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(tidyr)

weeknd <- Lyrics_Database
weeknd <- tbl_df(weeknd)

tidy_weeknd <- weeknd %>%
  unnest_tokens_("word","Lyrics") %>%
  anti_join(stop_words)

word_count <- tidy_weeknd %>%
  count(word, sort = TRUE)

tidy_weeknd %>%
  count(word, sort = TRUE) %>%
  filter(n >= 40) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL)+
  coord_flip()+
  ggtitle("Words Frequency in Weeknd Songs")
