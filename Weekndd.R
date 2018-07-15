library(dplyr)
library(tidyverse)
library(gutenbergr)
library(tidytext)
library(tidyr)
library(stringr)

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
  ggplot(aes(word, n, fill = "red")) +
  geom_col() +
  xlab(NULL)+
  coord_flip()+
  ggtitle("Words Frequency in Weeknd Songs") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.background = element_rect("black"),
        panel.grid = element_line("black"))

tidy_weeknd2 <- weeknd %>%
  group_by(Album) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Lyrics)


weeknd_sentiment <- tidy_weeknd2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(Album,index = linenumber %/% 1, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(weeknd_sentiment, aes(index, sentiment, fill = Album)) +
  geom_smooth(show.legend = FALSE) +
  facet_wrap(~Album, ncol = 2, scales = "free_x") +
  xlab("Song Number(Approx.)")+
  ylab("Sentiment")+
  ggtitle("Sentiment Analysis of songs of Weeknd Albums.")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

