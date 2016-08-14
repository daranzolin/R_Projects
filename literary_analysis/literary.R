library(dplyr)
library(gutenbergr)
library(tidytext)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggthemes)

br_titles <- c(
  "The Hound of the Baskervilles", 
  "The Man Who Was Thursday: A Nightmare",
  "Barchester Towers",
  "Gulliver's Travels into Several Remote Nations of the World",
  "Heart of Darkness",
  "The Strange Case of Dr. Jekyll and Mr. Hyde"
  )

am_titles <- c(
  "The Damnation of Theron Ware",
  "The Jungle",
  "The Red Badge of Courage: An Episode of the American Civil War",
  "The Scarlet Letter"
  )

books <- gutenberg_works() %>% 
  filter(title %in% am_titles | title %in% br_titles,
         has_text == TRUE) %>%
  select(gutenberg_id, title) %>% 
  gutenberg_download(meta_fields = "title") %>% 
  unnest_tokens(word, text) %>% 
  mutate(type = ifelse(title %in% am_titles, "American", "British"))

bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score, -lexicon)

books %>%
  inner_join(bing) %>% 
  count(type, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(overall_sentiment = positive - negative,
         sentiment_ratio = round(sum(positive)/sum(negative), 2))

get_top_n_by_sentiment <- function(text, sent, top_n) {
  x <- text %>% 
    inner_join(bing) %>% 
    filter(sentiment == sent) %>% 
    count(title, word) %>%
    arrange(desc(n)) %>%
    rename(word_count = n) %>% 
    do(head(., top_n))
  return(x)
}

x <- get_top_n_by_sentiment(books, "negative", 5)

titles <- unique(x$title)
for (i in seq_along(titles)) {
  gg <- x %>% 
    filter(title == titles[i]) %>% 
    ggplot(aes(reorder(word, word_count), word_count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ggtitle(paste("Most Frequent Negative Words in \n", titles[i])) +
    theme_fivethirtyeight() +
    theme(plot.title = element_text(colour = "firebrick1"))
 ggsave(paste0("literary_analysis/plots/negative_words_plot_", titles[i], ".png"), gg) 
}
