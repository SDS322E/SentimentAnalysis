## Sentiment analysis

library(tidyverse)
library(tidytext)

brothers <- read_lines("pg28054.txt.gz")
head(brothers, 200)

dat <- tibble(text = brothers)
dat

dat |> 
    slice(166:182)

## Find chapters, parts, and blank lines
book <- dat |> 
    mutate(chapter = str_detect(text, "^Chapter"),
           part = str_detect(text, "^PART")) |> 
    mutate(chapternum = cumsum(chapter)) |> 
    mutate(blank = str_detect(text, "[a-zA-Z0-9]+", negate = TRUE)) |> 
    filter(!part & !chapter & !blank & chapternum > 0) |> 
    select(-chapter, -part, -blank) 
book

## Split lines into words
book_words <- book |> 
    unnest_tokens(word, text, token = "ngrams", n = 1) 
book_words 

## Word frequency
book_words |> 
    group_by(word) |> 
    summarize(n = n()) |> 
    arrange(desc(n))

book_words

## Remove stop words
stop_words
stop_words |> 
    select(lexicon) |> 
    distinct()
stop_words |> 
    filter(lexicon == "onix")

stoplist <- stop_words |> 
    filter(lexicon == "onix") |> 
    distinct()

book_words |> 
    anti_join(stoplist, by = "word")

## Word sentiment list
sentiments |> 
    sample_n(10)

## Join with sentiments
book_words |> 
    anti_join(stoplist, by = "word") |> 
    inner_join(sentiments, by = "word") |> 
    group_by(chapternum, sentiment) |> 
    summarize(n = n(), 
              .groups = "drop") |> 
    pivot_wider(names_from = "sentiment",
                values_from = "n") |> 
    mutate(prop_positive = positive / (positive + negative)) |> 
    ggplot(aes(chapternum, prop_positive)) + 
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0.5) +
    labs(x = "Chapter Number", y = "Proportion Positive Sentiment Words")

## Use stop words
word_tab <- book_words |> 
    mutate(block = cut_interval(1:n(), length = 500, labels = FALSE)) |> 
    inner_join(stoplist, by = "word") |> 
    select(-lexicon) |> 
    group_by(block, word) |> 
    summarize(n = n()) |> 
    pivot_wider(names_from = "word", 
                values_from = "n",
                values_fill = 0) 
word_tab

word_tab |> 
    ggplot(aes(block, a)) + 
    geom_line() 

names(word_tab)

word_tab |> 
    ggplot(aes(a, the)) + 
    geom_point()

word_tab |> 
    ggplot(aes(she, he)) + 
    geom_point()

## Get another book
gatsby <- read_lines("pg64317.txt.gz")

dat <- bind_rows(
    tibble(text = brothers,
              book = "brothers karamazov"),
    tibble(text = gatsby,
           book = "great gatsby")
)
dat
tail(dat)

dat |> 
    unnest_tokens(word, text, token = "words") |> 
    group_by(book) |> 
    mutate(block = cut_interval(1:n(), length = 500, labels = FALSE)) |> 
    ungroup() |> 
    inner_join(stoplist, by = "word") |> 
    select(-lexicon) |> 
    group_by(book, block, word) |> 
    summarize(n = n(),
              .groups = "drop") |> 
    group_by(book, word) |> 
    summarize(mean_word = mean(n), 
              .groups = "drop") |> 
    arrange(word) |> 
    pivot_wider(names_from = "book", 
                values_from = "mean_word") |> 
    mutate(diff = `great gatsby` - `brothers karamazov`) |> 
    ggplot(aes(x = 1:length(word), y = diff)) + 
    geom_text(aes(label = word)) +
    labs(x = NULL, y = "Difference in Word Use (Gatsby - Brothers)")
    





