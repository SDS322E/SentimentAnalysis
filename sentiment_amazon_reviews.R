## Amazon review sentiment

library(tidyverse)
library(tidytext)

## Read in review data for "Digital Software"
amazon <- read_tsv("amazon_reviews_us_Digital_Software_v1_00.tsv.gz")

## Quick look
glimpse(amazon)

## Sample some reviews
amazon |>
    select(starts_with("review")) |> 
    sample_n(10)

## Keep a subset of variables
dat <- amazon |> 
    select(product_id, review_body) |> 
    arrange(product_id)
dat

## Tokenize words and save object
rev_words <- dat |> 
    unnest_tokens(word, review_body)
rev_words

## Join words to remove stop words and add sentiment data
rev_words |> 
    anti_join(stop_words, by = "word") |> 
    inner_join(sentiments, by = "word")
    
## Create a score indicating positive or negative sentiment
rev_words |> 
    anti_join(stop_words, by = "word") |> 
    inner_join(sentiments, by = "word") |> 
    mutate(score = case_when(
        sentiment == "negative" ~ -1,
        sentiment == "positive" ~ 1
    ))

## Sum up the scores across all reviews of a product
scores <- rev_words |> 
    anti_join(stop_words, by = "word") |> 
    inner_join(sentiments, by = "word") |> 
    mutate(score = case_when(
        sentiment == "negative" ~ -1,
        sentiment == "positive" ~ 1
    )) |> 
    group_by(product_id) |> 
    summarize(mean_score = mean(score),
              num_words = n()) 
scores

## See top ranked scores
scores |> 
    arrange(desc(mean_score), desc(num_words)) 

## See bottom ranked scores
scores |> 
    arrange(mean_score, num_words)


scores |> 
    ggplot(aes(num_words, mean_score)) + 
    geom_point()


scores |> 
    select(product_id, mean_score) 

amazon |> 
    select(product_id, star_rating)

scores |> 
    inner_join(amazon, by = "product_id") |> 
    select(product_id, star_rating, mean_score) |> 
    mutate(star_rating = factor(star_rating)) |> 
    ggplot(aes(x = star_rating, y = mean_score)) + 
    geom_boxplot()
    
scores |> 
    inner_join(amazon, by = "product_id") |> 
    select(product_id, star_rating, mean_score) |>
    group_by(product_id) |> 
    summarize(mean_star = mean(star_rating),
              mean_score = mean(mean_score)) |> 
    ggplot(aes(x = mean_score, y = mean_star)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_y_continuous(breaks = seq(1, 5), limits = c(1, 5)) +
    labs(x = "Amazon Review Sentiment Score",
         y = "Average Star Rating",
         title = "Relationship between Amazon review sentiment and star rating")


scores |> 
    arrange(desc(num_words))


scores |> 
    ggplot(aes(x = num_words)) + 
    geom_histogram(bins = 30) +
    labs(x = "Number of Review Words",
         y = "Number of Product IDs")


amazon |> 
    group_by(product_id) |> 
    summarize(n = n()) |> 
    ggplot(aes(x = n)) + 
    geom_histogram(bins = 10) +
    scale_x_log10() +
    labs(x = "Number of Reviews (Log Scale)",
         y = "Number of Product IDs")



## Compute correlation between average star rating and sentiment score
scores |> 
    inner_join(amazon, by = "product_id") |> 
    select(product_id, star_rating, mean_score) |>
    group_by(product_id) |> 
    summarize(mean_star = mean(star_rating),
              mean_score = mean(mean_score)) |> 
    filter(mean_score < 2000) |> 
    summarize(cor = cor(mean_star, mean_score))




