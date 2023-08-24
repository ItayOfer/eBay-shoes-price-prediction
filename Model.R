### Importing relevant libraries

library(tidyverse)
library(tidymodels)
if (!require("xgboost")) {
  install.packages("xgboost")
}
library(xgboost)
if (!require("naniar")) {
  install.packages("naniar")
}
library(naniar)
if (!require("textfeatures")) {
  install.packages("textfeatures")
}
library(textfeatures)
if (!require("SnowballC")) {
  install.packages("SnowballC")
}
if (!require("tidytext")) {
  install.packages("tidytext")
}
library(tidytext)
if (!require("proxy")) {
  install.packages("proxy")
}
library(proxy)
if (!require("dbscan")) {
  install.packages("dbscan")
}
library(dbscan)
if (!require("tm")) {
  install.packages("tm")
}
library(tm)
if (!require("stringdist")) {
  install.packages("stringdist")
}
library(stringdist)


### Reading training data and splitting to different samples

tidymodels_prefer()
data <- read_rds("data/ebay_women_shoes_train.rds")
data <- data %>% select(-c("id", "heel_type", "width", "shoe_width",
                           "occasion", "shoe_size", "toe_shape", "size", "model",
                           "year_of_manufacture", "closure", "fastening", "platform_height",
                           "n_sold", "seller_notes"))

# Set aside 10% to use for validation
set.seed(42)
val_split <- initial_split(data, prop = 0.1) 
val <- training(val_split)
shoes_train <- testing(val_split)

# Set aside 30% to use for model tuning
set.seed(42)
tuning_split <- initial_split(shoes_train, prop = (1/3)) 
tuning <- training(tuning_split)
eng <- testing(tuning_split) # 60% to use for feature engineering



### Feature engineering
#### Cleaning existing desired features

clean_features <- function(data) {
  data %>%
    mutate_if(~is.character(.), tolower) %>%
    rename(origin = country_region_of_manufacture) %>%
    mutate(n_watchers = ifelse(is.na(n_watchers), 0, n_watchers),
           free_shipping = ifelse(is.na(free_shipping), 0, free_shipping),
           longtime_member = ifelse(is.na(longtime_member), 0, longtime_member),
           same_day_shipping = ifelse(is.na(same_day_shipping), 0, same_day_shipping),
           fast_safe_shipping = ifelse(is.na(fast_safe_shipping), 0, fast_safe_shipping),
           returns = ifelse(is.na(returns), 0, returns),
           feedback = ifelse(is.na(feedback), 0, feedback),
           condition = ifelse(is.na(condition), "unknown", condition),
           pattern = if_else(!is.na(pattern) | !is.na(theme), 1, 0),
           origin = ifelse(is.na(origin) | origin == "n/a", "unknown", origin),
           vintage = if_else(is.na(vintage) | substr(vintage, 1, 1) == "n", 0, 1),
           heel_height = case_when(
             grepl("high", heel_height) ~ "h",
             grepl("med", heel_height) | grepl("mid", heel_height) ~ "m",
             !(grepl("high", heel_height) | grepl("med", heel_height) | 
                 grepl("mid", heel_height)) ~ "unknown"),
           location = sapply(strsplit(location, ", "), function(x) {
             ifelse(is.na(tail(x, 1)), "unknown", tail(x, 1))})
    ) %>%
    select(-c("material", "lining_material", 
              "upper_material", "colour", "color", "main_colour", 
              "lining", "sole", "theme"))
}


#### Extract style features

style_eng <- function(data) {
  patterns <- c("[[:punct:][:space:]]", "with", "does not apply", "not available", "not sure",
                "not supplied")
  style_names <- data %>% mutate(style = tolower(style)) %>% 
    mutate(style = str_remove_all(style, paste0(patterns, collapse ="|"))) %>%
    mutate(style = ifelse(is.na(style), "________", style)) %>%
    mutate(style = ifelse(style == "n/a", "________", style)) %>%
    mutate(style = ifelse(style == "na", "________", style)) %>%
    mutate(style = ifelse(style == "", "________", style)) %>%
    mutate(style = ifelse(grepl("[0-9]+", style), "________", style)) %>%
    mutate(style = ifelse(grepl("work", style), "work", style)) %>%
    mutate(style = ifelse(grepl("wedge", style), "wedge", style)) %>%
    mutate(style = ifelse(grepl("walk", style), "walking", style)) %>%
    mutate(style = ifelse(grepl("sneaker", style), "walking", style)) %>%
    mutate(style = ifelse(grepl("train", style), "training", style)) %>%
    mutate(style = ifelse(grepl("sport", style), "training", style)) %>%
    mutate(style = ifelse(grepl("tennis", style), "training", style)) %>%
    mutate(style = ifelse(grepl("athletic", style), "training", style)) %>%
    mutate(style = ifelse(grepl("stilletto", style), "stilletto", style)) %>%
    mutate(style = ifelse(grepl("stiletto", style), "stilletto", style)) %>%
    mutate(style = ifelse(grepl("slipper", style), "slipper", style)) %>%
    mutate(style = ifelse(grepl("slide", style), "slide", style)) %>%
    mutate(style = ifelse(grepl("sandal", style), "sandal", style)) %>%
    mutate(style = ifelse(grepl("boot", style), "boot", style)) %>%
    mutate(style = ifelse(grepl("slipon", style), "slipon", style)) %>%
    mutate(style = ifelse(grepl("slingback", style), "slingback", style)) %>%
    mutate(style = ifelse(grepl("pump", style), "pump", style)) %>%
    mutate(style = ifelse(grepl("platform", style), "platform", style)) %>%
    mutate(style = ifelse(grepl("oxford", style), "oxford", style)) %>%
    mutate(style = ifelse(grepl("opentoe", style), "opentoe", style)) %>%
    mutate(style = ifelse(grepl("nurs", style), "nursing", style)) %>%
    mutate(style = ifelse(grepl("mule", style), "mule", style)) %>%
    mutate(style = ifelse(grepl("moccassin", style), "moccassin", style)) %>%
    mutate(style = ifelse(grepl("moccasin", style), "moccassin", style)) %>%
    mutate(style = ifelse(grepl("mocassin", style), "moccassin", style)) %>%
    mutate(style = ifelse(grepl("mocasin", style), "moccassin", style)) %>%
    mutate(style = ifelse(grepl("maryjane", style), "maryjane", style)) %>%
    mutate(style = ifelse(grepl("loafer", style), "loafer", style)) %>%
    mutate(style = ifelse(grepl("laceup", style), "laceup", style)) %>%
    mutate(style = ifelse(grepl("highheel", style), "highheel", style)) %>%
    mutate(style = ifelse(grepl("flipflop", style), "slipper", style)) %>%
    mutate(style = ifelse(grepl("flat", style), "flat", style)) %>%
    mutate(style = ifelse(grepl("court", style), "court", style)) %>%
    mutate(style = ifelse(grepl("clog", style), "clog", style)) %>%
    mutate(style = ifelse(grepl("casual", style), "casual", style)) %>%
    mutate(style = ifelse(grepl("boat", style), "boat", style)) %>%
    mutate(style = ifelse(grepl("ballet", style), "ballet", style)) %>%
    mutate(style = ifelse(grepl("baller", style), "ballet", style)) %>%
    mutate(style = ifelse(grepl("dress", style), "dress", style)) %>%
    mutate(style = ifelse(grepl("strap", style), "strap", style)) %>%
    mutate(style = ifelse(grepl("classic", style), "classic", style)) %>%
    mutate(style = ifelse(grepl("comfort", style), "comfort", style)) %>%
    pull(style)
  return (style_names)
}



style_names <- style_eng(eng)
style_cutoff <- 4
frequent_styles <- names(table(style_names)[-1])[table(style_names)[-1] >= style_cutoff]

style_features <- function(data) {
  data <- data %>% mutate(style = style_eng(data)) %>%
    mutate(style = ifelse((style %in% frequent_styles) & (style != "________"), style, "________"))
  return (data)
}



#### Extract brand features

first_brand_eng <- function(data) {
  patterns <- c("[[:punct:][:space:]]", "the", "shoes", "footwear", "fashion", "edition", "adults",
                "artisan", "collections", "collection", "does not apply", "original", "women", 
                "high", "girl", "miscellaneous", "new york", "paris", "none", "non", "style",
                "stiletto", "boots")
  brand_names <- data %>% mutate(brand = tolower(brand)) %>% 
    mutate(brand = str_remove_all(brand, paste0(patterns, collapse ="|"))) %>%
    mutate(brand = ifelse(is.na(brand), "________", brand)) %>%
    mutate(brand = ifelse(brand == "n/a", "________", brand)) %>%
    mutate(brand = ifelse(brand == "na", "________", brand)) %>%
    mutate(brand = ifelse(brand == "", "________", brand)) %>%
    mutate(brand = ifelse(grepl("lucky", brand), "lucky", brand)) %>%
    mutate(brand = ifelse(grepl("brand", brand), "________", brand)) %>%
    mutate(brand = ifelse(grepl("undisclosed", brand), "________", brand)) %>%
    mutate(brand = ifelse(grepl("unlisted", brand), "________", brand)) %>%
    mutate(brand = ifelse(grepl("unknown", brand), "________", brand)) %>%
    mutate(brand = ifelse(grepl("ladies", brand), "________", brand)) %>%
    mutate(brand = ifelse(grepl("newlook", brand), "________", brand)) %>%
    mutate(brand = ifelse(brand == "new", "________", brand)) %>%
    mutate(brand = ifelse(grepl("aeoutfitters", brand), 
                          "americaneagle", brand)) %>%
    mutate(brand = ifelse(grepl("americaneagleoutfitters", brand), 
                          "americaneagle", brand)) %>%
    mutate(brand = ifelse(grepl("bcbg", brand), "bbgeneration", brand)) %>%
    mutate(brand = ifelse(grepl("bc", brand), "bcfootwear", brand)) %>%
    mutate(brand = ifelse(grepl("bbgeneration", brand), "bcbg", brand)) %>%         
    mutate(brand = ifelse(grepl("bobs", brand), "skechers", brand)) %>%
    mutate(brand = ifelse(grepl("boc", brand), "børn", brand)) %>%
    mutate(brand = ifelse(grepl("born", brand), "børn", brand)) %>%
    mutate(brand = ifelse(grepl("louboutin", brand), "louboutin", brand)) %>%
    mutate(brand = ifelse(grepl("clarks", brand), "clarks", brand)) %>%
    mutate(brand = ifelse(grepl("indigo", brand), "clarks", brand)) %>%
    mutate(brand = ifelse(grepl("earth", brand), "earth", brand)) %>%
    mutate(brand = ifelse(grepl("fft", brand), "söfft", brand)) %>%
    mutate(brand = ifelse(grepl("eurosoft", brand), "söfft", brand)) %>%
    mutate(brand = ifelse(grepl("jbu", brand), "jambu", brand)) %>%
    mutate(brand = ifelse(grepl("ralphlauren", brand), "ralphlauren", brand)) %>%
    mutate(brand = ifelse(grepl("madden", brand), "stevemadden", brand)) %>%
    mutate(brand = ifelse(grepl("sperry", brand), "sperry", brand)) %>%
    mutate(brand = ifelse(grepl("teva", brand), "naot", brand)) %>%
    mutate(brand = ifelse(grepl("ugg", brand), "ugg", brand)) %>%
    mutate(brand = ifelse(grepl("chloé", brand), "chloe", brand)) %>%
    mutate(brand = ifelse(grepl("ferrari", brand), "ferrari", brand)) %>%
    mutate(brand = ifelse(grepl("propét", brand), "propet", brand)) %>%
    pull(brand)
  return (brand_names)
}




# Adapted from https://rstudio-pubs-static.s3.amazonaws.com/445820_c6663e5a79874afdae826669a9499413.html
# Sample brand name column
brand_names <- first_brand_eng(eng)
brand_cutoff <- 2
frequent_brands <- names(table(brand_names)[-1])[table(brand_names)[-1] >= brand_cutoff]
frequent_brands <- unique(c(frequent_brands[nchar(frequent_brands) > 2 & 
                                              frequent_brands != "________"], 
                            "hushpuppies", "adidas", "newbalance",  "reebok", 
                            "birkenstock", "stevemadden", "jimmychoo", "vans", 
                            "converse", "veja", "hoka", "fila", "manoloblahnik", "ecco", 
                            "brooks", "timberland", "colehaan", "asics", "allsaints", 
                            "katespade", "columbia", "dearfrances", "michaelkors", 
                            "ninewest", "ralphlauren", "calvinklein", "balenciaga"))

second_brand_eng <- function(brand, title) {
  clean_title <- sapply(title, 
                        function(x) str_remove_all(tolower(x), "[[:punct:][:space:]]+"))
  appears <- sapply(frequent_brands, function(x) 
    grepl(x, clean_title)
  )
  first_match <- apply(appears, 1, function(row) which(row)[1])
  res <- ifelse(is.na(first_match), "________", frequent_brands[first_match])
  res <- ifelse(!(brand %in% frequent_brands) & (res != "________"), res, brand)
  return (res)
}



brand_names <- second_brand_eng(brand_names, eng$title)
frequent_brands <- names(table(brand_names))[table(brand_names) >= brand_cutoff]
frequent_brands <- unique(c(frequent_brands[nchar(frequent_brands) > 2 & 
                                              frequent_brands != "________"], 
                            "hushpuppies", "adidas", "newbalance",  "reebok", 
                            "birkenstock", "stevemadden", "jimmychoo", "vans", 
                            "converse", "veja", "hoka", "fila", "manoloblahnik", "ecco", 
                            "brooks", "timberland", "colehaan", "asics", "allsaints", 
                            "katespade", "columbia", "dearfrances", "michaelkors", 
                            "ninewest", "ralphlauren", "calvinklein", "balenciaga"))
rare_brands <- brand_names[!(brand_names %in% c(frequent_brands, "________"))]

cluster_count <- 30
set.seed(42)
corpus = tm::Corpus(tm::VectorSource(rare_brands))

# Building the feature matrices
tdm <- tm::DocumentTermMatrix(corpus)
tdm.tfidf <- tm::weightTfIdf(tdm)
# We remove A LOT of features. R is natively very weak with high dimensional matrix
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999)
# There is the memory-problem part
# - Native matrix isn't "sparse-compliant" in the memory
# - Sparse implementations aren't necessary compatible with clustering algorithms
tfidf.matrix <- as.matrix(tdm.tfidf)
# Cosine distance matrix (useful for specific clustering algorithms)
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

clustering.kmeans <- kmeans(tfidf.matrix, cluster_count)
clusters <- clustering.kmeans$cluster



average_word <- function(i) {
  word_cluster <- rare_brands[clusters == i]
  average_word <- stringdist::stringdistmatrix(word_cluster, method = "lcs") %>%
    apply(2, mean) %>%
    which.min() %>%
    {word_cluster[.]}
  average_word  
}

cluster_names <- sapply(1:cluster_count, FUN=average_word)

find_closest_cluster <- function(brand) {
  distances <- stringdist::stringdistmatrix(brand, cluster_names, method = "cosine")
  closest_cluster <- cluster_names[which.min(distances)]
  return(ifelse(closest_cluster == "e", "________", closest_cluster)) # "e" is garbage cluster
}

brand_features <- function(data) {
  data <- data %>% mutate(brand = first_brand_eng(data)) %>%
    mutate(brand = second_brand_eng(brand, title)) %>%
    mutate(brand = ifelse(brand %in% rare_brands, sapply(brand, find_closest_cluster), brand))
  return (data)
}


#### Extracting features from title

tidy_titles <- eng %>%
  mutate(id = 1:n(), log_price=log(price)) %>%
  select(id, log_price, title) %>%
  unnest_tokens(word, title) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

min_n_words <- tidy_titles %>%
  count(id, word) %>%
  count(word) %>%
  filter(n > 100) %>%
  pull(word)

ttest_sig <- function(data, target) {
  target_data <- data %>% filter(word == target)
  rest_data <- data %>% filter(!(id %in% target_data$id)) %>% 
    distinct(id, log_price)
  try({
    return (t.test(select(target_data, log_price), select(rest_data, log_price))$p.value)
  })
  return (1)
}

ttest_res <- tidy_titles %>%
  filter(word %in% min_n_words) %>%
  distinct(word) %>%
  mutate(pval = map(word, ~ttest_sig(tidy_titles, .x))) %>%
  mutate(adj.pval = p.adjust(pval, method = "fdr"))

sig_words <- ttest_res %>% filter(adj.pval < 0.01) %>% select(word)

words_appear <- function(title) {
  words <- unique(c(sig_words$word, "rare", "sale", "new", " men", 
                    "kid", "\\$", "msrp", "stilleto", "swarovski"))
  appearances <- map(words, ~ifelse(str_detect(tolower(title), .x), 1, 0))
  names(appearances) <- str_c("a_", words)
  return (appearances)
}

title_features <- function(data) {
  data$title %>%
    textfeatures(normalize=FALSE, word_dims=10) %>%
    select(n_words, n_uq_words, n_caps, n_lowers, n_polite,
           sent_afinn, sent_bing, sent_syuzhet, sent_vader
           #,paste0("w", 1:10) - textfeatures not working for some odd reason..
    ) %>%
    bind_cols(data) %>%
    bind_cols(map_dfr(data$title, words_appear)) %>%
    select(-title)
}


#### Full features

full_features <- function(data, train=TRUE) { 
  res <- title_features(brand_features(style_features(clean_features(data))))
  if (train) {
    res <- res %>% mutate(log_price = log(price)) %>% select(-price)  
  }
  return (res)
}


### Model Tuning
#### GBT

tuning_engd <- full_features(tuning)
cv_splits <- vfold_cv(tuning_engd, v=5)



rec <- recipe(log_price ~., data=tuning_engd) %>% 
  step_novel(all_nominal_predictors()) %>%     
  step_dummy(all_nominal_predictors())
mod <- boost_tree(mode="regression", engine="xgboost", trees=tune(),
                  learn_rate=tune(), sample_size=tune())
param_grid <- expand_grid(trees=c(500, 750, 1000), 
                          learn_rate=c(0.001, 0.005, 0.01), 
                          sample_size=c(0.25, 0.5, 0.75))



tune_res <- tune_grid(object=mod, preprocessor=rec, resamples=cv_splits, grid=param_grid,
                      metrics=metric_set(rmse), control=control_grid(verbose=TRUE))
collected_metrics <- collect_metrics(tune_res)



show_best(tune_res, n=5, metric="rmse")


### Testing against validation set
#### Baseline model and comparison function

baseline_model <- function(train) { # Receives raw data
  mod <- lm(log(price) ~ category * condition + brand + free_shipping, data = train)
  return (mod)
}

score_comparison <- function(score, baseline) {
  return (score/baseline)
}


#### Gather data

train <- bind_rows(eng, tuning)
top_brands <- train %>%
  count(brand, sort = TRUE) %>%
  slice(1:10) %>% pull(brand)
train_baseline <- train %>% mutate(condition = ifelse(is.na(condition), "NA", condition)) %>%
  mutate(brand = ifelse(is.na(brand), "NA",
                        ifelse(brand %in% top_brands, brand, "other"))) %>%
  mutate(free_shipping = ifelse(is.na(free_shipping), 0, free_shipping))

train_ready <- full_features(train)
rec <- recipe(log_price ~., data=train_ready) %>% 
  step_novel(all_nominal_predictors()) %>%     
  step_dummy(all_nominal_predictors()) %>%
  prep(train_ready)
train_ready <- rec %>% bake(train_ready)


val_baseline <- val %>% mutate(condition = ifelse(is.na(condition), "NA", condition)) %>%
  mutate(brand = ifelse(is.na(brand), "NA",
                        ifelse(brand %in% top_brands, brand, "other"))) %>%
  mutate(free_shipping = ifelse(is.na(free_shipping), 0, free_shipping))

val_ready <- full_features(val)
val_ready <- rec %>% bake(val_ready)

#### Model of choice

set.seed(42)
mod <- boost_tree(mode="regression", engine="xgboost", trees=10000,
                  learn_rate=0.01, sample_size=0.75) %>% fit(log_price ~ ., data=train_ready)


#### Assess performance

pred <- mod %>% predict(new_data=val_ready)
mod_rmse <- sqrt(mean(((pred-val_ready$log_price)^2)$.pred))
mod_rmse

baseline_mod <- baseline_model(train_baseline)
baseline_pred <- baseline_mod %>% predict(newdata=val_baseline)
baseline_rmse <- sqrt(mean((baseline_pred-log(val_baseline$price))^2))
baseline_rmse

0.63*mod_rmse/baseline_rmse 


### Predict on test set
#### Gather data

train_ready <- full_features(data)
rec <- recipe(log_price ~., data=train_ready) %>% 
  step_novel(all_nominal_predictors()) %>%     
  step_dummy(all_nominal_predictors()) %>%
  prep(train_ready)
train_ready <- rec %>% bake(train_ready)

test <- read_rds("data/ebay_women_shoes_test.rds")
test_ready <- full_features(test, FALSE)
test_ready <- rec %>% bake(test_ready)


#### Train model

set.seed(42)
mod <- boost_tree(mode="regression", engine="xgboost", trees=10000,
                  learn_rate=0.01, sample_size=0.75) %>% fit(log_price ~ ., data=train_ready)


#### Predict on test data

pred <- mod %>% predict(new_data=test_ready)
write_csv(cbind(test$id, pred), "model5.csv")


