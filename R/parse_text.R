# 1. Use spaCy to parse reviews (including tokenization, lemmatization, POS tagging, and dependency parsing)
# Perform initial preprocessing (convert to lowercase ) and filtering (remove short tokens, punctuation, tokens containing "non-letters", etc.)
# 2. Get stopwords (including task-specific stopwords)
# 3. Using fixed settings, filter words which we will use for topic modeling in Python

# Load libraries and initialize environment
library(spacyr)
library(tidyr)
library(dplyr)
library(stringr)
library(tm)
library(tidytext)
library(lubridate)

#spacy_install()
spacy_initialize(model = "en_core_web_sm")
setwd("C://Users/tysonp/Desktop/data-science-projects/ice-cream-data-dashboard/data")

# Load reviews data
rev_all <- read.csv("ice-cream-dataset-v2/combined/reviews.csv", encoding = "UTF-8")

# Use spaCy to parse all reviews. This may take up to 1-2 minutes with dependency parsing enabled (`dependency = TRUE`).
parsedtxt <- spacy_parse(rev_all$text, pos = TRUE, lemma = TRUE, entity = FALSE, dependency = TRUE)

# Filter words. We'll do more filtering in server.R.
parsedtxt2 <- parsedtxt %>% 
  # Note: all pronouns (i.e. I, my, her) are given lemma -PRON-. We don't expect pronouns to be informative so we'll remove these.
  filter(lemma != "-PRON-") %>% 
  
  # Recode token: nt/n't --> not. It is important to keep valence shifters intact for sentiment analysis.
  mutate_at(.funs = function(x){recode(x, "n't" = "not", "nt" = "not")}, .vars = c("token","lemma")) %>% 
  
  # Convert to lowercase 
  mutate_at(.funs = tolower, .vars = c("token", "lemma")) %>% 
  
  # Preliminary filtering. Since user can choose token or lemma in the app, we can only filter rows where BOTH are "bad."
  # Later we'll apply filter based on user input, i.e. if user chooses `token` then we will do: filter(grepl("^[a-z]+$", token) ...
  filter(grepl("^[a-z]+$", token) | grepl("^[a-z]+$", lemma)) %>% # remove tokens/lemmas which consist of anything other than letters
  filter((nchar(token) >= 3) | (nchar(lemma) >= 3)) %>% # remove short tokens/lemmas
  
  # Recode doc_id for file compactness: "text1", "text2", ... --> 1, 2, ...
  mutate(doc_id = as.integer(gsub(pattern = "text", x = doc_id, replacement = ""))) 

# Save to file
write.csv(parsedtxt2, "parsed_reviews.csv", row.names = FALSE)

# ----------- PARSE STOPWORDS ------------
Snowball_sw <- stopwords("English")
SMART_sw <- stopwords("SMART")

Snowball_sw_parsed <- spacy_parse(Snowball_sw, pos = FALSE, entity = FALSE) %>% 
  mutate(lemma = case_when(lemma == "-PRON-" ~ token, TRUE ~ lemma)) %>%
  select(token, lemma) %>%
  distinct()

SMART_sw_parsed <- spacy_parse(SMART_sw, pos = FALSE, entity = FALSE) %>% 
  mutate(lemma = case_when(lemma == "-PRON-" ~ token, TRUE ~ lemma)) %>%
  select(token, lemma) %>%
  distinct()

write.csv(Snowball_sw_parsed, "Snowball_sw_parsed.csv", row.names = FALSE)
write.csv(SMART_sw_parsed, "SMART_sw_parsed.csv", row.names = FALSE)

# TASK SPECIFIC STOPWORDS
prod_all <-read.csv("ice-cream-dataset-v2/combined/products.csv", encoding = "UTF-8")
task_sw_parsed <- data.frame()
for (b in c("bj", "hd", "breyers", "talenti")){
  prod <- prod_all %>% filter(brand == b)
  
  prod_name_parsed <- spacy_parse(prod$name, pos = FALSE, entity = FALSE)
  
  prod_ingredients_parsed <- spacy_parse(prod$ingredients, pos = FALSE, entity = FALSE) 
  
  prod_name_parsed$brand <- b
  prod_name_parsed$doc_type <- "name"
  prod_ingredients_parsed$brand <- b
  prod_ingredients_parsed$doc_type <- "ingredients"
  
  prod_parsed_merged <-rbind(prod_name_parsed, prod_ingredients_parsed) 
  task_sw_parsed <- rbind(task_sw_parsed, prod_parsed_merged)
}

task_sw_parsed <- task_sw_parsed %>%
  mutate(lemma = case_when(lemma == "-PRON-" ~ token, TRUE ~ lemma)) %>%
  mutate_at(.funs = tolower, .vars = c("token","lemma")) %>%
  filter(grepl("^[a-z]+$", token) | grepl("^[a-z]+$", lemma)) %>% # remove tokens/lemmas which consist of anything other than letters
  filter((nchar(token) >= 3) | (nchar(lemma) >= 3)) %>% # remove short tokens/lemmas
  distinct(token, lemma, brand, doc_type)

# use this format instead if you want to filter by document frequency rather than filtering ALL words that appear in the product name/ingredients
# task_sw_parsed <- task_sw_parsed %>%
#   mutate(lemma = case_when(lemma == "-PRON-" ~ token, TRUE ~ lemma)) %>%
#   mutate_at(.funs = tolower, .vars = c("token","lemma")) %>%
#   filter(grepl("^[a-z]+$", token) | grepl("^[a-z]+$", lemma)) %>% # remove tokens/lemmas which consist of anything other than letters
#   filter((nchar(token) >= 3) | (nchar(lemma) >= 3)) %>% # remove short tokens/lemmas
#   select(doc_id, token, lemma, brand, doc_type) %>%
#   mutate(doc_id = as.integer(gsub(pattern = "text", x = doc_id, replacement = "")))
# task_sw_parsed <- task_sw_parsed %>% group_by(brand, doc_type, token) %>% summarize(docfreq = length(unique(doc_id))) # need to divide by n_docs

write.csv(task_sw_parsed, "task_sw_parsed.csv", row.names = FALSE)

# some additional words/misspellings to include in stopwords
other_sw <- c("ben", "jerry", "jerrys",  "benjerrys", "haagen", "dazs", "daz", "haagendazs", "haagendaz", "hagen", "hagendazs", 
              "breyers", "breyer", "talenti", "talentis", "unilever", "mills", "froneri", "ice", "cream", "icecream", "icecreams", 
              "gelato", "brand", "nestle", "nestl", "com", "dreyer", "dryers", "review", "collected", "collect", "part", "promotion")

other_sw_parsed <- spacy_parse(other_sw, pos = FALSE, entity = FALSE) %>% 
  mutate(lemma = case_when(lemma == "-PRON-" ~ token, TRUE ~ lemma)) %>%
  select(token, lemma) %>%
  distinct()

write.csv(other_sw_parsed, "other_sw_parsed.csv", row.names = FALSE)

# ----- PREPARE FOR TOPIC MODELING -----
parsedtxt_all <- read.csv("parsed_reviews.csv")
rev_all <- read.csv("ice-cream-dataset-v2/combined/reviews.csv") 
rev_all <- rev_all %>% mutate(rev_id = row_number())
prod_all <-read.csv("ice-cream-dataset-v2/combined/products.csv")

lemmatization <- "lemma"
standard_sw <- "SMART (571 words)"
task_sw <- c("name", "ingredients")
filter_words <- FALSE
filter_words_num <- c(0,1)
max_tokens <- 5000
valence_shifters <- FALSE

# # Optional: filter by POS
# parsedtxt_all <- parsedtxt_all %>% filter(pos %in% c("NOUN", "ADJ", "VERB"))

# Rename token column (lemma/token) to "word" and drop other column
parsedtxt_all <- parsedtxt_all %>%
  rename("word" = lemmatization) %>%
  select(-setdiff(c("token", "lemma"), lemmatization))

# Filter parsed reviews based on lemmatization choice
parsedtxt_all <- parsedtxt_all %>%
  filter(grepl("^[a-z]+$",  word)) %>% # remove tokens/lemmas which consist of anything other than letters
  filter(nchar(word) >= 3) # remove short tokens/lemmas

# Standard English stopwords (see https://github.com/igorbrigadir/stopwords for a comprehensive database)
standard_sw <- switch(standard_sw,
                      "None" = c(),
                      "Snowball (174 words)" = read.csv("Snowball_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]],
                      "SMART (571 words)" = read.csv("SMART_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]]
                      
)

# some additional words/misspellings to include in stopwords
other_sw <- read.csv("other_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]]


# Subset based on brand 
parsedtxt_brands <- data.frame()
for (b in c("bj", "hd", "breyers", "talenti")){
  rev <- rev_all %>% filter(brand == b)
  parsedtxt <- parsedtxt_all %>% filter(doc_id %in% rev$rev_id)
  
  # Task-specific stopwords
  if (length(task_sw) > 0){
    custom_sw <- read.csv("task_sw_parsed.csv", encoding = "UTF-8")
    custom_sw <- custom_sw %>% 
      filter(brand == b & doc_type %in% task_sw)
    custom_sw <- custom_sw[[lemmatization]]
  } else{
    custom_sw <- c()
  }
  
  # Combine all stopwords
  sw <- unique(c(standard_sw, custom_sw, other_sw))
  
  # Words to exclude from stopwords (but not if filtering by document frequency)
  exclude <- c("natural", "flavor", "flavour", "creamy", "value", "down", "downward", "sweet", "super", "like", "ingredient", "ingredients",
               "artificial", "flavoring", "heaven", "light", "extra", "layers", "add", "added", "free", "indulgences", "indulgence", "delight",
               "delights", "style", "layered", "layer", "maintain", "freshness", "flavored", "flavors", "everything")
  sw <- setdiff(sw, exclude)
  
  # Valence shifters
  valShift <- lexicon::hash_valence_shifters$x
  sw <- union(sw, valShift)
  
  parsedtxt <- parsedtxt %>% anti_join(data.frame(word=sw), by = "word")
  
  # 5. Filter by document frequency and limit vocab size to `max_tokens`
  L <- length(unique(parsedtxt$doc_id)) # total number of documents (that still have words after removing stopwords, etc.)
  
  # keep words
  kw <- parsedtxt %>%
    group_by(word) %>%
    summarise(termfreq = n(), docfreq = length(unique(doc_id))/L) 
  
  if (filter_words){
    kw <- kw %>% filter(filter_words_num[1] <= docfreq & docfreq <= filter_words_num[2])
  }
  
  kw <- kw %>% arrange(desc(docfreq), desc(termfreq))
  kw <- kw %>% slice_head(n = max_tokens) %>% .$word
  parsedtxt <- parsedtxt %>% filter(word %in% kw)
  
  parsedtxt_brands <- rbind(parsedtxt_brands, parsedtxt)
}

# join with brand name & star rating -- then filter out reviews with < 5 stars -- then select only the important columns
parsedtxt_brands <- parsedtxt_brands %>% 
  inner_join(rev_all %>% select(rev_id, brand, stars), by = c("doc_id" =  "rev_id")) %>%
  filter(stars < 5) %>%
  select(c("doc_id", "sentence_id", "word", "brand"))
  
write.csv(parsedtxt_brands, "parsed_reviews_for_topic_model.csv", row.names = FALSE)
