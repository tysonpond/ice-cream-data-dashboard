library(shiny)
library(shinydashboard)
library(shinyWidgets) # radioGroupButtons, etc.
library(shinyjs)
library(shinycssloaders)

library(highcharter)
# library(ggplot2)
# library(scales) # for wrapping long text labels

library(dplyr)
library(tidyr) # complete() function and other utilities
library(lubridate) # date processing

library(xts)
library(DT)
library(jsonlite)

# text processing & NLP
library(vegan) # entropy & diversity
library(tidytext) # get_sentiments()
library(stringr) # str_squish()
# library(tm)
# library(SnowballC)
# library(class)
# library(spacyr)
# spacy_initialize(model = "en_core_web_sm")
# library(wordcloud)
# library(wordcloud2) # better wordcloud for Shiny (original wordcloud is hard to size properly in app)

library(colorspace) # for defining fading color scheme for wordclouds

# change these locations if you change file organization
setwd("C:/Users/tysonp/Desktop/data-science-projects/ice-cream-data-dashboard/shiny_app")
prod_all <- read.csv("../data/ice-cream-dataset-v2/combined/products.csv", encoding = "UTF-8")
rev_all <- read.csv("../data/ice-cream-dataset-v2/combined/reviews.csv", encoding = "UTF-8")
rev_all <- rev_all %>% mutate(rev_id = row_number())

# Review counts in product dataset do not always match number of reviews in reviews dataset. Correct this
prod_all <- rev_all %>%
        group_by(key) %>% 
        summarize(rating_count=n(), rating=round(mean(stars), 1)) %>%
        ungroup() %>%
        right_join(prod_all %>% select(-c("rating_count", "rating")))

# Topic modeling
cluster_data_all <- read.csv("../data/clustering/cluster_labelings.csv", encoding = "UTF-8")
cluster_words_all <- fromJSON("../data/clustering/cluster_words.json")

# -------- Highcharter theme & global options ---------
iconsize <- "fa-1x" # sidebar icon size

# WORD TABLES
table_sep <- " " # character to separate bigrams/trigrams in table view
bind_rev_numbers <- TRUE # whether or not to calculate "helpfulness" and "rating" of each word by joining with review stats 

# WORDCLOUDS
wordcloud_sep <- "_" # character to separate bigrams/trigrams in wordcloud view
monochrome <- TRUE # controls wordcloud coloring. If FALSE, default is to randomly choose colors from getOptions(highcharter.theme)$colors.
max_words_disp <- 50 # maximum words to display in wordclouds

# WORD BARCHARTS
top_words <- 10 # number of words to display in n-gram comparison barcharts

# COLORS & HIGHCHARTS OPTIONS
google_thm <- hc_theme_google()
tableau_cols <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
primary_col <- tableau_cols[1]
google_thm$colors <- tableau_cols
google_thm$xAxis$title$style$fontSize <- '18px'
google_thm$yAxis$title$style$fontSize <- '18px'
options(highcharter.theme = google_thm)
# -------------- END GLOBAL OPTIONS -----------------

# --------- UI components ---------
# Time series inputs (aggregation frequency, statistic (for brand overview & product comparison), and smoothing)
ts_UI <- function(inputId, with_stat = FALSE){
        UI <- tagList(radioButtons(inputId = paste0(inputId, "_ts_agg_freq"),
                                   label = "Aggregation frequency",
                                   choices = list("Monthly" = "month",
                                                  "Weekly" = "week"),
                                   selected = "month",
                                   inline = TRUE))
        if (with_stat){
                UI <- tagList(UI, 
                              radioButtons(inputId = paste0(inputId, "_ts_stat"),
                                           label = "Statistic",
                                           choices = list("Review count" = "n",
                                                          "Mean rating" = "mean_rating"),
                                           selected = "n",
                                           inline = TRUE)
                        )
        }
        UI <- tagList(UI,
                selectInput(inputId = paste0(inputId, "_ts_smoothing"),
                            label = "Smoothing",
                            choices = 0:6,
                            selected = 3)
                )
        return(UI)
}


smooth_ts_data <- function(x,p){
        # This function implements the smoothing method used by Google N-gram Viewer (https://books.google.com/ngrams/info).
        # For each value of the time series, the smoothed value is obtained by averaging the value plus its `p` prior and `p` subsequent values. 
        # Hence each smoothed value is a moving average applied to a window size of 1+2*n. Values at the start and end of the series are
        # averaged over fewer values. 
        # The data returned has the same length as the input data.
        stopifnot(is.numeric(p) & p%%1 == 0 & p >= 0)
        if (p == 0){
                return(x)
        } else {
                N <- length(x)
                x <- c(rep(NA, p), x, rep(NA, p))
                smoothed_x <- c()
                for (i in (1+p):(N+p)){
                        smoothed_x <- c(smoothed_x, mean(x[(i-p):(i+p)], na.rm = TRUE))
                }
                return(smoothed_x)   
        }
}

# experimenting with smoothing methods
# LOESS is not easily tunable by user input because the range of valid/numerically stable `span` parameters changes with dataset size.
# rollmean is easy to interpret and tune, but not very smooth & causes NA values at beginning and end
# Kernel smoothing is semi-tunable through the `bandwidth` parameter (no possible errors like with LOESS); however choice of bandwidth still 
# should depend on the data. It does produce smooth results though.
# Google's smoothing method is a small modification of rollmean.
# library(zoo)
# data <- sunspot.year
# data <- window(data, start = 1948, end = 1988)
# plot(data)
# lines(rollmean(data,10),col='red') # moving average
# lines(ksmooth(time(data),data,'normal',bandwidth=5),type='l',col='green') # kernel smoothing (Gaussian kernel)
# lines(as.vector(time(data)), loess(data ~ time(data), span = 0.5)$fitted) # LOESS
# lines(as.vector(time(data)), smoothing_ts_data(as.vector(data), 1)) # Google N-gram smoothing; a variation of moving average

get_preprocessing_stats <- function(parsedtxt) {
        words <- parsedtxt %>% group_by(doc_id) %>% summarize(num_words = n()) %>% .$num_words
        stats <- list(mean_words_per_doc = round(mean(words),1), 
                      median_words_per_doc = median(words),
                      vocab_size = length(unique(parsedtxt$word)),
                      number_of_documents = length(unique(parsedtxt$doc_id)))
        return(stats)
}
        
# Wordcloud2 has an issue with stopping other plots from rendering. Below is a "hacky" fix.
# Source: https://github.com/rstudio/shinydashboard/issues/281#issuecomment-615888981
# wordcloud2a <- function (data, size = 1, minSize = 0, gridSize = 0, fontFamily = "Segoe UI", 
#                          fontWeight = "bold", color = "random-dark", backgroundColor = "white", 
#                          minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE, 
#                          rotateRatio = 0.4, shape = "circle", ellipticity = 0.65, 
#                          widgetsize = NULL, figPath = NULL, hoverFunction = NULL) {
#         if ("table" %in% class(data)) {
#                 dataOut = data.frame(name = names(data), freq = as.vector(data))
#         }
#         else {
#                 data = as.data.frame(data)
#                 dataOut = data[, 1:2]
#                 names(dataOut) = c("name", "freq")
#         }
#         if (!is.null(figPath)) {
#                 if (!file.exists(figPath)) {
#                         stop("cannot find fig in the figPath")
#                 }
#                 spPath = strsplit(figPath, "\\.")[[1]]
#                 len = length(spPath)
#                 figClass = spPath[len]
#                 if (!figClass %in% c("jpeg", "jpg", "png", "bmp", "gif")) {
#                         stop("file should be a jpeg, jpg, png, bmp or gif file!")
#                 }
#                 base64 = base64enc::base64encode(figPath)
#                 base64 = paste0("data:image/", figClass, ";base64,", 
#                                 base64)
#         }
#         else {
#                 base64 = NULL
#         }
#         weightFactor = size * 180/max(dataOut$freq)
#         settings <- list(word = dataOut$name, freq = dataOut$freq, 
#                          fontFamily = fontFamily, fontWeight = fontWeight, color = color, 
#                          minSize = minSize, weightFactor = weightFactor, backgroundColor = backgroundColor, 
#                          gridSize = gridSize, minRotation = minRotation, maxRotation = maxRotation, 
#                          shuffle = shuffle, rotateRatio = rotateRatio, shape = shape, 
#                          ellipticity = ellipticity, figBase64 = base64, hover = htmlwidgets::JS(hoverFunction))
#         chart = htmlwidgets::createWidget("wordcloud2", settings, 
#                                           width = widgetsize[1], height = widgetsize[2], sizingPolicy = htmlwidgets::sizingPolicy(viewer.padding = 0, 
#                                                                                                                                   browser.padding = 0, browser.fill = TRUE))
#         chart
# }

get_all_ngrams <- function(data, max_ngram_length, within_sentence, repeats, with_pos = TRUE, with_sent = FALSE, bind_rev_numbers = TRUE){
        cols <- c("doc_id")
        if (within_sentence){
                cols <- c(cols, "sentence_id")
        }
        cols <- c(cols, "w1")
        if (with_pos){
                data <- data %>% rename(w1_pos = pos)
                cols <- c(cols, "w1_pos")
        }
        if (with_sent){
                data <- data %>% 
                        left_join(data.frame(lexicon::hash_valence_shifters), by = c("word" = "x")) %>%
                        left_join(get_sentiments("afinn"), by = "word") %>%
                        rename(w1_valType = y, w1_sent = value)
                cols <- c(cols, c("w1_valType", "w1_sent"))
        } 
        if (bind_rev_numbers){
                cols <- c(cols, c("stars", "helpful_yes", "helpful_no"))
        }
        
        data <- data %>% 
                rename(w1 = word) %>% 
                select(cols)
        
        all_ngrams <- list()
        for (ngram_length in 1:max_ngram_length){
                data_tmp <- data
                if (ngram_length > 1){
                        for (i in 1:(ngram_length-1)){
                                data_tmp[[ paste0("w", i+1) ]] <- lead(data_tmp$w1, i)
                                if (with_pos){
                                        data_tmp[[ paste0("w", i+1, "_pos") ]] <- lead(data_tmp$w1_pos, i)
                                }
                                if (with_sent){
                                        data_tmp[[ paste0("w", i+1, "_valType") ]] <- lead(data_tmp$w1_valType, i)
                                        data_tmp[[ paste0("w", i+1, "_sent") ]] <- lead(data_tmp$w1_sent, i)    
                                }
                                
                        }
                        
                        group_args <- if (within_sentence) c("doc_id", "sentence_id") else c("doc_id")
                        data_tmp <- data_tmp %>%
                                group_by_at(group_args) %>%
                                filter(row_number() <= (n() - ngram_length + 1)) %>%
                                ungroup()
                        
                        w_cols <- paste0("w", 1:ngram_length)
                        
                        if (!repeats){
                                data_tmp <- data_tmp[apply(data_tmp[,w_cols], MARGIN = 1, function(x){length(unique(x)) == ngram_length}),]
                        }
                        
                } 
                data_tmp <- data_tmp %>% select(-matches("sentence_id")) # drop sentence_id (if not already done because within_sentence==TRUE)
                all_ngrams <- c(all_ngrams, list(data_tmp))
        }
        
        return(all_ngrams)
}

filter_ngrams_pos <- function(data, ngram_length, valid_pos){
        data_n <- data[[ngram_length]]
        if (length(setdiff(c("NOUN", "ADJ", "VERB", "other"), valid_pos)) == 0){
                return(data_n)
        } 
        pos_cols <- paste0("w", 1:ngram_length, "_pos") # for POS filtering
        data_n <- data_n %>% filter_at(.vars = pos_cols, 
                                       .vars_predicate = all_vars(. %in% valid_pos | 
                                                                          (("other" %in% valid_pos) & !(. %in% c("NOUN", "ADJ", "VERB")))))
        return(data_n)
}

get_word_comparison_data <- function(data, top_words = 10){
        data <- data %>% 
                group_by(group, word) %>% 
                summarize(n = n()) %>%
                mutate(n = n/sum(n)) %>%
                ungroup() %>%
                complete(group, word, fill = list(n = 0)) %>% 
                spread(key = group, value = n) %>%
                mutate(diff = G2 - G1)
        
        G2_data <- data %>% 
                slice_max(n = top_words, order_by = diff, with_ties = FALSE) %>%
                select(word, diff)
        
        G1_data <- data %>% 
                arrange(desc(diff)) %>%
                slice_tail(n = top_words) %>%
                select(word, diff)
        
        G2_data2 <- rbind(G2_data, data.frame(word = G1_data$word, diff=rep(0, nrow(G1_data))))
        G1_data2 <- rbind(data.frame(word = G2_data$word, diff=rep(0, nrow(G2_data))), G1_data)
        
        categories <- G2_data2$word # equivalent to G1_data$word
        
        return(list(G1_data = G1_data2$diff, G2 = G2_data2$diff, categories = categories))
}

form_ngrams <- function(data, ngram_length, sep = "_"){
        w_cols <- paste0("w", 1:ngram_length)
        data[["word"]] <- apply(data[,w_cols, drop = FALSE], MARGIN = 1, FUN = function(x){paste(x, collapse = sep)}) 
        return(data)
}

form_all_ngrams <- function(data, sep = "_"){
        ngrams_formed <- data.frame()
        for (ngram_length in 1:length(data)){
                data_n <- data[[ngram_length]]
                data_n <- form_ngrams(data_n, ngram_length, sep = sep)
                ngrams_formed <- rbind(ngrams_formed, data_n %>% select(doc_id, word))
        }
        return(ngrams_formed)
}

get_word_counts <- function(data, bind_rev_numbers = TRUE){
        data <- data %>% 
                group_by(word)
        if (bind_rev_numbers){
                data <- data %>% summarize(n=n(),
                                           rating = round( mean(stars), 2),
                                           votes = sum(helpful_yes + helpful_no),
                                           helpful = round( sum(helpful_yes)/max(1,sum(helpful_yes + helpful_no)), 2 )
                                           ) 
        } else {
                data <- data %>% summarize(n=n())    
        }
        data
} 

make_wordtable <- function(data){
        DT::datatable(data, options = list(order=list(1, "desc")), rownames= FALSE, filter = "top")
}

make_wordcloud <- function(data, monochrome = TRUE, max_words_disp = 10){
        data <- data %>% 
                mutate(freq = log10(n)) %>%
                slice_max(order_by = freq, n = max_words_disp, with_ties = FALSE)
        
        if (monochrome){
                data$c <- unlist(lapply(1-(data$freq/max(data$freq))^2 , FUN = function(x) lighten(primary_col, amount = x)))   
                hc <- hchart(data, "wordcloud", hcaes(name = word, weight = freq, color = c))
        } else {
                hc <- hchart(data, "wordcloud", hcaes(name = word, weight = freq))
        }
        hc
}

get_cooccurrences <- function(data, d, within_sentence, sep = "_"){
        data <- data %>% rename(w0 = word, w0_pos = pos) 
        windowfunc <- ifelse(d > 0, lead, lag)
        data[["w1"]] <- windowfunc(data$w0, abs(d))
        data[["w1_pos"]] <- windowfunc(data$w0_pos, abs(d))
        
        group_args <- if (within_sentence) c("doc_id", "sentence_id") else c("doc_id")
        data <- data %>%
                group_by_at(group_args) %>%
                filter(((d > 0) & (row_number() <= (n() - 1))) | ((d < 0) & (row_number() >= 2))) %>%
                ungroup() %>%
                filter((w0_pos == "NOUN") & (w1_pos == "ADJ"))
        
        # if d < 0 then w1 comes before w0 so we need to paste in reverse order
        w_cols <- if (d > 0) c("w0", "w1") else c("w1", "w0")
        data[["word"]] <- apply(data[,w_cols], MARGIN = 1, FUN = function(x){paste(x, collapse = sep)}) # ngram
        
        return(data)
}

get_dep_parse <- function(data, sep = "_"){
        data <- data %>% inner_join(data %>% 
                                            rename(head_word = word, head_pos = pos) %>% 
                                            select(doc_id, sentence_id, token_id, head_word, head_pos) %>% 
                                            rename(head_token_id = token_id), by = c("doc_id", "sentence_id", "head_token_id")) %>%
                filter(dep_rel == "amod" & pos == "ADJ" & head_pos == "NOUN") %>%
                select(matches(c("doc_id", "word", "head_word", "stars", "helpful_yes", "helpful_no"))) %>%
                rename(w1 = word, w2 = head_word)
        
        w_cols <- c("w1", "w2")
        data[["word"]] <- apply(data[,w_cols], MARGIN = 1, FUN = function(x){paste(x, collapse = sep)}) # ngram
        
        return(data)
}

process_group_query <- function(data, group_query, agg_freq, normalization, start, end, smoothing){
        # Split group query into a list of queries (by splitting at commas). Also, remove extra whitespace
        group_query <- unlist(lapply(strsplit(group_query, split = ",")[[1]], FUN = str_squish))
        # convert to lowercase
        group_query <- tolower(group_query)
        # Remove empty entries (i.e. if original query was "cream, , taste" then the 2nd entry would be empty)
        group_query <- group_query[group_query != ""]
        
        # Find all words that match the group query.
        data <- data %>% filter(word %in% group_query)
        
        # Compute frequency, normalizing by total number of N-grams in each time period.  
        data <- data %>%
                group_by(!!as.name(agg_freq)) %>% 
                summarize(freq = n()) %>% 
                inner_join(normalization, by = agg_freq) %>%
                mutate(freq = freq/freq_all) %>%
                complete({{agg_freq}} := seq.Date(start, end, by=agg_freq), fill = list(freq=0)) %>% # Fill missing periods with 0.
                mutate(freq = smooth_ts_data(freq, p = smoothing)) # Smooth data
        
        # Convert to time series
        data <- as.xts(data$freq, data[[agg_freq]]) 
        data
}

js <- JS("
        var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\">' +
               '<h3>' + d[2] + '</h3>' +
               '<p>' + d[3] + '</p>' +
               '<h4> Ingredients: </h4>' +
               '<p>' + d[4] + '</p>' +
               '<img src=\"' + d[1] + '.png\" width=200 height=200> </div>';
        };
        table.on('click', 'td.details-control', function() {
        var td = $(this), row = table.row(td.closest('tr'));
        if (row.child.isShown()) {
          row.child.hide();
          td.html('\u2295');
        } else {
          row.child(format(row.data())).show();
          td.html('\u2296');
        }
        });
        ")