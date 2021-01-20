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

# Change these locations if you change file organization
# Note: must manually change PATH_TO_IMAGE in `js_prod_table` and `js_sentiment_table` below because string interpolation in R
# is inconvenient.
# setwd("C:/Users/tysonp/Desktop/data-science-projects/ice-cream-data-dashboard/shiny_app")
PATH_TO_APPDATA <- "www/appdata/"
PATH_TO_IMAGES <- "www/images/"

# Main dataset -- from ice-cream-dataset-v2+
prod_all <- read.csv(paste0(PATH_TO_APPDATA, "products.csv"), encoding = "UTF-8")
rev_all <- read.csv(paste0(PATH_TO_APPDATA, "reviews.csv"), encoding = "UTF-8")
rev_all <- rev_all %>% mutate(rev_id = row_number())

# Review counts in product dataset do not always match number of reviews in reviews dataset. Correct this
prod_all <- rev_all %>%
        group_by(key) %>% 
        summarize(rating_count=n(), rating=round(mean(stars), 1)) %>%
        ungroup() %>%
        right_join(prod_all %>% select(-c("rating_count", "rating")))

# Data for topic modeling
cluster_data_all <- read.csv(paste0(PATH_TO_APPDATA, "cluster_labelings.csv"), encoding = "UTF-8")
cluster_words_all <- fromJSON(paste0(PATH_TO_APPDATA, "cluster_words.json"))

# -------- GLOBAL OPTIONS & HIGHCHARTER THEME ---------
iconsize <- "fa-1x" # sidebar icon size

# WORD TABLES
table_sep <- " " # character to separate bigrams/trigrams in table view
bind_rev_numbers <- TRUE # whether or not to calculate "helpfulness" and "rating" of each word by joining with review stats 
table_max_words <- 1000

# WORDCLOUDS
wordcloud_sep <- "_" # character to separate bigrams/trigrams in wordcloud view
monochrome <- TRUE # controls wordcloud coloring. If FALSE, default is to randomly choose colors from getOptions(highcharter.theme)$colors.
wordcloud_max_words <- 50 # maximum words to display in wordclouds

# WORD BARCHARTS
barchart_max_words <- 10 # number of words to display in n-gram comparison barcharts

# COLORS & HIGHCHARTS OPTIONS
# Default thousands separator is a space, which (in my opinion) looks confusing.
hcoptslang <- getOption("highcharter.lang")
hcoptslang$thousandsSep <- "," 
options(highcharter.lang = hcoptslang)

# Colors and other plot settings
hctheme <- hc_theme_google()
color_theme <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
primary_col <- color_theme[1]
hctheme$colors <- color_theme
hctheme$title$align <- 'left' # title position
hctheme$subtitle$align <- 'left' # title position
hctheme$title$style$fontSize <- '18px' # title font size
hctheme$subtitle$style$fontSize <- '16px' # subtitle font size
hctheme$xAxis$title$style$fontSize <- '16px' # x label font size
hctheme$yAxis$title$style$fontSize <- '16px' # y label font size
hctheme$yAxis$opposite <- FALSE # If TRUE, time series "stock" graphs will place y ticks and y label on right side.
options(highcharter.theme = hctheme)
# -------------- END GLOBAL OPTIONS -----------------

# --------- UI COMPONENTS ---------
# Time series inputs (aggregation frequency, statistic (for brand overview & product comparison), and smoothing)
ts_UI <- function(inputId, with_stat = FALSE, smoothing_choice = 0){
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
                                           choices = list("Number of reviews" = "n",
                                                          "Average rating" = "mean_rating"),
                                           selected = "n",
                                           inline = TRUE)
                        )
        }
        UI <- tagList(UI,
                selectInput(inputId = paste0(inputId, "_ts_smoothing"),
                            label = "Smoothing",
                            choices = 0:6,
                            selected = smoothing_choice)
                )
        return(UI)
}
# ---------- END UI COMPONENTS --------------


# ----------- UTILITIES --------------
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

get_word_comparison_data <- function(data, max_words = 10){
        data <- data %>% 
                group_by(group, word) %>% 
                summarize(n = n()) %>%
                mutate(n = n/sum(n)) %>%
                ungroup() %>%
                complete(group, word, fill = list(n = 0)) %>% 
                spread(key = group, value = n) %>%
                mutate(diff = G2 - G1)
        
        G2_data <- data %>% 
                slice_max(n = max_words, order_by = diff, with_ties = FALSE) %>%
                select(word, diff)
        
        G1_data <- data %>% 
                arrange(desc(diff)) %>%
                slice_tail(n = max_words) %>%
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

make_wordtable <- function(data, max_words = FALSE){
        if (is.numeric(max_words)){
                data <- data %>% slice_max(order_by = n, n = max_words, with_ties = FALSE)
        }
        DT::datatable(data, selection = "none", options = list(order=list(1, "desc")), rownames= FALSE, filter = "top")
}

make_wordcloud <- function(data, monochrome = TRUE, max_words = 50){
        data <- data %>% 
                mutate(freq = log10(n)) %>%
                slice_max(order_by = freq, n = max_words, with_ties = FALSE)
        
        if (monochrome){
                # A somewhat arbitrary colormap
                # low exponent --> slow fade (most words are easily seen)
                # high exponent --> fast fade (only first few words are easily seen)
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

js_prod_table <- JS("
        var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\">' +
               '<h3>' + d[2] + '</h3>' +
               '<p>' + d[3] + '</p>' +
               '<h4> Ingredients: </h4>' +
               '<p>' + d[4] + '</p>' +
               '<img src=\"images/' + d[1] + '.png\" width=200 height=200> </div>';
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

js_sentiment_table <- JS("
        var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\">' +
               '<h3>' + d[1] + '</h3>' +
               '<h4> Review text: </h4>' +
               '<p>' + d[2] + '</p>' +
               '<h4> Words extracted: </h4>' +
               '<p>' + d[3] + '</p>';
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


# Make times series plots. A slightly quicker pipeline, with just the options we need.
# If any argument is left as FALSE then it takes on its default behavior.
make_ts_plot <- function(series_list, series_names = FALSE, xLab = FALSE, xFmt = "{value:%b. \'%y}", yLab = FALSE, yFmt = FALSE, title = FALSE, 
                         tooltip = "<b> {point.x:%b. %e, \'%y} </b> <br> {point.y}", formatter = FALSE, marker = FALSE){
        hc <- highchart(type = "stock") %>% 
                hc_rangeSelector(buttons = list(
                        list(type = 'month', count = 6, text = '6m'),
                        list(type = 'ytd', text = 'YTD'),
                        list(type = 'year', count = 1, text = '1y'),
                        list(type = 'year', count = 2, text = '2y'),
                        list(type = 'all', text = 'All')
                ))
        for (i in 1:length(series_list)){
                if (!is.logical(series_names)){
                        hc <- hc %>% hc_add_series(series_list[[i]], name = series_names[i], marker = list(enabled = marker))
                } else {
                        hc <- hc %>% hc_add_series(series_list[[i]], marker = list(enabled = marker)) 
                }
                
        }
        xOpts <- c(if (is.character(xLab)) list(title = list(text = xLab)) else list(), 
                   if (is.character(xFmt)) list(labels = list(format = xFmt)) else list())
        if (length(xOpts) > 0){
                hc <- hc %>% hc_xAxis(xOpts)
        }
        yOpts <- c(if (is.character(yLab)) list(title = list(text = yLab)) else list(), 
                   if (is.character(yFmt)) list(labels = list(format = yFmt)) else list())
        if (length(yOpts) > 0){
                hc <- hc %>% hc_yAxis_multiples(yOpts) # note the use of yAxis_multiples (https://github.com/jbkunst/highcharter/issues/79)
        }
        if (is.character(title)){
                hc <- hc %>% hc_title(text = title)
        }
        if (is.character(tooltip)){
                hc <- hc %>% hc_tooltip(headerFormat = "", pointFormat = tooltip)
        } else {
                if (is.character(formatter)){
                        hc <- hc %>% hc_tooltip(formatter = formatter, shared = FALSE, crosshairs = FALSE, split = FALSE)    
                }
        }
        hc
}

make_word_comparison_barchart <- function(x1, name1, x2, name2, categories, xLab, max_words = 10){
        highchart() %>%
                hc_add_series(x1, type = "bar", name = name1) %>%
                hc_add_series(x2, type = "bar", name = name2) %>%
                hc_xAxis(categories = categories,
                         labels = list(step = 1),
                         plotLines = list(
                                 list(color = "#808080",
                                      width = 1,
                                      value = max_words - 0.5,
                                      zIndex = 10)
                         )) %>%
                hc_yAxis(title = list(text = xLab), # we use xLab here because it LOOKS like the x-axis. It's hard to remember the axes are inverted.
                         plotLines = list(
                                 list(color = "#808080",
                                      width = 1,
                                      value = 0,
                                      zIndex = 10)
                         )) %>%
                hc_title(text = "N-gram usage comparison chart") %>%
                hc_plotOptions(series = list(stacking = "normal")) %>%
                hc_tooltip(
                        headerFormat = "",
                        pointFormat = "<b> {point.category} </b> <br> {point.y:.5f}"
                )
}

multiple_ts_tooltip <- "function() {
        var series = this.series.chart.series,
                point = this.point,
                s = '<span style=\"font-size: 10px\">' + Highcharts.dateFormat('%b. %e, %y', this.key) + '</span><br/>',
                tmp;

            $.each(series, function(i, serie){
                if (serie.name != 'Navigator 1'){
                        dot = '<span style=\"color:' + serie.color + '\">\u25CF</span> ';
                        var serieData = serie.data;
                        for (var j = 0; j < serieData.length; j++){
                                if (serieData[j].x == point.x){
                                        var idx = j;
                                }
                        }
                        tmp = serie.name + ': ' + (serieData[idx].y != null ? serieData[idx].y : 'NA');
        
                        if( serie.index === point.series.index )
                            tmp = '<b>' + tmp + '</b>';
                        
                        s += '<br/>' + dot + ' ' + tmp;
                }  
            });
            
            return s;
        }"


# Scrap work for plotting time series & formatting tooltip
# https://community.rstudio.com/t/highcharter-shared-tooltip-bolding-currently-hovered-series/30871/2
# my_tooltip <- "function() {
#             var series = this.series.chart.series,
#                 point = this.point,
#                 s = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>',
#                 tmp;
# 
#             $.each(series, function(i, serie){
#                 dot = '<span style=\"color:' + serie.color + '\">\u25CF</span> '
#                 tmp = serie.data[point.x].series.name + ': ' + serie.data[point.x].y + 'm';
# 
#                 if( serie.index === point.series.index )
#                     tmp = '<b>' + tmp + '</b>';
#                 
#                 s += '<br/>' + dot + ' ' + tmp;
#             });
#             
#             return s;
#         }"
# highcharts_demo() %>% 
#         hc_tooltip(formatter = JS(my_tooltip), shared = FALSE)
# 
# data <- rev_all %>% 
#         mutate(date = as.Date(date)) %>% 
#         mutate(month = floor_date(date, "month")) %>% 
#         count(month, brand) %>%
#         ungroup() %>%
#         complete(month = seq.Date(min(month), max(month), by = "month"), brand, fill = list(n=0))
# hchart(data, "line", hcaes(x = month, y = n, group = brand), marker = list(enabled = FALSE)) %>%
#         hc_xAxis(crosshair = TRUE) %>% hc_tooltip(shared=TRUE)
# hchart(data, "line", hcaes(x = month, y = n, group = brand), marker = list(enabled = FALSE)) %>%
#         hc_tooltip(formatter = JS(multiple_ts_tooltip))
# hc <- highchart(type = "stock")
# for (b in unique(data$brand)){
#         x <- data %>% filter(brand == b)
#         hc <- hc %>% hc_add_series(as.xts(x$n, x$month), name = b)
# }
# hc
# ------------ END UTILITIES -------------