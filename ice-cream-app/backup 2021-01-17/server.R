shinyServer(function(input, output) {
    # --------- BRAND OVERVIEW --------
    output$brandImage <- renderImage({
        imgSrc <- paste0("www/logo ", input$brand_in)
        if (input$brand_in %in% c("bj", "hd")){
            imgSrc <- paste0(imgSrc, ".png")
        }
        else {
            imgSrc <- paste0(imgSrc, ".jpg")
        }
        list(src = imgSrc, width="100", height="100", alt="brand image")
    }, deleteFile = FALSE)
    
    rev <- reactive({
        rev_all %>% 
            filter(brand == input$brand_in) %>% 
            mutate(date = as.Date(date, "%Y-%m-%d")) %>%
            mutate(week = lubridate::floor_date(date, "week"), month = lubridate::floor_date(date, "month")) %>% 
            left_join(prod()[,c("key", "name")], by = "key")
    })
    
    rev_dateRange <- reactive({
        data <- rev()
        list(date = list(start = min(data$date), end = max(data$date)),
            month = list(start = min(data$month), end = max(data$month)),
            week = list(start = min(data$week), end = max(data$week))
        )
    })
    
    prod <- reactive({
        prod_all %>% filter(brand == input$brand_in)
    })
    
    output$n_revs <- renderValueBox({ 
        valueBox(value = nrow(rev()), subtitle = "Number of reviews", icon = icon("comment"), color = "green")
    })
    output$n_prods <- renderValueBox({ 
        valueBox(value = nrow(prod()), subtitle = "Number of products", icon = icon("ice-cream"), color = "green")
    })
    output$rev_per_prod <- renderValueBox({ 
        valueBox(value = round(nrow(rev())/nrow(prod())), subtitle = "Average reviews/product", icon = icon("divide"), color = "green")
    })
    output$avg_rating <- renderValueBox({ 
        valueBox(value = round(mean(rev()$stars),2), subtitle = "Average review rating", icon = icon("thumbs-up"), color = "green")
    })
    
    top3_prod <- reactive({
        prod() %>% slice_max(order_by = rating_count, n = 3, with_ties = FALSE)
    })
    
    # Map saves us having to copy and paste this
    # https://stackoverflow.com/questions/35737029/how-to-generate-output-for-multi-plots-within-a-loop-in-shiny-app
    Map(function(i) {
        output[[paste0("top_prod_", i)]] <- renderImage({
            data <- top3_prod()
            imgSrc <- paste0("www/", data$key[i], ".png")
            list(src = imgSrc, width="170", height="170", alt="brand image")
        }, deleteFile = FALSE)
    }, 1:3)
    
    output$most_reviewed <- renderUI({
        most_reviewed_UI <- tagList()
        data <- top3_prod()
        for (i in 1:3){
            most_reviewed_UI <- tagList(most_reviewed_UI, column(width = 4,
                                                                 div(class = "prod-container", style = "width:100%; display:flex; justify-content:center;",
                                                                     imageOutput(paste0("top_prod_",i), width="170", height="170"),
                                                                     tags$div(class="prod-overlay",
                                                                              tags$div(data$name[i]), 
                                                                              tags$div(paste("Reviews:", data$rating_count[i])),
                                                                              tags$div(paste("Average rating:", data$rating[i])))
                                                                 )
            ))
        }
        most_reviewed_UI
    }) 
    
    output$overview_dist <- renderHighchart({

        x    <- rev()$stars
        bins <- seq(0.5, 5.5, length.out = 6)
        h <- hist(x, breaks = bins, plot = FALSE)
        
        hchart(h, breaks = bins) %>% 
            hc_yAxis(title = list(text = "Count")) %>% 
            hc_xAxis(title = list(text = "Star rating")) %>%
            hc_title(text = "Distribution of star ratings") %>%
            hc_tooltip(borderWidth = 1, sort = TRUE, crosshairs = TRUE,
                       headerFormat = "") %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
            )
    })

    output$overview_ts <- renderHighchart({
        dat <- rev()
        agg_freq <- input$overview_ts_agg_freq 
        stat <- input$overview_ts_stat
        smoothing <- as.numeric(input$overview_ts_smoothing)
        
        rev_by_date <- dat %>% 
            group_by(!!as.name(agg_freq)) %>%
            summarize(mean_rating = mean(stars), n = n())
        
        start <- min(rev_by_date[[agg_freq]])
        end <- max(rev_by_date[[agg_freq]])
        rev_by_date <- rev_by_date %>% complete( {{agg_freq}} := seq.Date(start, end, by=agg_freq), fill = list(n=0))
        
        x <- rev_by_date %>% select(c(agg_freq, stat))
        
        # hchart(x, "line", hcaes(x = !!as.name(agg_freq), y = !!as.name(stat)), marker = list(enabled = FALSE))
        
        if (smoothing > 0){
            x <- x %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        }
        
        
        x <- as.xts(x[[stat]], x[[agg_freq]]) 
        
        stat_label <- switch(stat, "n" = "Review count", "mean_rating" = "Mean rating")
        agg_label <- switch(agg_freq, "month" = "Month", "week" = "Week")
        hchart(x) %>%
            hc_rangeSelector( buttons = list(
                list(type = 'month', count = 6, text = '6m'),
                list(type = 'ytd', text = 'YTD'),
                list(type = 'year', count = 1, text = '1y'),
                list(type = 'year', count = 2, text = '2y'),
                list(type = 'all', text = 'All')
            )) %>%
            hc_yAxis(title = list(text = stat_label), opposite = FALSE) %>%
            hc_xAxis(title = list(text = agg_label)) %>% 
            hc_title(text = paste(stat_label, "per", agg_freq))
    })
    
    output$overview_by_prod <- renderHighchart({
        data <- prod()
        stat <- input$overview_by_prod_stat
        
        if (stat == "rating"){
            bins <- seq(0.5, 5.5, length.out = 6)
            h <- hist(data$rating, breaks = bins, plot = FALSE)
            hchart(h, breaks = bins)
        } else {
            hchart(data$rating_count)
        }
    })
    # --------- END BRAND OVERVIEW ---------
    
    # ----------- SETTINGS ---------------
    observeEvent(input$filter_words, {
        if(!input$filter_words){
            shinyjs::disable("filter_words_num")
        }else{
            shinyjs::enable("filter_words_num")
        }
    })
    
    # ------------ TEXT PREPROCESSING ---------------
    preprocessed_rev <- reactive({
        # Take dependency on input$submitButton by accessing it. We will then use isolate(...) to make sure this reactive only
        # updates when the settings button is pressed. Reference: https://shiny.rstudio.com/articles/isolation.html.
        input$submitButton

        # Isolating inputs from Settings form
        lemmatization <- isolate(input$lemmatization)
        standard_sw <- isolate(input$standard_sw)
        task_sw <- isolate(input$task_sw)
        filter_words <- isolate(input$filter_words)
        filter_words_num <- isolate(input$filter_words_num)
        max_tokens <- isolate(input$max_tokens)
        valence_shifters <- isolate(input$valence_shifters)
        
        # Recode user input for lemmatization: "No" --> "token", "Yes" --> "lemma"
        lemmatization <- ifelse(lemmatization, "lemma", "token")
        
        # Load parsed reviews
        parsedtxt <- read.csv("../data/parsed_reviews.csv", encoding = "UTF-8")
        
        # Rename token column (lemma/token) to "word" and drop other column
        parsedtxt <- parsedtxt %>%
            rename("word" = lemmatization) %>%
            select(-setdiff(c("token", "lemma"), lemmatization))
        
        # Subset based on brand 
        parsedtxt <- parsedtxt %>% filter(doc_id %in% rev()$rev_id)
        
        # Filter parsed reviews based on lemmatization choice
        parsedtxt <- parsedtxt %>%
            filter(grepl("^[a-z]+$",  word)) %>% # remove tokens/lemmas which consist of anything other than letters
            filter(nchar(word) >= 3) # remove short tokens/lemmas
        
        # Save statistics before filtering
        stats_before <- get_preprocessing_stats(parsedtxt)
        
        # ---------------WORD FILTERING------------------
        # 1. Standard English stopwords (see https://github.com/igorbrigadir/stopwords for a comprehensive database)
        standard_sw <- switch(standard_sw,
                              "None" = c(),
                              "Snowball (174 words)" = read.csv("../data/Snowball_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]],
                              "SMART (571 words)" = read.csv("../data/SMART_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]]
    
        )
        
        # 2. Task-specific stopwords
        if (length(task_sw) > 0){
            custom_sw <- read.csv("../data/task_sw_parsed.csv", encoding = "UTF-8")
            custom_sw <- custom_sw %>% 
                filter(brand == input$brand_in & doc_type %in% task_sw)
            custom_sw <- custom_sw[[lemmatization]]
        }
        else{
            custom_sw <- c()
        }
        
        # some additional words/misspellings to include in stopwords
        other_sw <- read.csv("../data/other_sw_parsed.csv", encoding = "UTF-8")[[lemmatization]]
        
        # combine all stopwords
        sw <- unique(c(standard_sw, custom_sw, other_sw))
        
        # 3. Words to exclude from stopwords (but not if filtering by document frequency)
        exclude <- c("natural", "flavor", "flavour", "creamy", "value", "down", "downward", "sweet", "super", "like", "ingredient", "ingredients",
                     "artificial", "flavoring", "heaven", "light", "extra", "layers", "add", "added", "free", "indulgences", "indulgence", "delight",
                     "delights", "style", "layered", "layer", "maintain", "freshness", "flavored", "flavors", "everything")
        sw <- setdiff(sw, exclude)
        
        # 4. Valence shifters
        # Keep valence shifters
        valShift <- lexicon::hash_valence_shifters$x
        sw <- setdiff(sw, valShift)
        
        parsedtxt <- parsedtxt %>% anti_join(data.frame(word=sw), by = "word")
        
        # 5. Filter by document frequency and limit vocab size to `max_tokens`
        L <- length(unique(parsedtxt$doc_id)) # total number of documents (that still have words after removing stopwords, etc.)
        
        # keep words
        kw <- parsedtxt %>%
            group_by(word) %>%
            summarise(termfreq = n(), docfreq = length(unique(doc_id))/L) 
        
        valShift_present <- intersect(kw$word, valShift)
        
        if (filter_words){
            kw <- kw %>% filter(filter_words_num[1] <= docfreq & docfreq <= filter_words_num[2])
        }
        
        kw <- kw %>% arrange(desc(docfreq), desc(termfreq))
        kw <- kw %>% anti_join(data.frame(word=valShift_present), by = "word")
        
        # with valence shifters
        kw1 <- kw %>% slice_head(n = max(max_tokens - length(valShift_present), 0)) %>% .$word
        kw1 <- union(kw1, valShift_present)
        
        # no valence shifters
        kw2 <- kw %>% slice_head(n = max_tokens) %>% .$word
        
        parsedtxt_val <- parsedtxt %>% filter(word %in% kw1)
        parsedtxt <- parsedtxt %>% filter(word %in% kw2)

        # Save statistics after filtering
        stats_after <- get_preprocessing_stats(if(valence_shifters) parsedtxt_val else parsedtxt)
        
        # Join with star ratings from review data
        cols <- c("rev_id", "stars")
        if (bind_rev_numbers){
            cols <- c(cols, c("helpful_yes", "helpful_no"))
        }
        parsedtxt_val <- parsedtxt_val %>% inner_join(rev_all %>% select(cols), c("doc_id" = "rev_id"))
        parsedtxt <- parsedtxt %>% inner_join(rev_all %>% select(cols), c("doc_id" = "rev_id"))
        
        return(list(rev_data = if(valence_shifters) parsedtxt_val else parsedtxt,
                    rev_valShift = parsedtxt_val,
                    stats_before = stats_before, 
                    stats_after = stats_after))
        # -------- END WORD FILTERING --------------------------
    })
    # -------------- END TEXT PREPROCESSING --------
    
    output$preprocessed_rev_table <- renderDataTable({
        stats_before <- preprocessed_rev()[["stats_before"]]
        stats_after <- preprocessed_rev()[["stats_after"]]
        data <- data.frame(x=unlist(stats_before, use.names = FALSE), y=unlist(stats_after, use.names = FALSE))
        rn <- c("Mean words per review", "Median words per reivew", "Vocabulary size", "Number of reviews")
        cn <- c("", "Before filtering", "After filtering")
        DT::datatable(data, options = list(searching = FALSE, paging = FALSE, bInfo = FALSE, ordering=F), rownames = rn, colnames = cn, selection = "none")
    })
    
    # output$preprocessed_rev_table <- renderUI({ 
    #     stats_before <- preprocessed_rev()[["stats_before"]]
    #     stats_after <- preprocessed_rev()[["stats_after"]]
    #     tags$table(style = "border: 1px solid black; padding: 1%; width: 100%; height: 252px;",
    #              tags$tr(
    #                  tags$th(),
    #                  tags$th("Before filtering"),
    #                  tags$th("After filtering")
    #              ),
    #              tags$tr(
    #                  tags$td("Mean words per review"),
    #                  tags$td(stats_before[["mean_words_per_doc"]]),
    #                  tags$td(stats_after[["mean_words_per_doc"]])
    #              ),
    #                tags$tr(
    #                    tags$td("Median words per review"),
    #                    tags$td(stats_before[["median_words_per_doc"]]),
    #                    tags$td(stats_after[["median_words_per_doc"]])
    #                ),
    #                tags$tr(
    #                    tags$td("Vocabulary size"),
    #                    tags$td(stats_before[["vocab_size"]]),
    #                    tags$td(stats_after[["vocab_size"]])
    #                ),
    #                tags$tr(
    #                    tags$td("Number of reviews"),
    #                    tags$td(stats_before[["number_of_documents"]]),
    #                    tags$td(stats_after[["number_of_documents"]])
    #                )
    #                
    #     )
    # })
    # -------------- END SETTINGS ------------
    
    # --------- BASIC STATS ------------
    output$counts_plot1 <- renderHighchart({
        data <- preprocessed_rev()[["rev_data"]]
        data <- data %>% 
            group_by(doc_id) %>%
            summarize(value = switch(input$count_stat,
                                     "Characters" = sum(nchar(word)),
                                     "Words" = n(),
                                     "Sentences" = length(unique(sentence_id)))
            )
        
        dataRange <- range(data$value)
        bins <- seq(dataRange[1] - 0.5, dataRange[2] + 0.5, length.out = dataRange[2] - dataRange[1] + 2)
        h <- hist(data$value, breaks = bins, plot = FALSE)
        
        count_stat <- input$count_stat
        hc <- hchart(h, breaks = bins) %>% 
            hc_yAxis(title = list(text = "Count")) %>% 
            hc_xAxis(title = list(text = count_stat)) %>%
            hc_title(text = paste("Distribution of", count_stat, "count")) %>%
            hc_tooltip(borderWidth = 1, sort = TRUE, crosshairs = TRUE,
                       headerFormat = "",
                       pointFormatter = JS("function() { return this.x;    
             }")) %>%
            hc_legend(enabled = FALSE) %>%
            hc_plotOptions(
                series = list(groupPadding = 0, pointPadding =  0, borderWidth = 0)
            )
        hc
    })
    
    output$counts_plot2 <- renderHighchart({
        data <- preprocessed_rev()[["rev_data"]]
        data <- data %>% 
            group_by(doc_id, stars) %>%
            summarize(value = switch(input$count_stat,
                                     "Characters" = sum(nchar(word)),
                                     "Words" = n(),
                                     "Sentences" = length(unique(sentence_id)))
            ) %>%
            group_by(stars) %>%
            summarize(stat = median(value))
        
        count_stat <- input$count_stat
        hc <- data %>%
            hchart('column', hcaes(x = stars, y = stat)) %>%
            hc_yAxis(title = list(text = "Count")) %>% 
            hc_xAxis(title = list(text = count_stat)) %>%
            hc_title(text = paste("Distribution of", count_stat, "count"))
        hc
        
    })
    
    output$diversity_table1 <- renderDataTable({
        data <- preprocessed_rev()[["rev_data"]]
        data <- data %>% 
            mutate(specie = as.numeric(as.factor(word))) 
        
        all_stats <- list(stars = "All",
                          nunq = length(unique(data$specie)),
                          nunq_dens = length(unique(data$specie))/nrow(data),
                          entropy = diversity(data$specie, index = "shannon"),
                          simpson = diversity(data$specie, index = "simpson"))
        all_stats <- c(all_stats, list(perplexity = 2^all_stats$entropy))
        
        by_star_stats <- data %>% 
            group_by(stars) %>%
            summarize(nunq = length(unique(specie)),
                      nunq_dens = length(unique(specie))/n(),
                      entropy = diversity(specie, index = "shannon"),
                      simpson = diversity(specie, index = "simpson")) %>%
            mutate(perplexity = 2^entropy)
        
        df_out <- rbind(all_stats, by_star_stats)
        df_out <- df_out[,c("stars","nunq", "nunq_dens", "entropy", "perplexity", "simpson")]
        names(df_out) <- c("Stars", "Unique words", "Unique words density", "Shannon entropy", "Perplexity", "Simpson index")
        
        DT::datatable(df_out, options = list(searching = FALSE, paging = FALSE, bInfo = FALSE, ordering=F), rownames= FALSE) %>% 
            formatRound(c(3,4,5), 3, mark = "") %>% formatRound(6, 6)
    })
    
    output$diversity_plot1 <- renderHighchart({
        data <- preprocessed_rev()[["rev_data"]]
        data <- data %>% 
            group_by(doc_id) %>%
            summarize(
            nunq = length(unique(word)),
            nunq_dens = length(unique(word))/n()
            )
        
        diversity_stat <- switch(input$diversity_stat, "Unique words" = "nunq", "Unique words density" = "nunq_dens") 
        x <- data[[diversity_stat]]
        hc <- hchart(density(x))
        hc
        
    })
    
    output$rank_freq <- renderHighchart({
        data <- preprocessed_rev()[["rev_data"]]
    
        # rank-frequency calculation
        data <- data %>%
            count(word) %>%
            arrange(desc(n)) %>%
            mutate(rank = row_number())
        
        hc <- data %>% hchart('scatter', hcaes(x = rank, y = n)) %>%
            hc_chart(zoomType = "x") %>%
            hc_xAxis(type = "logarithmic", tickInterval = 1, minRange = 1, title = list(text = "Rank")) %>%
            hc_yAxis(type = "logarithmic", title = list(text = "Frequency")) %>%
            hc_tooltip(borderWidth = 1, crosshairs = TRUE,
                       headerFormat = "",
                       pointFormatter = JS("function() {return this.word;}")
            )
        hc
    })
    
    output$rank_freq_wordcloud <- renderHighchart({
        data <- preprocessed_rev()[["rev_data"]]

        pos_input <- input$rank_freq_wordcloud_pos
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )

        if (length(setdiff(c("NOUN", "ADJ", "VERB", "other"), pos_input)) > 0){
            data <- data %>% filter(pos %in% pos_input | (("other" %in% pos_input) & !(pos %in% c("NOUN", "ADJ", "VERB"))))
        } 
        
        data <- data %>% count(word) 

        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp)
        # wordcloud2a(data = data, size = 0.25, shape = "circle",
        #             color = sample(getOption("highcharter.theme")$colors, size = max_words_disp, replace = TRUE))
    })
    
    # ----------- END BASIC STATS --------
    
    # ---------- WORD INSIGHTS ---------
    all_ngrams_reactive <- reactive({
        data <- preprocessed_rev()[["rev_data"]]
        within_sentence <- input$within_sentence
        repeats <- input$ngram_repeats
        return( get_all_ngrams(data, max_ngram_length=3, within_sentence=within_sentence, repeats=repeats, 
                               with_pos = TRUE, with_sent = FALSE, bind_rev_numbers = bind_rev_numbers) )
    }) 
    
    output$ngram_table <- renderDataTable({
        
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos
        
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data)
    })
    
    output$ngram_wordcloud <- renderHighchart({
        
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos
        
        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one choice.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp) 
    })
    
    output$pos_neg_comparison <- renderHighchart({
        ngram_length <- as.integer(input$ngram_length)
        pos_input <- input$ngram_pos

        shiny::validate(
            need( length(intersect(pos_input, c("NOUN", "ADJ", "VERB", "other"))) > 0, 
                  "Please select at least one input.")
        )
        
        ngram_data <- all_ngrams_reactive()
        data <- filter_ngrams_pos(ngram_data, ngram_length, pos_input)
        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)

        data <- data %>% select(doc_id, word) %>% inner_join(rev() %>% select(rev_id, stars), by = c("doc_id" = "rev_id"))
        data <- data %>% mutate(group = case_when(stars < 5 ~ "G1", TRUE ~ "G2"))
        
        data <- get_word_comparison_data(data = data, top_words = top_words)
        
        highchart() %>%
            hc_add_series(data$G2, type = "bar", name = "positive") %>%
            hc_add_series(data$G1, type = "bar", name = "negative") %>%
            hc_xAxis(categories = data$categories,
                     labels = list(step = 1),
                     plotLines = list(
                         list(color = "#000000",
                              width = 2,
                              value = top_words - 0.5)
                     )) %>%
            hc_plotOptions(series = list(stacking = "normal")) %>%
            hc_yAxis(
                labels = list(
                    formatter = JS("function(){return Math.abs(this.value);}")
                )
            ) %>%
            hc_tooltip(
                shared = FALSE,
                formatter = JS("function () {
            return this.point.category + '<br/>' + 
            '<b>' + this.series.name + '</b> ' + 
            Highcharts.numberFormat(Math.abs(this.point.y), 1);}")
            )
    })
    
    output$ngram_tracker <- renderHighchart({
        input$ngram_button
        queries <- c(isolate(input$group1), isolate(input$group2), isolate(input$group3))
        agg_freq <- isolate(input$ngram_ts_agg_freq)
        smoothing <- as.integer(isolate(input$ngram_ts_smoothing))
        
        ngram_data <- all_ngrams_reactive()
        
        ngrams_formed <- form_all_ngrams(ngram_data, sep = " ")
        
        data <- ngrams_formed %>% inner_join(rev() %>% select(c("rev_id", agg_freq)), by = c("doc_id" = "rev_id"))
        normalization <- data %>% group_by(!!as.name(agg_freq)) %>% summarize(freq_all = n())
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        
        x <- lapply(queries, function(q) process_group_query(data, q, agg_freq, normalization, start, end, smoothing))
        
        highchart(type = "stock") %>%
            hc_add_series(x[[1]]) %>%
            hc_add_series(x[[2]]) %>%
            hc_add_series(x[[3]]) %>%
            hc_rangeSelector( buttons = list(
                list(type = 'month', count = 6, text = '6m'),
                list(type = 'ytd', text = 'YTD'),
                list(type = 'year', count = 1, text = '1y'),
                list(type = 'year', count = 2, text = '2y'),
                list(type = 'all', text = 'All')
            ))
    })
    
    output$cooccurrences_table <- renderDataTable({
        
        cooccurrence_distance <- as.numeric(input$cooccurrence_distance)
        within_sentence <- input$within_sentence
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_cooccurrences(data, cooccurrence_distance, within_sentence, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data)
    })
    
    output$cooccurrences_wordcloud <- renderHighchart({
        
        cooccurrence_distance <- as.numeric(input$cooccurrence_distance)
        within_sentence <- input$within_sentence
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_cooccurrences(data, cooccurrence_distance, within_sentence, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp)
    })
    
    output$dep_parse_table <- renderDataTable({
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_dep_parse(data, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data)
    })
    
    output$dep_parse_wordcloud <- renderHighchart({
        
        data <- preprocessed_rev()[["rev_data"]]
        
        data <- get_dep_parse(data, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp)
    })
    # ---------- END WORD INSIGHTS ------

    # ---------- SENTIMENT ANALYSIS -------
    output$sentiment_radar <- renderHighchart({
        data <- preprocessed_rev()[["rev_valShift"]]
        
        data <- data %>% count(word)
        
        nrc_sent <- get_sentiments("nrc")
        
        data <- data %>% inner_join(nrc_sent, by = "word") %>% group_by(sentiment) %>% summarize(n_sent=sum(n))
        
        hc <- highchart() %>% 
            hc_chart(polar = TRUE) %>% 
            hc_title(text = "Sentiment types") %>% 
            hc_xAxis(categories = data$sentiment,
                     tickmarkPlacement = "on",
                     lineWidth = 0) %>% 
            hc_yAxis(gridLineInterpolation = "polygon",
                     lineWidth = 0,
                     min = 0) %>% 
            hc_series(
                list(
                    name = "All products",
                    data = data$n_sent,
                    pointPlacement = "on",
                    type = "area"
                )
            )
        hc
        
    })
        
    rev_sentiment <- reactive({
        data <- preprocessed_rev()[["rev_valShift"]]
        
        reform_sentences <- data %>% group_by(doc_id, sentence_id) %>% summarize(sentence=paste0(paste(word, collapse=' '), "."))  
        reform_docs <- reform_sentences %>% group_by(doc_id) %>% summarize(doc=paste0(sentence, collapse=' '))
        sentiment_sentences <- sentimentr::get_sentences(reform_docs)
        sentiment_scores <- sentiment_sentences %>% 
            sentimentr::sentiment_by(by = "doc_id") %>% 
            mutate(ave_sentiment = round(ave_sentiment, 3)) %>% 
            arrange(desc(ave_sentiment)) %>%
            inner_join(reform_docs %>% select(doc_id, doc), by = "doc_id")  %>%
            inner_join(rev() %>% select(rev_id, text, stars, date, month, week), by = c("doc_id" = "rev_id") )
        return(sentiment_scores)
    })
    
    output$sentiment_table <- renderDataTable({
        
        data <- rev_sentiment() %>% select(-c("month", "week"))
        
        DT::datatable(data, rownames = FALSE)
    })
    
    output$sent_dist <- renderHighchart({
        data <- rev_sentiment()
        data <- density(data$ave_sentiment)
        hc <- hchart(data)
        hc
    })
    
    output$sent_dist_by_stars <- renderHighchart({
        data <- rev_sentiment()
        hc <- hcboxplot(
            x = data$ave_sentiment,
            var = data$stars,
            outliers = FALSE
        ) %>% 
            hc_chart(type = "column")
        hc
    })
    
    output$sent_ts <- renderHighchart({
        data <- rev_sentiment()
        agg_freq <- input$sent_ts_agg_freq
        smoothing <- as.integer(input$sent_ts_smoothing)
        data <- data %>% group_by(!!as.name(agg_freq)) %>% summarize(y = mean(ave_sentiment))
        data <- data %>% mutate(y = smooth_ts_data(y, p = smoothing))
        data <- as.xts(data$y, data[[agg_freq]]) 
        
        highchart(type = "stock") %>%
            hc_add_series(data) %>%
            hc_rangeSelector( buttons = list(
                list(type = 'month', count = 6, text = '6m'),
                list(type = 'ytd', text = 'YTD'),
                list(type = 'year', count = 1, text = '1y'),
                list(type = 'year', count = 2, text = '2y'),
                list(type = 'all', text = 'All')
            ))
    })
    
    output$num_valShift <- renderText({
        data <- preprocessed_rev()[["rev_valShift"]]
        data <- nrow(data %>% filter(word %in% lexicon::hash_valence_shifters$x))/nrow(rev())
        paste("Average number of valence shifters per review:", round(data, 3))
    })
    
    all_ngrams_valShift_reactive <- reactive({
        data <- preprocessed_rev()[["rev_valShift"]]
        within_sentence <- input$within_sentence
        repeats <- input$ngram_repeats
        return( get_all_ngrams(data, max_ngram_length=3, within_sentence=within_sentence, repeats=repeats, 
                               with_pos = FALSE, with_sent = TRUE, bind_rev_numbers = bind_rev_numbers) )
    }) 
    
    output$ngram_valShift_table <- renderDataTable({
        
        ngram_length <- as.integer(input$ngram_length_valShift)
        valShift_type <- as.integer(input$valShift_type)
        
        shiny::validate(need(length(intersect(valShift_type, 1:4)) > 0, "Please select at least one choice."))
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain valence shifters
        valType_cols <- paste0("w", 1:ngram_length, "_valType")
        data <- data %>% filter_at(.vars = valType_cols, .vars_predicate = any_vars(. %in% valShift_type))
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data)
    })
    
    output$ngram_valShift_wordcloud <- renderHighchart({
        
        ngram_length <- as.integer(input$ngram_length_valShift)
        valShift_type <- as.integer(input$valShift_type)
        
        shiny::validate(need(length(intersect(valShift_type, 1:4)) > 0, "Please select at least one choice."))
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain valence shifters
        valType_cols <- paste0("w", 1:ngram_length, "_valType")
        data <- data %>% filter_at(.vars = valType_cols, .vars_predicate = any_vars(. %in% valShift_type))
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp) 
    })
    
    output$ngram_polarity_table <- renderDataTable({
        
        ngram_length <- as.integer(input$ngram_length_valShift)
        polarity_range_filter <- input$polarity_range_filter
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain a sentimental word in the specified polarity range
        sent_cols <- paste0("w", 1:ngram_length, "_sent")
        data <- data %>% filter_at(.vars = sent_cols, .vars_predicate = any_vars( (. >= polarity_range_filter[1]) &
                                                                                  (. <= polarity_range_filter[2]) )
                                   )

        data <- form_ngrams(data, ngram_length = ngram_length, sep = table_sep)
        data <- get_word_counts(data, bind_rev_numbers = bind_rev_numbers)
        
        make_wordtable(data)
    })
    
    output$ngram_polarity_wordcloud <- renderHighchart({
        
        ngram_length <- as.integer(input$ngram_length_valShift)
        polarity_range_filter <- input$polarity_range_filter
        
        data <- all_ngrams_valShift_reactive()[[ngram_length]]
        
        # only keep ngrams that contain a sentimental word in the specified polarity range
        sent_cols <- paste0("w", 1:ngram_length, "_sent")
        data <- data %>% filter_at(.vars = sent_cols, .vars_predicate = any_vars( (. >= polarity_range_filter[1]) &
                                                                                      (. <= polarity_range_filter[2]) )
        )
        
        data <- form_ngrams(data, ngram_length = ngram_length, sep = wordcloud_sep)
        data <- get_word_counts(data, bind_rev_numbers = FALSE)
        
        make_wordcloud(data, monochrome = monochrome, max_words_disp = max_words_disp) 
    })
    
    # -------- END SENTIMENT ANALYSIS -------
    
    # --------- TOPIC MODEL -------------
    output$num_clusters <- renderUI({
        # `choices` are 5 or 10.
        # `selected` defaults to 5 on first render, and afterwards defaults to whatever the current selection is (unless cluster_method == manual),
        # If cluster_method == manual, the only valid selection is 5.
        # If this logic isn't included then every time the user changes the cluster_method, the num_clusters selection will *RESET* to
        # whatever the default is, i.e. 5.
        selected <- if (!is.null(input$num_clusters) & input$cluster_method != "manual") input$num_clusters else 5

        uiOut <-  selectInput(inputId = "num_clusters",
                    label = "Number of clusters",
                    choices = c(5, 10),
                    selected = selected
                    )
        if(input$cluster_method == "manual"){
            return(shinyjs::disabled(uiOut))
        }else{
            return(uiOut)
        }
    })
    
    cluster_data <- reactive({
        cluster_data_all %>% filter(brand == input$brand_in)
    })
    
    output$topic_model_vectorspace <- renderHighchart({
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- isolate(input$num_clusters)
        
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")

        hc <- hchart(cluster_data(), "scatter", hcaes(x = X1, y = X2, group = !!as.name(col_name)))
        hc
    })
    
    output$cluster_size_dist <- renderHighchart({
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- isolate(input$num_clusters)
        
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")
        data <- cluster_data() %>% count(!!as.name(col_name))
        
        hc <- hchart(data, "column", hcaes(x = !!as.name(col_name), y = n))
        hc
    })
    
    output$cluster_size_ts <- renderHighchart({
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- isolate(input$num_clusters)
        agg_freq <- input$cluster_size_ts_agg_freq
        smoothing <- as.integer(input$cluster_size_ts_smoothing)
        
        data <- cluster_data() %>% inner_join(rev() %>% select(c("rev_id", agg_freq)), by = c("doc_id" = "rev_id"))
        col_name <- paste("y", cluster_method, num_clusters, sep = "_")
        data <- data %>% group_by(!!as.name(agg_freq)) %>% count(!!as.name(col_name)) %>% ungroup()
        
        start <- rev_dateRange()[[agg_freq]]$start
        end <- rev_dateRange()[[agg_freq]]$end
        data <- data %>% complete({{agg_freq}} := seq.Date(start, end, by=agg_freq), !!as.name(col_name), fill = list(n=0))
        
        hc <- highchart(type = "stock") 
        for (i in 0:(as.integer(num_clusters)-1)){
            data_i <- data %>% filter(!!as.name(col_name) == i)
            data_i <- as.xts(data_i$n, data_i[[agg_freq]]) 
            hc <- hc %>% hc_add_series(data_i)
        }
        hc <- hc %>%
            hc_rangeSelector(buttons = list(
                list(type = 'month', count = 6, text = '6m'),
                list(type = 'ytd', text = 'YTD'),
                list(type = 'year', count = 1, text = '1y'),
                list(type = 'year', count = 2, text = '2y'),
                list(type = 'all', text = 'All')
            ))
        hc
    })
    
    output$topic_model_words <- renderDataTable({
        req(input$topic_button) # req() ensures that this code is only run after the button is clicked
        cluster_method <- isolate(input$cluster_method)
        num_clusters <- isolate(input$num_clusters)
        brand <- input$brand_in
        
        df_name <- paste(brand, cluster_method, num_clusters, sep = "_")
        cluster_words <- cluster_words_all[[df_name]]
        
        # note: the column names are originally numeric; however, R does not allow names to start with a number,
        # so we convert the column names to c(c1,c2,c3,...)
        cluster_words <- as.data.frame(cluster_words, col.names = paste0("c", names(cluster_words)))
        
        DT::datatable(cluster_words, options = list(scrollX = TRUE), rownames= FALSE)
    })
    # ----------- END TOPIC MODELING -------------
    
    # ------------- PRODUCT LEADERBOARD ----------------
    output$prod_leaderboard <- renderDataTable({
        data <- rev() 
        data <- data %>% 
            group_by(key) %>% 
            summarize(n = n(),
                      mean_rating = mean(stars),
                      sd_rating = sd(stars),
                      first_review = min(date),
                      last_review = max(date)) %>%
            ungroup() %>%
            right_join(prod()[,c("key", "name", "description", "ingredients")], by = "key") # right join so that if a product has 0 reviews it will still be kept in the datatable
        
        data <- data[,c("key","name","description","ingredients","n","mean_rating","sd_rating","first_review","last_review")]
        data <- cbind(' ' = '\u2295', data)
        
        DT::datatable(data,
                      rownames = FALSE,
                      filter = "top",
                      selection = "none",
                      options = list(
                          order = list(2, "asc"),
                          drawCallback = JS("function() {
                          this.api().table().column(0).nodes().to$().css({'cursor': 'pointer', 'font-size': '18px' });
                          }"),
                          columnDefs = list(
                              list(visible = FALSE, targets = c(1,3,4)),
                              list(orderable = FALSE, className = 'details-control', targets = 0)
                          )
                      ),
                      callback = js) %>% formatRound(c(7,8), 2) 
    })
    # ------------ END PRODUCT LEADERBOARD -----------
    
    # -------- PRODUCT TRENDS ----------
    output$prod_trends_table <- renderDataTable({
        data <- rev()
        trends_month_range <- input$trends_month_range
        trends_column_options <- input$trends_column_options
        
        # Split reviews into last N months (date group 1) and prior N months (date group 0), where N = input$trends_month_range
        # I.e. if N = 3 and the last review was written on 2020-09-24, then date group 1 is (2020-06-24, 2020-09-24] 
        # and date group 0 is (2020-03-24, 2020-06-24]
        last_date <- max(data$date)
        split_date0 <- last_date - 2*months(trends_month_range)
        split_date1 <- last_date - months(trends_month_range)
        gp0_range <- c(split_date0, split_date1)
        gp1_range <- c(split_date1, last_date)
        
        # Encode date group 
        data <- data %>%
            mutate(date_group = factor( case_when( ((gp0_range[1] < date)&(date <= gp0_range[2])) ~ 0,
                                                   ((gp1_range[1]< date)&(date <= gp1_range[2])) ~ 1) )
            )
        
        # Calculate summary statistics for each date group
        data <- data %>%
            group_by(key, date_group) %>%
            summarize(n = n(), 
                      mean_rating = mean(stars),
                      sd_rating=sd(stars)) %>%
            ungroup() 
        
        # If a product has no reviews AT ALL, it won't appear. Let's add these to our dataset -- initializing with NAs
        missing <- setdiff(prod()$key, data$key)
        data <- data %>% bind_rows(data.frame(key = missing))
        
        # Fill values for products that have no reviews in either or both time windows, but does have at least
        data <- data %>% 
            complete(key, date_group, fill = list(n = 0, mean_rating = NA, sd_rating = NA)) %>%
            drop_na(date_group)
        
        # If only 1 review, sd returns NA but it is more appropriate to say sd = 0
        data <- data %>% mutate(sd_rating = case_when(n == 1 ~ 0, TRUE ~ sd_rating))
        
        data <- inner_join(data %>% filter(date_group == 0) %>% select(-date_group), 
                           data %>% filter(date_group == 1) %>% select(-date_group), 
                           by = "key", suffix = c(".0", ".1"))
        
        stats <- c("n", "mean_rating", "sd_rating")
        cols <- c("key")
        round_to <- 3
        for (stat in stats){
            # always include n.0 and n.1 even if user does not want raw values for mean & sd
            if (("value" %in% trends_column_options) | (stat == "n")){
                col0 <- paste0(stat, ".0")
                col1 <- paste0(stat, ".1")
                data[!is.na(data[[col0]]), col0] <- round(data[!is.na(data[[col0]]), col0], round_to)
                data[!is.na(data[[col1]]), col1] <- round(data[!is.na(data[[col1]]), col1], round_to)
                cols <- c(cols, paste0(stat, c(".0", ".1")))
            }
            if ("diff" %in% trends_column_options){
                data[[paste0(stat, "_diff")]] <- round(data[[paste0(stat, ".1")]] - data[[paste0(stat, ".0")]], round_to) # difference
                cols <- c(cols, paste0(stat, "_diff"))
            }
            if ("rel" %in% trends_column_options){
                data[[paste0(stat, "_rel")]] <- round((data[[paste0(stat, ".1")]] - data[[paste0(stat, ".0")]])/data[[paste0(stat, ".0")]], round_to) # percent change
                cols <- c(cols, paste0(stat, "_rel"))
            }
        }
        
        DT::datatable(data[,cols])
    })
    # ----------- END PRODUCT TRENDS -----
    
    
    # ---------- PRODUCT COMPARISON ---------
    output$prod_1 <- renderUI({
        data <- prod() %>% arrange(name)
        choices <- data$key
        names(choices) <- paste0(data$name, " (", data$rating_count, " reviews)")
        selected <- data$key[1]
        
        div(class = "prod_input",
            selectInput(inputId = "prod_1",
                        label = "Product 1",
                        choices = choices,
                        selected = selected
            )
        )
    })
    
    output$prod_2 <- renderUI({
        req(input$prod_1)
        data <- prod() %>% arrange(name)
        choices <- c("all", data$key)
        n_revs_without_prod1 <- sum(data[data$key != input$prod_1, "rating_count"])
        names(choices) <- c(paste0("All (", n_revs_without_prod1, " reviews)"), 
                            paste0(data$name, " (", data$rating_count, " reviews)"))
        selected <- ifelse(is.null(input$prod_2), "all", input$prod_2)
        
        div(class = "prod_input",
            selectInput(inputId = "prod_2",
                        label = "Product 2",
                        choices = choices,
                        selected = selected
            )
        )
    })
    
    output$prod_1_img <- renderImage({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        imgSrc <- paste0("www/", key1, ".png")
        
        list(src = imgSrc, width="170", height="170", alt="brand image")
    }, deleteFile = FALSE)
    
    output$prod_2_img <- renderImage({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        if (key2 == "all"){
            imgSrc <- paste0("www/logo ", input$brand_in)
            if (input$brand_in %in% c("bj", "hd")){
                imgSrc <- paste0(imgSrc, ".png")
            }
            else {
                imgSrc <- paste0(imgSrc, ".jpg")
            }
        }
        else {
            imgSrc <- paste0("www/", key2, ".png")
        }
        
        list(src = imgSrc, width="170", height="170", alt="brand image")
    }, deleteFile = FALSE)
    
    output$prod_comp_table <- renderDataTable({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% 
            mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2)) %>%
            group_by(prod_group) %>%
            summarise(n = n(), mean_rating = mean(stars), sd_rating = sd(stars), first_review=min(date), last_review = max(date)) %>%
            ungroup() %>%
            select(-prod_group) %>%
            t()
        
        DT::datatable(data, options = list(searching = FALSE, paging = FALSE, bInfo = FALSE, ordering=F), colnames = c("Statistic", "Product 1", "Product 2"))
    })
    
    output$prod_comp_dist <- renderHighchart({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2))
        
        data <- data %>% 
            group_by(prod_group, stars) %>% 
            summarize(n = n()) %>%
            mutate(n = n/sum(n)) %>%
            ungroup() %>%
            complete(stars, prod_group, fill = list(n = 0))
        
        hchart(data, type = "column", hcaes(x = stars, group = prod_group, y = n))
    })
    
    output$prod_comp_ts <- renderHighchart({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        smoothing <- as.numeric(input$prod_ts_smoothing)
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        data <- rev()
        
        agg_freq <- input$prod_ts_agg_freq 
        stat <- input$prod_ts_stat
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2))
        
        data <- data %>% 
            group_by(prod_group, !!as.name(agg_freq)) %>%
            summarize(mean_rating = mean(stars), n = n()) %>%
            mutate(n = n/sum(n)) %>%
            ungroup()
        
        start <- min(data[[agg_freq]])
        end <- max(data[[agg_freq]])
        data <- data %>% complete( {{agg_freq}} := seq.Date(start, end, by=agg_freq), prod_group, fill = list(n=0))
        
        x1 <- data %>% filter(prod_group == 1)
        x2 <- data %>% filter(prod_group == 2)
        
        
        if (smoothing > 0){
            x1 <- x1 %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
            x2 <- x2 %>% mutate({{stat}} := smooth_ts_data(!!as.name(stat), p = smoothing))
        }
        
        x1 <- as.xts(x1[[stat]], x1[[agg_freq]]) 
        x2 <- as.xts(x2[[stat]], x2[[agg_freq]]) 
        
        stat_label <- switch(stat, "n" = "Review count", "mean_rating" = "Mean rating")
        agg_label <- switch(agg_freq, "month" = "Month", "week" = "Week")
        highchart(type = "stock") %>%
            hc_add_series(x1, marker = list(enabled = TRUE)) %>%
            hc_add_series(x2, marker = list(enabled = TRUE)) %>%
            hc_rangeSelector( buttons = list(
                list(type = 'month', count = 6, text = '6m'),
                list(type = 'ytd', text = 'YTD'),
                list(type = 'year', count = 1, text = '1y'),
                list(type = 'year', count = 2, text = '2y'),
                list(type = 'all', text = 'All')
            ))
    })
    
    output$prod_comp_ngrams <- renderHighchart({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        ngram_length <- as.integer(input$prod_comp_ngram_length)
        
        data <- all_ngrams_reactive()[[ngram_length]]
        data <- form_ngrams(data, ngram_length, sep = table_sep)
        
        data <- data %>% inner_join(rev() %>% select("rev_id","key"), by = c("doc_id" = "rev_id"))
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        
        data <- data %>% mutate(group = case_when(key == key1 ~ "G1", TRUE ~ "G2"))
        data <- get_word_comparison_data(data = data, top_words = top_words)
        
        highchart() %>%
            hc_add_series(data$G1, type = "bar", name = "Product 1") %>%
            hc_add_series(data$G2, type = "bar", name = "Product 2") %>%
            hc_xAxis(categories = data$categories,
                     labels = list(step = 1),
                     plotLines = list(
                         list(color = "#000000",
                              width = 2,
                              value = top_words - 0.5)
                     )) %>%
            hc_plotOptions(series = list(stacking = "normal")) %>%
            hc_yAxis(
                labels = list(
                    formatter = JS("function(){return Math.abs(this.value);}")
                )
            ) %>%
            hc_tooltip(
                shared = FALSE,
                formatter = JS("function () {
            return this.point.category + '<br/>' + 
            '<b>' + this.series.name + '</b> ' + 
            Highcharts.numberFormat(Math.abs(this.point.y), 1);}")
            )
    })
    
    output$prod_comp_sentiment <- renderHighchart({
        # Note: Upon switching `brand` input, an error message will flash (for 1/2 a sec) due to reactives being updated out of order.
        # Specifically, the plot here is trying to update before the renderUI for input$prod_1 and input$prod_2 is complete.
        # Reference:  https://stackoverflow.com/questions/50340172/how-do-i-avoid-flashing-errors-in-shiny-r-plot
        # A hacky fix:  req(input$prod_1 %in% prod()$key) will ensure that the renderUI finishes before this plot
        req(input$prod_1 %in% prod()$key)
        req(input$prod_2 %in% c("all",prod()$key))
        key1 <- input$prod_1
        key2 <- input$prod_2
        shiny::validate(need(key1 != key2, message = "Choose two different products"))
        
        data <- preprocessed_rev()[["rev_valShift"]] %>% inner_join(rev() %>% select("rev_id","key"), by = c("doc_id" = "rev_id"))
        
        if (key2 != "all"){
            data <- data %>% filter(key %in% c(key1, key2)) 
        }
        data <- data %>% mutate(prod_group = case_when(key == key1 ~ 1, TRUE ~ 2))
        
        data <- data %>% 
            group_by(prod_group, word) %>% 
            summarize(n = n()) %>%
            ungroup() %>%
            complete(prod_group, word, fill = list(n = 0))
        
        nrc_sent <- get_sentiments("nrc")
        data <- data %>% 
            inner_join(nrc_sent, by = "word") %>% 
            group_by(prod_group, sentiment) %>% 
            summarize(n_sent=sum(n)) %>% 
            mutate(n_sent = n_sent/sum(n_sent)) %>%
            ungroup()
        
        hc <- hchart(data, type = "column", hcaes(x = sentiment, group = prod_group, y = n_sent))
        # hc <- highchart() %>% 
        #     hc_chart(polar = TRUE) %>% 
        #     hc_title(text = "Sentiment types") %>% 
        #     hc_xAxis(categories = unique(data$sentiment),
        #              tickmarkPlacement = "on",
        #              lineWidth = 0) %>% 
        #     hc_yAxis(gridLineInterpolation = "polygon",
        #              lineWidth = 0,
        #              min = 0) %>% 
        #     hc_series(
        #         list(
        #             name = "Product 1",
        #             data = data %>% filter(prod_group == 1) %>% .$n_sent,
        #             pointPlacement = "on",
        #             type = "area"
        #         ),
        #         list(
        #             name = "Product 2",
        #             data = data %>% filter(prod_group == 2) %>% .$n_sent,
        #             pointPlacement = "on",
        #             type = "area"
        #         )
        #     )
        hc
    })
    # ---------- END PRODUCT COMPARISON -------
})

